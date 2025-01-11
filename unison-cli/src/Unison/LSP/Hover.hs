{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.Hover where

import Control.Lens hiding (List)
import Control.Monad.Reader
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.ABT qualified as ABT
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.LSP.FileAnalysis (ppedForFile)
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LSP.Util.IntersectionMap qualified as IM
import Unison.LSP.VFS qualified as VFS
import Unison.LabeledDependency qualified as LD
import Unison.Parser.Ann (Ann)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Symbol (Symbol)
import Unison.Symbol qualified as Symbol
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Util.Pretty qualified as Pretty
import Unison.Var (Var)
import Unison.Var qualified as Var
import UnliftIO qualified

-- | Hover help handler
hoverHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentHover -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond = do
  respond . Right . maybe (InR Null) InL =<< runMaybeT do
    let pos = (m ^. params . position)
    hoverTxt <- hoverInfo (m ^. params . textDocument . uri) pos
    pure $
      Hover
        { _contents = InL (MarkupContent MarkupKind_Markdown hoverTxt),
          _range = Nothing -- TODO add range info
        }

hoverInfo :: Uri -> Position -> MaybeT Lsp Text
hoverInfo uri pos =
  (hoverInfoForRef <|> hoverInfoForLiteral <|> hoverInfoForLocalVar)
  where
    markdownify :: Text -> Text
    markdownify rendered = Text.unlines ["``` unison", rendered, "```"]
    prettyWidth :: Pretty.Width
    prettyWidth = 40
    hoverInfoForRef :: MaybeT Lsp Text
    hoverInfoForRef = do
      symAtCursor <- VFS.identifierAtPosition uri pos
      ref <- LSPQ.refAtPosition uri pos
      pped <- lift $ ppedForFile uri
      let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
      let fqn = case ref of
            LD.TypeReference ref -> PPE.typeName unsuffixifiedPPE ref
            LD.TermReferent ref -> PPE.termName unsuffixifiedPPE ref

      builtinsAsync <- liftIO . UnliftIO.async $ UnliftIO.evaluate IOSource.typecheckedFile
      checkBuiltinsReady <- liftIO do
        pure
          ( UnliftIO.poll builtinsAsync
              <&> ( \case
                      Nothing -> False
                      Just (Left {}) -> False
                      Just (Right {}) -> True
                  )
          )
      renderedDocs <-
        -- We don't want to block the type signature hover info if the docs are taking a long time to render;
        -- We know it's also possible to write docs that eval forever, so the timeout helps
        -- protect against that.
        lift (UnliftIO.timeout 2_000_000 (LSPQ.markdownDocsForFQN uri fqn))
          >>= ( \case
                  Nothing ->
                    checkBuiltinsReady >>= \case
                      False -> pure ["\n---\n🔜 Doc renderer is initializing, try again in a few seconds."]
                      True -> pure ["\n---\n⏳ Timeout evaluating docs"]
                  Just [] -> pure []
                  -- Add some space from the type signature
                  Just xs@(_ : _) -> pure ("\n---\n" : xs)
              )
      typeSig <-
        case ref of
          LD.TypeReference (Reference.Builtin {}) -> do
            pure (symAtCursor <> " : <builtin>")
          LD.TypeReference ref@(Reference.DerivedId refId) -> do
            nameAtCursor <- MaybeT . pure $ Name.parseText symAtCursor
            decl <- LSPQ.getTypeDeclaration uri refId
            let typ = Text.pack . Pretty.toPlain prettyWidth . Pretty.syntaxToColor $ DeclPrinter.prettyDecl pped ref (HQ.NameOnly nameAtCursor) decl
            pure typ
          LD.TermReferent ref -> do
            typ <- LSPQ.getTypeOfReferent uri ref
            pure $ renderTypeSigForHover pped symAtCursor typ
      pure . Text.unlines $ [markdownify typeSig] <> renderedDocs

    renderTypeSigForHover :: Var v => PPED.PrettyPrintEnvDecl -> Text -> Type.Type v a -> Text
    renderTypeSigForHover pped name typ =
      let renderedType = Text.pack $ TypePrinter.prettyStr (Just prettyWidth) (PPED.suffixifiedPPE pped) typ
       in (name <> " : " <> renderedType)

    hoverInfoForLiteral :: MaybeT Lsp Text
    hoverInfoForLiteral =
      markdownify <$> do
        LSPQ.nodeAtPosition uri pos >>= \case
          LSPQ.TermNode term -> do
            typ <- hoistMaybe $ builtinTypeForTermLiterals term
            pure (": " <> typ)
          LSPQ.TypeNode {} -> empty
          LSPQ.PatternNode pat -> do
            typ <- hoistMaybe $ builtinTypeForPatternLiterals pat
            pure (": " <> typ)

    hoverInfoForLocalVar :: MaybeT Lsp Text
    hoverInfoForLocalVar = do
      let varFromNode = do
            node <- LSPQ.nodeAtPosition uri pos
            Debug.debugM Debug.Temp "node" node
            case node of
              LSPQ.TermNode (Term.Var' (Symbol.Symbol _ (Var.User v))) -> pure $ v
              LSPQ.TermNode {} -> empty
              LSPQ.TypeNode {} -> empty
              LSPQ.PatternNode _pat -> empty
      let varFromText = VFS.identifierAtPosition uri pos
      localVar <- varFromNode <|> varFromText
      Debug.debugM Debug.Temp "localVar" localVar
      FileAnalysis {localBindingTypes} <- FileAnalysis.getFileAnalysis uri
      Debug.debugM Debug.Temp "pos" pos
      Debug.debugM Debug.Temp "localBindingTypes" localBindingTypes
      (_range, typ) <- hoistMaybe $ IM.keyedSmallestIntersection localVar pos localBindingTypes
      pped <- lift $ ppedForFile uri
      pure $ renderTypeSigForHover pped localVar typ

    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure

-- | Get the type for term literals.
builtinTypeForTermLiterals :: Term.Term Symbol Ann -> Maybe Text
builtinTypeForTermLiterals term =
  case ABT.out term of
    ABT.Tm f -> case f of
      Term.Int {} -> Just "Int"
      Term.Nat {} -> Just "Nat"
      Term.Float {} -> Just "Float"
      Term.Boolean {} -> Just "Boolean"
      Term.Text {} -> Just "Text"
      Term.Char {} -> Just "Char"
      Term.Blank {} -> Nothing
      Term.Ref {} -> Nothing
      Term.Constructor {} -> Nothing
      Term.Request {} -> Nothing
      Term.Handle {} -> Nothing
      Term.App {} -> Nothing
      Term.Ann {} -> Nothing
      Term.List {} -> Nothing
      Term.If {} -> Nothing
      Term.And {} -> Nothing
      Term.Or {} -> Nothing
      Term.Lam {} -> Nothing
      Term.LetRec {} -> Nothing
      Term.Let {} -> Nothing
      Term.Match {} -> Nothing
      Term.TermLink {} -> Nothing
      Term.TypeLink {} -> Nothing
    ABT.Var {} -> Nothing
    ABT.Cycle {} -> Nothing
    ABT.Abs {} -> Nothing

builtinTypeForPatternLiterals :: Pattern.Pattern Ann -> Maybe Text
builtinTypeForPatternLiterals = \case
  Pattern.Unbound _ -> Nothing
  Pattern.Var _ -> Nothing
  Pattern.Boolean _ _ -> Just "Boolean"
  Pattern.Int _ _ -> Just "Int"
  Pattern.Nat _ _ -> Just "Nat"
  Pattern.Float _ _ -> Just "Float"
  Pattern.Text _ _ -> Just "Text"
  Pattern.Char _ _ -> Just "Char"
  Pattern.Constructor _ _ _ -> Nothing
  Pattern.As _ _ -> Nothing
  Pattern.EffectPure _ _ -> Nothing
  Pattern.EffectBind _ _ _ _ -> Nothing
  Pattern.SequenceLiteral _ _ -> Nothing
  Pattern.SequenceOp _ _ _ _ -> Nothing
