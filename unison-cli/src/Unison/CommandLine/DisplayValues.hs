module Unison.CommandLine.DisplayValues where

import Data.Map qualified as Map
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Builtin.Decls qualified as DD
import Unison.Codebase.Editor.DisplayObject qualified as DO
import Unison.CommandLine.OutputMessages qualified as OutputMessages
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Runtime.IOSource qualified as DD
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DP
import Unison.Syntax.NamePrinter qualified as NP
import Unison.Syntax.TermPrinter qualified as TP
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Util.Pretty qualified as P
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)

type Pretty = P.Pretty P.ColorText

displayTerm ::
  (Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term Symbol ()))) ->
  (Referent -> m (Maybe (Type Symbol ()))) ->
  (Term Symbol () -> m (Maybe (Term Symbol ()))) ->
  (Reference -> m (Maybe (DD.Decl Symbol ()))) ->
  Term Symbol () ->
  m Pretty
displayTerm = displayTerm' False

-- Whether to elide printing of `()` at the end of a block
-- For instance, in:
--
--   id x = x
--   ()
--
-- We could render it as above, with the `()` explicit, or just as:
--
--   id x = x
--
type ElideUnit = Bool

displayTerm' ::
  (Monad m) =>
  ElideUnit ->
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term Symbol ()))) ->
  (Referent -> m (Maybe (Type Symbol ()))) ->
  (Term Symbol () -> m (Maybe (Term Symbol ()))) ->
  (Reference -> m (Maybe (DD.Decl Symbol ()))) ->
  Term Symbol () ->
  m Pretty
displayTerm' elideUnit pped terms typeOf eval types = \case
  tm@(Term.Apps' (Term.Constructor' (ConstructorReference typ _)) _)
    | typ == DD.docRef -> displayDoc pped terms typeOf eval types tm
    | typ == DD.doc2Ref -> do
        -- Pretty.get (doc.formatConsole tm)
        let tm' =
              Term.app
                ()
                (Term.ref () DD.prettyGetRef)
                (Term.app () (Term.ref () DD.doc2FormatConsoleRef) tm)
        tm <- eval tm'
        case tm of
          Nothing -> pure $ errMsg tm'
          Just tm -> displayTerm pped terms typeOf eval types tm
    | typ == DD.prettyAnnotatedRef -> displayPretty pped terms typeOf eval types tm
  tm@(Term.Constructor' (ConstructorReference typ _))
    | typ == DD.prettyAnnotatedRef -> displayPretty pped terms typeOf eval types tm
  tm -> pure $ src tm
  where
    errMsg tm =
      P.fatalCallout $
        P.lines
          [ P.wrap $
              "I couldn't render this document, because the"
                <> "rendering function produced an error when"
                <> "evaluating this expression:",
            "",
            P.indentN 2 $ src tm,
            "",
            P.wrap $
              "Sadly, I don't know the error, but you can evaluate"
                <> "the above expression in a scratch file to see it."
          ]
    src tm = TP.prettyBlock elideUnit (PPE.suffixifiedPPE pped) tm

-- assume this is given a
-- Pretty.Annotated ann (Either SpecialForm ConsoleText)
displayPretty ::
  forall m.
  (Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term Symbol ()))) ->
  (Referent -> m (Maybe (Type Symbol ()))) ->
  (Term Symbol () -> m (Maybe (Term Symbol ()))) ->
  (Reference -> m (Maybe (DD.Decl Symbol ()))) ->
  Term Symbol () ->
  m Pretty
displayPretty pped terms typeOf eval types tm = go tm
  where
    go = \case
      DD.PrettyEmpty -> pure mempty
      DD.PrettyGroup _ p -> P.group <$> go p
      DD.PrettyLit _ (DD.EitherLeft' special) -> goSpecial special
      DD.PrettyLit _ (DD.EitherRight' consoleTxt) -> goConsole consoleTxt
      DD.PrettyWrap _ p -> P.wrap' (pure . P.lit) <$> go p
      DD.PrettyOrElse _ p1 p2 -> P.orElse <$> go p1 <*> go p2
      DD.PrettyIndent _ initial afterNl p -> do
        initial <- go initial
        afterNl <- go afterNl
        p <- go p
        pure $ initial <> P.indentAfterNewline afterNl p
      DD.PrettyAppend _ ps -> mconcat . toList <$> traverse go ps
      DD.PrettyTable _ rows ->
        P.group . P.table <$> traverse goRow (toList rows)
        where
          goRow (Term.List' row) = traverse go (toList row)
          goRow _ = pure []
      tm -> displayTerm pped terms typeOf eval types tm

    goSrc es = do
      -- we ignore the annotations; but this could be extended later
      -- to do some ascii art rendering
      let tys = [ref | DD.TupleTerm' [DD.EitherLeft' (Term.TypeLink' ref), _anns] <- toList es]
          toRef (Term.Ref' r) = Just r
          toRef (Term.RequestOrCtor' r) = Just (r ^. ConstructorReference.reference_)
          toRef _ = Nothing
          tms = [ref | DD.TupleTerm' [DD.EitherRight' (DD.Doc2Term (toRef -> Just ref)), _anns] <- toList es]
      typeMap <-
        let -- todo: populate the variable names / kind once BuiltinObject supports that
            go ref@(Reference.Builtin _) = pure (ref, DO.BuiltinObject ())
            go ref =
              (ref,) <$> do
                decl <- types ref
                let missing = DO.MissingObject (Reference.toShortHash ref)
                pure $ maybe missing DO.UserObject decl
         in Map.fromList <$> traverse go tys
      termMap <-
        let go ref =
              (ref,) <$> case ref of
                Reference.Builtin _ -> pure $ Builtin.typeOf missing DO.BuiltinObject ref
                _ -> maybe missing DO.UserObject <$> terms ref
              where
                missing = DO.MissingObject (Reference.toShortHash ref)
         in Map.fromList <$> traverse go tms
      -- in docs, we use suffixed names everywhere
      let pped' = pped {PPE.unsuffixifiedPPE = PPE.suffixifiedPPE pped}
      pure . P.group . P.indentN 4 $ OutputMessages.displayDefinitions' pped' typeMap termMap

    goSpecial = \case
      DD.Doc2SpecialFormFoldedSource (Term.List' es) -> goSrc es
      -- Source [Either Link.Type Doc2.Term]
      DD.Doc2SpecialFormSource (Term.List' es) -> goSrc es
      -- Example Nat Doc2.Term
      -- Examples like `foo x y` are encoded as `Example 2 (_ x y -> foo)`, where
      -- 2 is the number of variables that should be dropped from the rendering.
      -- So this will render as `foo x y`.
      DD.Doc2SpecialFormExample n (DD.Doc2Example vs body) ->
        P.backticked <$> displayTerm pped terms typeOf eval types ex
        where
          ex = Term.lamWithoutBindingAnns (ABT.annotation body) (drop (fromIntegral n) vs) body
      DD.Doc2SpecialFormExampleBlock n (DD.Doc2Example vs body) ->
        -- todo: maybe do something with `vs` to indicate the variables are free
        P.indentN 4 <$> displayTerm' True pped terms typeOf eval types ex
        where
          ex = Term.lamWithoutBindingAnns (ABT.annotation body) (drop (fromIntegral n) vs) body

      -- Link (Either Link.Type Doc2.Term)
      DD.Doc2SpecialFormLink e ->
        let ppe = PPE.suffixifiedPPE pped
            go = pure . P.underline . P.syntaxToColor . NP.prettyHashQualified
         in case e of
              DD.EitherLeft' (Term.TypeLink' ref) -> go $ PPE.typeName ppe ref
              -- Eta-reduce the term, as the compiler may have eta-expanded it.
              DD.EitherRight' (DD.Doc2Term t) -> case Term.etaNormalForm t of
                Term.Ref' ref -> go $ PPE.termName ppe (Referent.Ref ref)
                Term.Request' ref ->
                  go $ PPE.termName ppe (Referent.Con ref CT.Effect)
                Term.Constructor' ref ->
                  go $ PPE.termName ppe (Referent.Con ref CT.Data)
                _ -> P.red <$> displayTerm pped terms typeOf eval types t
              _ -> P.red <$> displayTerm pped terms typeOf eval types e
      -- Signature [Doc2.Term]
      DD.Doc2SpecialFormSignature (Term.List' tms) ->
        let referents = [r | DD.Doc2Term (toReferent -> Just r) <- toList tms]
            go r = P.indentN 4 <$> goSignature r
         in P.group . P.sep "\n\n" <$> traverse go referents
      -- SignatureInline Doc2.Term
      DD.Doc2SpecialFormSignatureInline (DD.Doc2Term tm) ->
        P.backticked <$> case toReferent tm of
          Just r -> goSignature r
          _ -> displayTerm pped terms typeOf eval types tm
      -- Eval Doc2.Term
      DD.Doc2SpecialFormEval (DD.Doc2Term tm) ->
        eval tm >>= \case
          Nothing -> do
            p <- displayTerm pped terms typeOf eval types tm
            pure . P.indentN 4 $ P.lines [p, "⧨", P.red "🆘  An error occured during evaluation"]
          Just result -> do
            p1 <- displayTerm pped terms typeOf eval types tm
            p2 <- displayTerm pped terms typeOf eval types result
            pure . P.indentN 4 $ P.lines [p1, "⧨", P.green p2]

      -- EvalInline Doc2.Term
      DD.Doc2SpecialFormEvalInline (DD.Doc2Term tm) ->
        eval tm >>= \case
          Nothing -> pure . P.backticked . P.red $ "🆘  An error occurred during evaluation"
          Just result -> P.backticked <$> displayTerm pped terms typeOf eval types result
      -- Embed Any
      DD.Doc2SpecialFormEmbed (Term.App' _ any) ->
        displayTerm pped terms typeOf eval types any <&> \p ->
          P.indentN 2 $ "\n" <> "{{ embed {{" <> p <> "}} }}" <> "\n"
      -- EmbedInline Any
      DD.Doc2SpecialFormEmbedInline any ->
        displayTerm pped terms typeOf eval types any <&> \p ->
          "{{ embed {{" <> p <> "}} }}"
      tm -> P.red <$> displayTerm pped terms typeOf eval types tm

    toReferent tm = case tm of
      Term.Ref' r -> Just (Referent.Ref r)
      Term.Constructor' r -> Just (Referent.Con r CT.Data)
      Term.Request' r -> Just (Referent.Con r CT.Effect)
      _ -> Nothing

    goSignature r =
      typeOf r >>= \case
        Nothing -> pure $ termName (PPE.suffixifiedPPE pped) r
        Just typ ->
          pure . P.group $
            TypePrinter.prettySignaturesCTCollapsed
              (PPE.suffixifiedPPE pped)
              [(r, PPE.termName (PPE.suffixifiedPPE pped) r, typ)]

    goColor c = case c of
      DD.AnsiColorBlack -> P.black
      DD.AnsiColorRed -> P.red
      DD.AnsiColorGreen -> P.green
      DD.AnsiColorYellow -> P.yellow
      DD.AnsiColorBlue -> P.blue
      DD.AnsiColorMagenta -> P.purple
      DD.AnsiColorCyan -> P.cyan
      DD.AnsiColorWhite -> P.white
      DD.AnsiColorBrightBlack -> P.hiBlack
      DD.AnsiColorBrightRed -> P.hiRed
      DD.AnsiColorBrightGreen -> P.hiGreen
      DD.AnsiColorBrightYellow -> P.hiYellow
      DD.AnsiColorBrightBlue -> P.hiBlue
      DD.AnsiColorBrightMagenta -> P.hiPurple
      DD.AnsiColorBrightCyan -> P.hiCyan
      DD.AnsiColorBrightWhite -> P.hiWhite
      _ -> id

    goConsole = \case
      DD.ConsoleTextPlain (Term.Text' txt) -> pure $ P.text txt
      DD.ConsoleTextForeground color txt -> goColor color <$> goConsole txt
      DD.ConsoleTextBackground color txt -> do
        txt <- goConsole txt
        color <- pure $ goColor color
        pure $ P.background color txt
      DD.ConsoleTextBold txt -> P.bold <$> goConsole txt
      DD.ConsoleTextUnderline txt -> P.underline <$> goConsole txt
      DD.ConsoleTextInvert txt -> P.invert <$> goConsole txt
      tm -> displayTerm pped terms typeOf eval types tm

-- pattern DocBlob txt <- Term.App' (Term.Constructor' DocRef DocBlobId) (Term.Text' txt)

displayDoc ::
  forall v m.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term v ()))) ->
  (Referent -> m (Maybe (Type v ()))) ->
  (Term v () -> m (Maybe (Term v ()))) ->
  (Reference -> m (Maybe (DD.Decl v ()))) ->
  Term v () ->
  m Pretty
displayDoc pped terms typeOf evaluated types = go
  where
    go (DD.DocJoin docs) = fold <$> traverse go docs
    go (DD.DocBlob txt) = pure $ P.paragraphyText txt
    go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) =
      pure $ P.underline (termName (PPE.suffixifiedPPE pped) r)
    go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) =
      pure $ P.underline (typeName (PPE.suffixifiedPPE pped) r)
    go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = prettyTerm terms r
    go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = prettyType r
    go (DD.DocSignature (Term.TermLink' r)) = prettySignature r
    go (DD.DocEvaluate (Term.TermLink' r)) = prettyEval (evaluated . Term.ref ()) r
    go tm = pure $ TP.pretty (PPE.suffixifiedPPE pped) tm
    prettySignature r =
      typeOf r >>= \case
        Nothing -> pure $ termName (PPE.unsuffixifiedPPE pped) r
        Just typ ->
          pure . P.group $
            TypePrinter.prettySignaturesCTCollapsed
              (PPE.suffixifiedPPE pped)
              [(r, PPE.termName (PPE.unsuffixifiedPPE pped) r, typ)]
    prettyEval terms r = case r of
      Referent.Ref (Reference.Builtin n) -> pure . P.syntaxToColor $ P.text n
      Referent.Ref ref ->
        let ppe = PPE.declarationPPE pped ref
         in terms ref >>= \case
              Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
              Just tm -> pure $ TP.pretty ppe tm
      Referent.Con (ConstructorReference r _) _ -> pure $ typeName (PPE.declarationPPE pped r) r
    prettyTerm terms r = case r of
      Referent.Ref (Reference.Builtin _) -> prettySignature r
      Referent.Ref ref ->
        let ppe = PPE.declarationPPE pped ref
         in terms ref >>= \case
              Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
              Just tm -> pure . P.syntaxToColor . P.group $ TP.prettyBinding ppe (PPE.termName ppe r) tm
      Referent.Con (ConstructorReference r _) _ -> prettyType r
    prettyType r =
      let ppe = PPE.declarationPPE pped r
       in types r >>= \case
            Nothing -> pure $ "😶  Missing type source for: " <> typeName ppe r
            Just ty ->
              pure . P.syntaxToColor $
                P.group $
                  DP.prettyDecl
                    pped
                    DP.RenderUniqueTypeGuids'No
                    r
                    (PPE.typeName ppe r)
                    ty

termName :: PPE.PrettyPrintEnv -> Referent -> Pretty
termName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.TermReference r) name
  where
    name = PPE.termName ppe r

typeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
typeName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.TypeReference r) name
  where
    name = PPE.typeName ppe r
