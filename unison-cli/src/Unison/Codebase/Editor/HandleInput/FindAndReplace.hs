module Unison.Codebase.Editor.HandleInput.FindAndReplace
  ( handleStructuredFindReplaceI,
    handleStructuredFindI,
    handleTextFindI
  )
where

import Control.Lens hiding (at)
import Control.Monad.Reader (ask)
import Control.Monad.State
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as P
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo, empty)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toVar)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.UnisonFile qualified as UF
import Unison.Util.Alphabetical qualified as Alphabetical
import Unison.Util.Pretty qualified as P
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.Var qualified as Var

handleStructuredFindReplaceI :: HQ.HashQualified Name -> Cli ()
handleStructuredFindReplaceI rule = do
  env <- ask
  uf0 <- Cli.expectLatestParsedFile
  let (prepare, uf, finish) = UF.prepareRewrite uf0
  (ppe, _ns, rules) <- lookupRewrite InvalidStructuredFindReplace prepare rule
  (dest, _) <- Cli.expectLatestFile
  #latestFile ?= (dest, True)
  let go n tm [] = if n == (0 :: Int) then Nothing else Just tm
      go n tm ((r, _) : rules) = case r tm of
        Nothing -> go n tm rules
        Just tm -> go (n + 1) tm rules
      (vs, uf0') = UF.rewrite (Set.singleton (HQ.toVar rule)) (\tm -> go 0 tm rules) uf
      uf' = (vs, finish uf0')
  #latestTypecheckedFile .= Just (Left . snd $ uf')
  let msg = "| Rewrote using: "
  let rendered = Text.pack . P.toPlain 80 $ renderRewrittenFile ppe msg uf'
  liftIO $ env.writeSource (Text.pack dest) rendered True
  Cli.respond $ OutputRewrittenFile dest vs

handleStructuredFindI :: HQ.HashQualified Name -> Cli ()
handleStructuredFindI rule = do
  Cli.Env {codebase} <- ask
  (ppe, names, rules0) <- lookupRewrite InvalidStructuredFind (\_ tm -> tm) rule
  let rules = snd <$> rules0
  let fqppe = PPED.unsuffixifiedPPE ppe
  results :: [(HQ.HashQualified Name, Referent)] <- pure $ do
    r <- Set.toList (Relation.ran $ Names.terms names)
    Just hq <- [PPE.terms fqppe r]
    fullName <- [HQ'.toName hq]
    guard (not (Name.beginsWithSegment fullName NameSegment.libSegment))
    Referent.Ref _ <- pure r
    Just shortName <- [PPE.terms (PPED.suffixifiedPPE ppe) r]
    pure (HQ'.toHQ shortName, r)
  let ok (hq, Referent.Ref (Reference.DerivedId r)) = do
        oe <- Cli.runTransaction (Codebase.getTerm codebase r)
        pure $ (hq, maybe False (\e -> any ($ e) rules) oe)
      ok (hq, _) = pure (hq, False)
  results0 <- traverse ok results
  let results = Alphabetical.sortAlphabetically [hq | (hq, True) <- results0]
  Cli.setNumberedArgs $ map SA.HashQualified results
  Cli.respond (ListStructuredFind results)

handleTextFindI :: Bool -> [String] -> Cli ()
handleTextFindI allowLib tokens = do
  Cli.Env {codebase} <- ask
  currentBranch <- Cli.getCurrentBranch0
  hqLength <- Cli.runTransaction Codebase.hashLength
  let names = Branch.toNames currentBranch
  let ppe = PPED.makePPED (PPE.hqNamer hqLength names) (PPE.suffixifyByHash names)
  let fqppe = PPED.unsuffixifiedPPE ppe
  results :: [(HQ.HashQualified Name, Referent)] <- pure $ do
    r <- Set.toList (Relation.ran $ Names.terms names)
    Just hq <- [PPE.terms fqppe r]
    fullName <- [HQ'.toName hq]
    guard (allowLib || not (Name.beginsWithSegment fullName NameSegment.libSegment))
    Referent.Ref _ <- pure r
    Just shortName <- [PPE.terms (PPED.suffixifiedPPE ppe) r]
    pure (HQ'.toHQ shortName, r)
  let ok (hq, Referent.Ref (Reference.DerivedId r)) = do
        oe <- Cli.runTransaction (Codebase.getTerm codebase r)
        pure $ (hq, maybe False containsTokens oe)
      ok (hq, _) = pure (hq, False)
  results0 <- traverse ok results
  let results = Alphabetical.sortAlphabetically [hq | (hq, True) <- results0]
  Cli.setNumberedArgs $ map SA.HashQualified results
  Cli.respond (ListTextFind allowLib results)
  where
    tokensTxt = Text.pack <$> tokens
    containsTokens tm =
      hasAll . join $ ABT.find txts tm
      where
        hasAll txts = all (\tok -> any (\haystack -> Text.isInfixOf tok haystack) txts) tokensTxt
        txts (Term.Text' haystack) = ABT.Found [haystack]
        txts (Term.Nat' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Int' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Float' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Char' haystack) = ABT.Found [Text.pack [haystack]]
        txts (Term.Match' _ cases) = ABT.Found r
          where r = join $ Pattern.foldMap' txtPattern . Term.matchPattern <$> cases
        txts _ = ABT.Continue
        txtPattern (Pattern.Text _ txt) = [txt]
        txtPattern _ = []

lookupRewrite ::
  (HQ.HashQualified Name -> Output) ->
  ([Symbol] -> Term Symbol Ann -> Term Symbol Ann) ->
  HQ.HashQualified Name ->
  Cli (PPED.PrettyPrintEnvDecl, Names, [(Term Symbol Ann -> Maybe (Term Symbol Ann), Term Symbol Ann -> Bool)])
lookupRewrite onErr prepare rule = do
  Cli.Env {codebase} <- ask
  currentBranch <- Cli.getCurrentBranch0
  hqLength <- Cli.runTransaction Codebase.hashLength
  fileNames <- Cli.getNamesFromLatestFile
  let currentNames = fileNames <> Branch.toNames currentBranch
  let ppe = PPED.makePPED (PPE.hqNamer hqLength currentNames) (PPE.suffixifyByHash currentNames)
  ot <- Cli.getTermFromLatestParsedFile rule
  ot <- case ot of
    Just _ -> pure ot
    Nothing -> do
      case Names.lookupHQTerm Names.IncludeSuffixes rule currentNames of
        s
          | Set.size s == 1,
            Referent.Ref (Reference.DerivedId r) <- Set.findMin s ->
              Cli.runTransaction (Codebase.getTerm codebase r)
        s -> Cli.returnEarly (TermAmbiguous (PPE.suffixifiedPPE ppe) rule s)
  tm <- maybe (Cli.returnEarly (TermAmbiguous (PPE.suffixifiedPPE ppe) rule mempty)) pure ot
  let extract vs tm = case tm of
        Term.Ann' tm _typ -> extract vs tm
        (DD.RewriteTerm' lhs rhs) ->
          pure
            ( ABT.rewriteExpression lhs rhs,
              ABT.containsExpression lhs
            )
        (DD.RewriteCase' lhs rhs) ->
          pure
            ( Term.rewriteCasesLHS lhs rhs,
              fromMaybe False . Term.containsCaseTerm lhs
            )
        (DD.RewriteSignature' _vs lhs rhs) ->
          pure (Term.rewriteSignatures lhs rhs, Term.containsSignature lhs)
        _ -> Cli.returnEarly (onErr rule)
      extractOuter vs0 tm = case tm of
        Term.Ann' tm _typ -> extractOuter vs0 tm
        Term.LamsNamed' vs tm -> extractOuter (vs0 ++ vs) tm
        tm -> case prepare vs0 tm of
          DD.Rewrites' rules -> traverse (extract vs0) rules
          _ -> Cli.returnEarly (onErr rule)
  rules <- extractOuter [] tm
  pure (ppe, currentNames, rules)

renderRewrittenFile :: (Ord a, Var v) => PPED.PrettyPrintEnvDecl -> String -> ([v], UF.UnisonFile v a) -> P.Pretty P.ColorText
renderRewrittenFile ppe msg (vs, uf) = do
  let prettyVar = P.text . Var.name
      modifiedDefs = P.sep " " (P.blue . prettyVar <$> vs)
      header = "-- " <> P.string msg <> "\n" <> "-- | Modified definition(s): " <> modifiedDefs
   in (header <> "\n\n" <> P.prettyUnisonFile ppe uf)
