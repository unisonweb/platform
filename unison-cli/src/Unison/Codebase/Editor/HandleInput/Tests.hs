module Unison.Codebase.Editor.HandleInput.Tests
  ( handleTest,
    handleIOTest,
    handleAllIOTests,
    isTestOk,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.RuntimeUtils (EvalMode (..))
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.Input (TestInput (..))
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Runtime
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TermReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation qualified as R
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as WK

-- | Handle a @test@ command.
-- Run pure tests in the current subnamespace.
handleTest :: Bool -> TestInput -> Cli ()
handleTest native TestInput {includeLibNamespace, path, showFailures, showSuccesses} = do
  Cli.Env {codebase} <- ask

  testRefs <- findTermsOfTypes codebase includeLibNamespace path (NESet.singleton (DD.testResultListType mempty))

  cachedTests <-
    Map.fromList <$> Cli.runTransaction do
      Set.toList testRefs & wither \case
        rid -> fmap (rid,) <$> Codebase.getWatch codebase WK.TestWatch rid
  let (fails, oks) = passFails cachedTests
      passFails :: (Ord r) => Map r (Term v a) -> (Map r [Text], Map r [Text])
      passFails =
        Map.foldrWithKey
          (\r v (f, o) -> bimap (\ts -> if null ts then f else Map.insert r ts f) (\ts -> if null ts then o else Map.insert r ts o) . partitionEithers $ p v)
          (Map.empty, Map.empty)
        where
          p :: Term v a -> [Either Text Text]
          p = \case
            Term.List' ts -> mapMaybe q $ toList ts
            _ -> []
          q = \case
            Term.App' (Term.Constructor' (ConstructorReference ref cid)) (Term.Text' msg) ->
              if
                | ref == DD.testResultRef ->
                    if
                      | cid == DD.okConstructorId -> Just (Right msg)
                      | cid == DD.failConstructorId -> Just (Left msg)
                      | otherwise -> Nothing
                | otherwise -> Nothing
            _ -> Nothing
  let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let fqnPPE = PPED.unsuffixifiedPPE pped
  Cli.respondNumbered $
    TestResults
      stats
      fqnPPE
      showSuccesses
      showFailures
      oks
      fails
  let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
  when (not (Set.null toCompute)) do
    let total = Set.size toCompute
    computedTests <- fmap join . for (toList toCompute `zip` [1 ..]) $ \(r, n) ->
      Cli.runTransaction (Codebase.getTerm codebase r) >>= \case
        Nothing -> do
          hqLength <- Cli.runTransaction Codebase.hashLength
          Cli.respond (TermNotFound' . SH.shortenTo hqLength . Reference.toShortHash $ Reference.DerivedId r)
          pure []
        Just tm -> do
          Cli.respond $ TestIncrementalOutputStart fqnPPE (n, total) r
          --                        v don't cache; test cache populated below
          tm' <- RuntimeUtils.evalPureUnison native fqnPPE False tm
          case tm' of
            Left e -> do
              Cli.respond (EvaluationFailure e)
              pure []
            Right tm' -> do
              -- After evaluation, cache the result of the test
              Cli.runTransaction (Codebase.putWatch WK.TestWatch r tm')
              Cli.respond $ TestIncrementalOutputEnd fqnPPE (n, total) r (isTestOk tm')
              pure [(r, tm')]

    let m = Map.fromList computedTests
        (mFails, mOks) = passFails m
    Cli.respondNumbered $ TestResults Output.NewlyComputed fqnPPE showSuccesses showFailures mOks mFails

handleIOTest :: Bool -> HQ.HashQualified Name -> Cli ()
handleIOTest native main = do
  let mode = if native then Native else Permissive
  runtime <- RuntimeUtils.selectRuntime mode
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  let isIOTest typ = Foldable.any (Typechecker.isSubtype typ) $ Runtime.ioTestTypes runtime
  refs <- resolveHQNames names (Set.singleton main)
  (fails, oks) <-
    Foldable.foldrM
      ( \(ref, typ) (f, o) -> do
          when (not $ isIOTest typ) $
            Cli.returnEarly (BadMainFunction "io.test" main typ suffixifiedPPE (Foldable.toList $ Runtime.ioTestTypes runtime))
          bimap (\ts -> if null ts then f else Map.insert ref ts f) (\ts -> if null ts then o else Map.insert ref ts o) <$> runIOTest suffixifiedPPE ref
      )
      (Map.empty, Map.empty)
      refs
  Cli.respondNumbered $ TestResults Output.NewlyComputed suffixifiedPPE True True oks fails

findTermsOfTypes :: Codebase.Codebase m Symbol Ann -> Bool -> Path -> NESet (Type.Type Symbol Ann) -> Cli (Set TermReferenceId)
findTermsOfTypes codebase includeLib path filterTypes = do
  branch <- Cli.expectBranch0AtPath path

  let possibleTests =
        branch
          & (if includeLib then id else Branch.withoutLib)
          & Branch.deepTerms
          & R.dom
          & Set.mapMaybe Referent.toTermReferenceId
  Cli.runTransaction do
    filterTypes & foldMapM \matchTyp -> do
      Codebase.filterTermsByReferenceIdHavingType codebase matchTyp possibleTests

handleAllIOTests :: Bool -> Cli ()
handleAllIOTests native = do
  Cli.Env {codebase} <- ask
  let mode = if native then Native else Permissive
  runtime <- RuntimeUtils.selectRuntime mode
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  ioTestRefs <- findTermsOfTypes codebase False Path.empty (Runtime.ioTestTypes runtime)
  case NESet.nonEmptySet ioTestRefs of
    Nothing -> Cli.respondNumbered $ TestResults Output.NewlyComputed suffixifiedPPE True True Map.empty Map.empty
    Just neTestRefs -> do
      let total = NESet.size neTestRefs
      (fails, oks) <-
        toList neTestRefs
          & zip [1 :: Int ..]
          & Foldable.foldrM
            ( \(n, r) (f, o) -> do
                Cli.respond $ TestIncrementalOutputStart suffixifiedPPE (n, total) r
                (fails, oks) <- runIOTest suffixifiedPPE r
                Cli.respond $ TestIncrementalOutputEnd suffixifiedPPE (n, total) r (null fails)
                pure (if null fails then f else Map.insert r fails f, if null oks then o else Map.insert r oks o)
            )
            (Map.empty, Map.empty)
      Cli.respondNumbered $ TestResults Output.NewlyComputed suffixifiedPPE True True oks fails

resolveHQNames :: Names -> Set (HQ.HashQualified Name) -> Cli (Set (Reference.Id, Type.Type Symbol Ann))
resolveHQNames parseNames hqNames =
  Set.fromList <$> do
    (Set.toList hqNames) & foldMapM \main -> do
      fmap maybeToList . runMaybeT $ do
        getNameFromScratchFile main <|> getNameFromCodebase parseNames main
  where
    getNameFromScratchFile :: HQ.HashQualified Name -> MaybeT Cli (Reference.Id, Type.Type Symbol Ann)
    getNameFromScratchFile main = do
      typecheckedFile <- MaybeT Cli.getLatestTypecheckedFile
      mainName <- hoistMaybe $ Name.parseText (HQ.toText main)
      (_, ref, _wk, _term, typ) <- hoistMaybe $ Map.lookup (Name.toVar mainName) (UF.hashTermsId typecheckedFile)
      pure (ref, typ)

    getNameFromCodebase :: Names -> HQ.HashQualified Name -> MaybeT Cli (Reference.Id, Type.Type Symbol Ann)
    getNameFromCodebase parseNames main = do
      Cli.Env {codebase} <- ask
      mapMaybeT Cli.runTransaction do
        (Set.toList (Names.lookupHQTerm Names.IncludeSuffixes main parseNames)) & altMap \ref0 -> do
          ref <- hoistMaybe (Referent.toTermReferenceId ref0)
          typ <- MaybeT (Codebase.getTypeOfReferent codebase (Referent.fromTermReferenceId ref))
          pure (ref, typ)

runIOTest :: PPE.PrettyPrintEnv -> Reference.Id -> Cli ([Text], [Text])
runIOTest ppe ref = do
  let a = ABT.annotation tm
      tm = DD.forceTerm a a (Term.refId a ref)
  -- Don't cache IO tests
  tm' <- RuntimeUtils.evalUnisonTerm Permissive ppe False tm
  pure $ partitionTestResults tm'

partitionTestResults :: Term Symbol Ann -> ([Text {- fails -}], [Text {- oks -}])
partitionTestResults tm = fold $ do
  element <- case tm of
    Term.List' ts -> toList ts
    _ -> empty
  case element of
    Term.App' (Term.Constructor' (ConstructorReference conRef cid)) (Term.Text' msg) -> do
      guard (conRef == DD.testResultRef)
      if
        | cid == DD.okConstructorId -> pure (mempty, [msg])
        | cid == DD.failConstructorId -> pure ([msg], mempty)
        | otherwise -> empty
    _ -> empty

isTestOk :: Term v Ann -> Bool
isTestOk tm = case tm of
  Term.List' ts -> all isSuccess ts
    where
      isSuccess (Term.App' (Term.Constructor' (ConstructorReference ref cid)) _) =
        cid == DD.okConstructorId
          && ref == DD.testResultRef
      isSuccess _ = False
  _ -> False
