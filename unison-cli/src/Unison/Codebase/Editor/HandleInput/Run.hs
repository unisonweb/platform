module Unison.Codebase.Editor.HandleInput.Run
  ( handleRun,
  )
where

import Control.Lens ((.=), _1)
import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.Load (EvalMode (Native, Permissive), evalUnisonFile)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TypeLookup
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Defns (Defns (..))
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Recursion
import Unison.Var qualified as Var

handleRun :: Bool -> HQ.HashQualified Name -> [String] -> Cli ()
handleRun native main args = do
  (unisonFile, mainResType) <- do
    (sym, term, typ, otyp) <- getTerm main
    uf <- createWatcherFile sym term typ
    pure (uf, otyp)
  names <- Cli.currentNames
  let namesWithFileDefinitions = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
  let pped = PPED.makePPED (PPE.hqNamer 10 namesWithFileDefinitions) (PPE.suffixifyByHash namesWithFileDefinitions)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  let mode | native = Native | otherwise = Permissive
  (_, xs) <-
    evalUnisonFile mode suffixifiedPPE unisonFile args & onLeftM \err ->
      Cli.returnEarly (Output.EvaluationFailure err)
  mainRes :: Term Symbol () <-
    case lookup magicMainWatcherString (map bonk (Map.toList xs)) of
      Nothing ->
        error
          ( "impossible: we manually added the watcher "
              <> show magicMainWatcherString
              <> " with 'createWatcherFile', but it isn't here."
          )
      Just x -> pure (stripUnisonFileReferences unisonFile x)
  #lastRunResult .= Just (Term.amap (\() -> External) mainRes, mainResType, unisonFile)
  Cli.respond (Output.RunResult suffixifiedPPE mainRes)
  where
    bonk (_, (_ann, watchKind, _id, _term0, term1, _isCacheHit)) =
      (watchKind, term1)

data GetTermResult
  = NoTermWithThatName
  | TermHasBadType (Type Symbol Ann)
  | GetTermSuccess (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)

-- | Look up runnable term with the given name in the codebase or
-- latest typechecked unison file. Return its symbol, term, type, and
-- the type of the evaluated term.
getTerm :: HQ.HashQualified Name -> Cli (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)
getTerm main =
  getTerm' main >>= \case
    NoTermWithThatName -> do
      mainType <- Runtime.mainType <$> view #runtime
      names <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      Cli.returnEarly $ Output.NoMainFunction main suffixifiedPPE [mainType]
    TermHasBadType ty -> do
      mainType <- Runtime.mainType <$> view #runtime
      names <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      Cli.returnEarly $ Output.BadMainFunction "run" main ty suffixifiedPPE [mainType]
    GetTermSuccess x -> pure x

getTerm' :: HQ.HashQualified Name -> Cli GetTermResult
getTerm' mainName =
  let getFromCodebase = do
        Cli.Env {codebase, runtime} <- ask
        names <- Cli.currentNames
        let loadTypeOfTerm ref = Cli.runTransaction (Codebase.getTypeOfTerm codebase ref)
        mainToFile
          =<< MainTerm.getMainTerm loadTypeOfTerm names mainName (Runtime.mainType runtime)
        where
          mainToFile (MainTerm.NotFound _) = pure NoTermWithThatName
          mainToFile (MainTerm.BadType _ ty) = pure $ maybe NoTermWithThatName TermHasBadType ty
          mainToFile (MainTerm.Success hq tm typ) =
            let v = Var.named (HQ.toText hq)
             in checkType Nothing typ \otyp ->
                  pure (GetTermSuccess (v, tm, typ, otyp))
      getFromFile uf = do
        let components = join $ UF.topLevelComponents uf
        -- __TODO__: We shouldn’t need to serialize mainName` for this check
        let mainComponent = filter ((\v -> Var.name v == HQ.toText mainName) . view _1) components
        case mainComponent of
          [(v, _, tm, ty)] ->
            checkType (Just uf) ty \otyp ->
              let runMain = DD.forceTerm a a (Term.var a v)
                  v2 = Var.freshIn (Set.fromList [v]) v
                  a = ABT.annotation tm
               in pure (GetTermSuccess (v2, runMain, ty, otyp))
          _ -> getFromCodebase
      checkType :: Maybe (TypecheckedUnisonFile Symbol Ann) -> Type Symbol Ann -> (Type Symbol Ann -> Cli GetTermResult) -> Cli GetTermResult
      checkType mayTuf ty f = do
        Cli.Env {codebase, runtime} <- ask
        case Typechecker.fitsScheme ty (Runtime.mainType runtime) of
          True -> do
            tlCodebase <-
              Cli.runTransaction $
                Codebase.typeLookupForDependencies codebase Defns {terms = Set.empty, types = Type.dependencies ty}
            let tlTuf = Monoid.fromMaybe (fmap UF.typecheckedToTypeLookup mayTuf)
            f $! synthesizeForce (tlTuf <> tlCodebase) ty
          False -> pure (TermHasBadType ty)
   in Cli.getLatestTypecheckedFile >>= \case
        Nothing -> getFromCodebase
        Just uf -> getFromFile uf

-- | Produce a typechecked unison file where the given term is the
-- only watcher, with the watch type set to 'magicMainWatcherString'.
createWatcherFile :: Symbol -> Term Symbol Ann -> Type Symbol Ann -> Cli (TypecheckedUnisonFile Symbol Ann)
createWatcherFile v tm typ =
  Cli.getLatestTypecheckedFile >>= \case
    Nothing -> pure (UF.typecheckedUnisonFile mempty mempty mempty [(magicMainWatcherString, [(v, External, tm, typ)])])
    Just uf ->
      let v2 = Var.freshIn (Set.fromList [v]) v
       in pure $
            UF.typecheckedUnisonFile
              (UF.dataDeclarationsId' uf)
              (UF.effectDeclarationsId' uf)
              (UF.topLevelComponents' uf)
              -- what about main's component? we have dropped them if they existed.
              [(magicMainWatcherString, [(v2, External, tm, typ)])]

-- | synthesize the type of forcing a term
--
-- precondition: @fitsScheme typeOfFunc Runtime.mainType@ is satisfied
synthesizeForce :: TypeLookup Symbol Ann -> Type Symbol Ann -> Type Symbol Ann
synthesizeForce tl typeOfFunc = do
  let term :: Term Symbol Ann
      term = Term.ref External ref
      ref = Reference.DerivedId (Reference.Id (Hash.fromByteString "deadbeef") 0)
      env =
        Typechecker.Env
          { ambientAbilities = [DD.exceptionType External, Type.builtinIO External],
            typeLookup = mempty {TypeLookup.typeOfTerms = Map.singleton ref typeOfFunc} <> tl,
            termsByShortname = Map.empty,
            freeNameToFuzzyTermsByShortName = Map.empty,
            topLevelComponents = Map.empty
          }
  case Result.runResultT
    ( Typechecker.synthesize
        PPE.empty
        Typechecker.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
        env
        (DD.forceTerm External External term)
    ) of
    Identity (Nothing, notes) ->
      error
        ( unlines
            [ "synthesizeForce fails although fitsScheme passed",
              "Input Type:",
              show typeOfFunc,
              "Notes:",
              show notes
            ]
        )
    Identity (Just typ, _) -> typ

-- Hack alert
--
-- After we evaluate a term all vars are transformed into references,
-- but we want to feed this result into 'slurpFile' which won't add
-- dependencies that are referenced by hash. The hacky solution for
-- now is to convert all references that match a variable defined
-- within the unison file to variable references. This is hacky both
-- because we needlessly flip-flopping between var and reference
-- representations, and because we might unexpectedly add a term from
-- the local file if it has the same hash as a term in the codebase.
stripUnisonFileReferences :: TypecheckedUnisonFile Symbol a -> Term Symbol () -> Term Symbol ()
stripUnisonFileReferences unisonFile term =
  let refMap :: Map Reference.Id Symbol
      refMap = Map.fromList . map (\(sym, (_, refId, _, _, _)) -> (refId, sym)) . Map.toList . UF.hashTermsId $ unisonFile
      alg (ABT.Term' _ () abt) = case abt of
        ABT.Var x -> ABT.var x
        ABT.Cycle x -> ABT.cycle x
        ABT.Abs v x -> ABT.abs v x
        ABT.Tm t -> case t of
          Term.Ref ref
            | Just var <- (\k -> Map.lookup k refMap) =<< Reference.toId ref -> ABT.var var
          x -> ABT.tm x
   in cata alg term

magicMainWatcherString :: String
magicMainWatcherString = "main"
