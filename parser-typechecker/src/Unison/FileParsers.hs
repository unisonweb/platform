module Unison.FileParsers
  ( ShouldUseTndr (..),
    computeTypecheckingEnvironment,
    synthesizeFile,
  )
where

import Control.Lens
import Control.Monad.State (evalStateT)
import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Blank qualified as Blank
import Unison.Builtin qualified as Builtin
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Names.ResolvesTo (ResolvesTo (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result (CompilerBug (..), Note (..), ResultT, pattern Result)
import Unison.Result qualified as Result
import Unison.Syntax.Name qualified as Name (unsafeParseVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.Extractor (RedundantTypeAnnotation)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile (definitionLocation)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.List qualified as List
import Unison.Util.Map qualified as Map (upsert)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Rel
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind)

type Term v = Term.Term v Ann

type Type v = Type.Type v Ann

type UnisonFile v = UF.UnisonFile v Ann

-- each round of TDNR emits its own TopLevelComponent notes, so we remove
-- duplicates (based on var name and location), preferring the later note as
-- that will have the latest typechecking info
convertNotes :: (Ord v) => Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes bugs es is) =
  (CompilerBug . TypecheckerBug <$> bugs) <> (TypeError <$> es) <> (TypeInfo <$> Seq.fromList is')
  where
    is' = snd <$> List.uniqueBy' f ([(1 :: Word) ..] `zip` Foldable.toList is)
    f (_, Context.TopLevelComponent cs) = Right [v | (v, _, _) <- cs]
    f (i, _) = Left i

-- | Should we use type-directed name resolution?
data ShouldUseTndr m
  = ShouldUseTndr'No
  | ShouldUseTndr'Yes (Parser.ParsingEnv m)

-- | Compute a typechecking environment, given:
--
--     * Whether or not to use type-directed name resolution during type checking.
--     * The abilities that are considered to already have ambient handlers.
--     * A function to compute a @TypeLookup@ for the given set of type- or term-references.
--     * The parsing environment that was used to parse the parsed Unison file.
--     * The parsed Unison file for which the typechecking environment is applicable.
computeTypecheckingEnvironment ::
  forall m v.
  (Var v, Monad m) =>
  ShouldUseTndr m ->
  [Type v] ->
  (DefnsF Set TermReference TypeReference -> m (TL.TypeLookup v Ann)) ->
  UnisonFile v ->
  m (Typechecker.Env v Ann)
computeTypecheckingEnvironment shouldUseTndr ambientAbilities typeLookupf uf =
  case shouldUseTndr of
    ShouldUseTndr'No -> do
      tl <- typeLookupf (UF.dependencies uf)
      pure
        Typechecker.Env
          { ambientAbilities = ambientAbilities,
            typeLookup = tl,
            termsByShortname = Map.empty,
            topLevelComponents = Map.empty
          }
    ShouldUseTndr'Yes parsingEnv -> do
      let tm = UF.typecheckingTerm uf
          resolveName :: Name -> Relation Name (ResolvesTo Referent)
          resolveName =
            Names.resolveNameIncludingNames
              (Names.shadowing1 (Names.terms (UF.toNames uf)) (Names.terms (Parser.names parsingEnv)))
              (Set.map Name.unsafeParseVar (UF.toTermAndWatchNames uf))
          possibleDeps = do
            v <- Set.toList (Term.freeVars tm)
            let shortname = Name.unsafeParseVar v
            (name, ref) <- Rel.toList (resolveName shortname)
            [(name, shortname, ref)]
          possibleRefs =
            List.foldl'
              ( \acc -> \case
                  (_, _, ResolvesToNamespace ref0) ->
                    case ref0 of
                      Referent.Con ref _ -> acc & over #types (Set.insert (ref ^. ConstructorReference.reference_))
                      Referent.Ref ref -> acc & over #terms (Set.insert ref)
                  (_, _, ResolvesToLocal _) -> acc
              )
              (Defns Set.empty Set.empty)
              possibleDeps
      tl <- fmap (UF.declsToTypeLookup uf <>) (typeLookupf (UF.dependencies uf <> possibleRefs))
      let termsByShortname :: Map Name [Either Name (Typechecker.NamedReference v Ann)]
          termsByShortname =
            List.foldl'
              ( \acc -> \case
                  (name, shortname, ResolvesToLocal _) -> let v = Left name in Map.upsert (maybe [v] (v :)) shortname acc
                  (name, shortname, ResolvesToNamespace ref) ->
                    case TL.typeOfReferent tl ref of
                      Just ty ->
                        let v = Right (Typechecker.NamedReference name ty (Context.ReplacementRef ref))
                         in Map.upsert (maybe [v] (v :)) shortname acc
                      Nothing -> acc
              )
              Map.empty
              possibleDeps
      pure
        Typechecker.Env
          { ambientAbilities,
            typeLookup = tl,
            termsByShortname,
            topLevelComponents = Map.empty
          }

synthesizeFile ::
  forall m v.
  (Monad m, Var v) =>
  Typechecker.Env v Ann ->
  UnisonFile v ->
  ResultT (Seq (Note v Ann)) m (UF.TypecheckedUnisonFile v Ann)
synthesizeFile env0 uf = do
  let term = UF.typecheckingTerm uf
      -- substitute Blanks for any remaining free vars in UF body
      tdnrTerm = Term.prepareTDNR term
      unisonFilePPE = PPE.makePPE (PPE.hqNamer 10 (Names.shadowing (UF.toNames uf) Builtin.names)) PPE.dontSuffixify
      Result notes mayType =
        evalStateT (Typechecker.synthesizeAndResolve unisonFilePPE env0) tdnrTerm
  -- If typechecking succeeded, reapply the TDNR decisions to user's term:
  Result.makeResult (convertNotes notes) mayType >>= \_typ -> do
    let infos = Foldable.toList $ Typechecker.infos notes
    (topLevelComponents :: [[(v, Term v, Type v)]]) <-
      let topLevelBindings :: Map v (Term v)
          topLevelBindings = Map.mapKeys Var.reset $ extractTopLevelBindings tdnrTerm
          extractTopLevelBindings :: (Term.Term v a -> Map v (Term.Term v a))
          extractTopLevelBindings (Term.LetRecNamedAnnotatedTop' True _ bs body) =
            Map.fromList (first snd <$> bs) <> extractTopLevelBindings body
          extractTopLevelBindings _ = Map.empty
          tlcsFromTypechecker :: [[(v, Type.Type v Ann, RedundantTypeAnnotation)]]
          tlcsFromTypechecker =
            List.uniqueBy'
              (fmap vars)
              [t | Context.TopLevelComponent t <- infos]
            where
              vars (v, _, _) = v
          addTypesToTopLevelBindings :: (v, c, c1) -> ResultT (Seq (Note v Ann)) m (v, Term v, c)
          addTypesToTopLevelBindings (v, typ, _redundant) = do
            tm <- case Map.lookup v topLevelBindings of
              Nothing -> Result.compilerBug $ Result.TopLevelComponentNotFound v term
              Just x -> pure x
            -- The Var.reset removes any freshening added during typechecking
            pure (Var.reset v, tm, typ)
       in traverse (traverse addTypesToTopLevelBindings) tlcsFromTypechecker
    let doTdnr = applyTdnrDecisions infos
    let doTdnrInComponent (v, t, tp) = (v, doTdnr t, tp)
    let tdnredTlcs =
          topLevelComponents
            & (fmap . fmap)
              ( \vtt ->
                  vtt
                    & doTdnrInComponent
                    & \(v, t, tp) -> (v, fromMaybe (error $ "Symbol from typechecked file not present in parsed file" <> show v) (definitionLocation v uf), t, tp)
              )
    let (watches', terms') = partition isWatch tdnredTlcs
        isWatch = all (\(v, _, _, _) -> Set.member v watchedVars)
        watchedVars = Set.fromList [v | (v, _a, _) <- UF.allWatches uf]
        tlcKind [] = error "empty TLC, should never occur"
        tlcKind tlc@((v, _, _, _) : _) =
          let hasE :: WatchKind -> Bool
              hasE k = elem v . fmap (view _1) $ Map.findWithDefault [] k (UF.watches uf)
           in case Foldable.find hasE (Map.keys $ UF.watches uf) of
                Nothing -> error "wat"
                Just kind -> (kind, tlc)
    pure $
      UF.typecheckedUnisonFile
        (UF.dataDeclarationsId uf)
        (UF.effectDeclarationsId uf)
        terms'
        (map tlcKind watches')
  where
    applyTdnrDecisions ::
      [Context.InfoNote v Ann] ->
      Term v ->
      Term v
    applyTdnrDecisions infos tdnrTerm = ABT.visitPure resolve tdnrTerm
      where
        decisions = Map.fromList [((Var.nameStr v, loc), replacement) | Context.Decision v loc replacement <- infos]
        -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
        resolve t = case t of
          Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
            | Just replacement <- Map.lookup (name, loc') decisions ->
                -- loc of replacement already chosen correctly by whatever made the
                -- Decision
                Just $ replacement
          _ -> Nothing
