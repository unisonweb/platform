{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.Propagate
  ( propagateAndApply,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.Graph qualified as Graph
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.TypeCheck qualified as Cli (computeTypecheckingEnvironment)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.TermEdit (TermEdit (..))
import Unison.Codebase.TermEdit qualified as TermEdit
import Unison.Codebase.TermEdit.Typing qualified as TermEdit
import Unison.Codebase.TypeEdit (TypeEdit (..))
import Unison.Codebase.TypeEdit qualified as TypeEdit
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Reference (Reference, Reference' (..), TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (UnisonFile (..))
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation qualified as R
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 qualified as Star2
import Unison.Util.TransitiveClosure (transitiveClosure)
import Unison.Var (Var)
import Unison.WatchKind (WatchKind)

data Edits v = Edits
  { termEdits :: Map Reference TermEdit,
    -- same info as `termEdits` but in more efficient form for calling `Term.updateDependencies`
    termReplacements :: Map Referent Referent,
    newTerms :: Map Reference (Term v Ann, Type v Ann),
    typeEdits :: Map Reference TypeEdit,
    typeReplacements :: Map Reference Reference,
    newTypes :: Map Reference (Decl v Ann),
    constructorReplacements :: Map Referent Referent
  }
  deriving (Eq, Show)

noEdits :: Edits v
noEdits = Edits mempty mempty mempty mempty mempty mempty mempty

propagateAndApply ::
  Names ->
  Patch ->
  Branch0 IO ->
  Cli (Branch0 IO)
propagateAndApply rootNames patch branch = do
  edits <- propagate rootNames patch branch
  let f = applyPropagate patch edits
  (pure . f . applyDeprecations patch) branch

-- This function produces constructor mappings for propagated type updates.
--
-- For instance in `type Foo = Blah Bar | Zoink Nat`, if `Bar` is updated
-- from `Bar#old` to `Bar#new`, `Foo` will be a "propagated update" and
-- we want to map the `Foo#old.Blah` constructor to `Foo#new.Blah`.
--
-- The function works by aligning same-named types and same-named constructors,
-- using the names of the types provided by the two maps and the names
-- of constructors embedded in the data decls themselves.
--
-- This is correct, and relies only on the type and constructor names coming
-- out of the codebase and Decl.unhashComponent being unique, which they are.
--
-- What happens is that the declaration component is pulled out of the codebase,
-- references are converted back to variables, substitutions are made in
-- constructor type signatures, and then the component is rehashed, which
-- re-canonicalizes the constructor orders in a possibly different way.
--
-- The unique names for the types and constructors are just carried through
-- unchanged through this process, so their being the same establishes that they
-- had the same role in the two versions of the cycle.
propagateCtorMapping ::
  (Var v, Show a) =>
  Map v (Reference, Decl v a) ->
  Map v (Reference, Decl.DataDeclaration v a) ->
  Map Referent Referent
propagateCtorMapping oldComponent newComponent =
  let singletons = Map.size oldComponent == 1 && Map.size newComponent == 1
      isSingleton c = null . drop 1 $ Decl.constructors' c
      r =
        Map.fromList
          [ (oldCon, newCon)
            | (v1, (oldR, oldDecl)) <- Map.toList oldComponent,
              (v2, (newR, newDecl)) <- Map.toList newComponent,
              v1 == v2 || singletons,
              let t = Decl.constructorType oldDecl,
              (oldC, (_, ol'Name, _)) <- zip [0 ..] $ Decl.constructors' (Decl.asDataDecl oldDecl),
              (newC, (_, newName, _)) <- zip [0 ..] $ Decl.constructors' newDecl,
              ol'Name == newName || (isSingleton (Decl.asDataDecl oldDecl) && isSingleton newDecl),
              oldR /= newR,
              let oldCon = Referent.Con (ConstructorReference oldR oldC) t
                  newCon = Referent.Con (ConstructorReference newR newC) t
          ]
   in if debugMode then traceShow ("constructorMappings" :: Text, r) r else r

-- TODO: Use of this function will go away soon, once constructor mappings can be
-- added directly to the patch.
--
-- Given a set of type replacements, this creates a mapping from the constructors
-- of the old type(s) to the constructors of the new types.
--
-- Constructors for the same-unqualified-named type with a same-unqualified-name
-- constructor are mapped to each other.
--
-- If the cycle is size 1 for old and new, then the type names need not be the same,
-- and if the number of constructors is 1, then the constructor names need not
-- be the same.
genInitialCtorMapping :: Names -> Map Reference Reference -> Sqlite.Transaction (Map Referent Referent)
genInitialCtorMapping rootNames initialTypeReplacements = do
  let mappings :: (Reference, Reference) -> Sqlite.Transaction (Map Referent Referent)
      mappings (old, new) = do
        old <- unhashTypeComponent old
        new <- fmap (over _2 (either Decl.toDataDecl id)) <$> unhashTypeComponent new
        pure $ ctorMapping old new
  Map.unions <$> traverse mappings (Map.toList initialTypeReplacements)
  where
    -- True if the unqualified versions of the names in the two sets overlap
    -- ex: {foo.bar, foo.baz} matches the set {blah.bar}.
    unqualifiedNamesMatch :: Set Name.Name -> Set Name.Name -> Bool
    unqualifiedNamesMatch n1 n2 | debugMode && traceShow ("namesMatch" :: Text, n1, n2) False = undefined
    unqualifiedNamesMatch n1 n2 =
      (not . Set.null)
        ( Set.intersection
            (Set.map Name.unqualified n1)
            (Set.map Name.unqualified n2)
        )
    ctorNamesMatch oldR newR =
      unqualifiedNamesMatch
        (Names.namesForReferent rootNames oldR)
        (Names.namesForReferent rootNames newR)

    typeNamesMatch typeMapping oldType newType =
      Map.lookup oldType typeMapping == Just newType
        || unqualifiedNamesMatch
          (Names.namesForReference rootNames oldType)
          (Names.namesForReference rootNames oldType)

    ctorMapping ::
      Map v (Reference, Decl v a) ->
      Map v (Reference, Decl.DataDeclaration v a) ->
      Map Referent Referent
    ctorMapping oldComponent newComponent =
      let singletons = Map.size oldComponent == 1 && Map.size newComponent == 1
          isSingleton c = null . drop 1 $ Decl.constructors' c
          r =
            Map.fromList
              [ (oldCon, newCon)
                | (_, (oldR, oldDecl)) <- Map.toList oldComponent,
                  (_, (newR, newDecl)) <- Map.toList newComponent,
                  typeNamesMatch initialTypeReplacements oldR newR || singletons,
                  let t = Decl.constructorType oldDecl,
                  (oldC, _) <- zip [0 ..] $ Decl.constructors' (Decl.asDataDecl oldDecl),
                  (newC, _) <- zip [0 ..] $ Decl.constructors' newDecl,
                  let oldCon = Referent.Con (ConstructorReference oldR oldC) t
                      newCon = Referent.Con (ConstructorReference newR newC) t,
                  ctorNamesMatch oldCon newCon
                    || (isSingleton (Decl.asDataDecl oldDecl) && isSingleton newDecl),
                  oldR /= newR
              ]
       in if debugMode then traceShow ("constructorMappings" :: Text, r) r else r

debugMode :: Bool
debugMode = False

-- Note: this function adds definitions to the codebase as it propagates.
-- Description:
------------------
-- For any `Reference` in the frontier which has an unconflicted
-- term edit, `old -> new`, replace `old` with `new` in dependents of the
-- frontier, and call `propagate'` recursively on the new frontier if
-- the dependents still typecheck.
--
-- If the term is `Typing.Same`, the dependents don't need to be typechecked.
-- If the term is `Typing.Subtype`, and the dependent only has inferred type,
-- it should be re-typechecked, and the new inferred type should be used.
--
-- This will create a whole bunch of new terms and types in the codebase and
-- move the names onto those new terms. Uses `updateDependencies` to perform
-- the substitutions.
--
-- Algorithm:
----------------
-- compute the frontier relation (dependencies of updated terms and types)
-- for each dirty definition d:
--  for each member c of cycle(d):
--   construct c', an updated c incorporating all edits
--   Add an edit c -> c'
--     and save c' to a `Map Reference Term` or `Map Reference Type`
--     as appropriate
--   Collect all c' into a new cycle and typecheck (TODO: kindcheck) that cycle.
--     If the cycle doesn't check, discard edits to that cycle.
--
-- "dirty" means in need of update
-- "frontier" means updated definitions responsible for the "dirty"
propagate :: Names -> Patch -> Branch0 IO -> Cli (Edits Symbol)
propagate rootNames patch b = case validatePatch patch of
  Nothing -> do
    Cli.respond PatchNeedsToBeConflictFree
    pure noEdits
  Just (initialTermEdits, initialTypeEdits) -> do
    -- TODO: this can be removed once patches have term replacement of type `Referent -> Referent`
    let -- TODO: these are just used for tracing, could be deleted if we don't care
        -- about printing meaningful names for definitions during propagation, or if
        -- we want to just remove the tracing.
        refName r =
          -- could just become show r if we don't care
          let rns =
                Names.namesForReferent rootNames (Referent.Ref r)
                  <> Names.namesForReference rootNames r
           in case toList rns of
                [] -> show r
                n : _ -> show n
        -- this could also become show r if we're removing the dependency on Names
        referentName r = case toList (Names.namesForReferent rootNames r) of
          [] -> Referent.toString r
          n : _ -> show n

    Cli.Env {codebase} <- ask

    Cli.runTransaction do
      initialDirty <-
        computeDirty
          (Codebase.dependents Queries.ExcludeOwnComponent)
          patch
          -- Dirty reference predicate: does the reference have a name in this branch that isn't in the "lib" namespace?
          (Names.contains (Names.filter nameNotInLibNamespace (Branch.toNames b)))

      let initialTypeReplacements = Map.mapMaybe TypeEdit.toReference initialTypeEdits
      -- TODO: once patches can directly contain constructor replacements, this
      -- line can turn into a pure function that takes the subset of the term replacements
      -- in the patch which have a `Referent.Con` as their LHS.
      initialCtorMappings <- genInitialCtorMapping rootNames initialTypeReplacements

      order <-
        let restrictToTypes :: Set TypeReference
            restrictToTypes =
              R.dom (R.filterRan nameNotInLibNamespace (Branch.deepTypes b))
            restrictToTerms :: Set TermReference
            restrictToTerms =
              Set.mapMaybe Referent.toTermReference (R.dom (R.filterRan nameNotInLibNamespace (Branch.deepTerms b)))
         in sortDependentsGraph
              initialDirty
              (Set.union restrictToTypes restrictToTerms)

      let getOrdered :: Set Reference -> Map Int Reference
          getOrdered rs =
            Map.fromList [(i, r) | r <- toList rs, Just i <- [Map.lookup r order]]
          collectEdits ::
            Edits Symbol ->
            Set Reference ->
            Map Int Reference ->
            Sqlite.Transaction (Edits Symbol)
          collectEdits es@Edits {..} seen todo = case Map.minView todo of
            Nothing -> pure es
            Just (r, todo) -> case r of
              ReferenceBuiltin _ -> collectEdits es seen todo
              ReferenceDerived _ -> go r todo
            where
              debugCtors =
                unlines
                  [ referentName old <> " -> " <> referentName new
                    | (old, new) <- Map.toList constructorReplacements
                  ]
              go r _ | debugMode && traceShow ("Rewriting: " :: Text, refName r) False = undefined
              go _ _ | debugMode && trace ("** Constructor replacements:\n\n" <> debugCtors) False = undefined
              go r todo =
                if Map.member r termEdits || Set.member r seen || Map.member r typeEdits
                  then collectEdits es seen todo
                  else do
                    haveType <- Codebase.isType codebase r
                    haveTerm <- Codebase.isTerm codebase r
                    let message =
                          "This reference is not a term nor a type " <> show r
                        mmayEdits
                          | haveTerm = doTerm r
                          | haveType = doType r
                          | otherwise = error message
                    mayEdits <- mmayEdits
                    case mayEdits of
                      (Nothing, seen') -> collectEdits es seen' todo
                      (Just edits', seen') -> do
                        -- plan to update the dependents of this component too
                        dependents <- case r of
                          ReferenceBuiltin {} -> Codebase.dependents Queries.ExcludeOwnComponent r
                          Reference.Derived h _i -> Codebase.dependentsOfComponent h
                        let todo' = todo <> getOrdered dependents
                        collectEdits edits' seen' todo'

              doType :: Reference -> Sqlite.Transaction (Maybe (Edits Symbol), Set Reference)
              doType r = do
                when debugMode $ traceM ("Rewriting type: " <> refName r)
                componentMap <- unhashTypeComponent r
                let componentMap' =
                      over _2 (Decl.updateDependencies typeReplacements)
                        <$> componentMap
                    declMap = over _2 (either Decl.toDataDecl id) <$> componentMap'
                    -- TODO: kind-check the new components
                    hashedDecls =
                      (fmap . fmap) (over _2 DerivedId)
                        . Hashing.hashDataDecls
                        $ view _2 <$> declMap
                hashedComponents' <- case hashedDecls of
                  Left _ ->
                    error $
                      "Edit propagation failed because some of the dependencies of "
                        <> show r
                        <> " could not be resolved."
                  Right c -> pure . Map.fromList $ (\(v, r, d) -> (v, (r, d))) <$> c
                let -- Relation: (nameOfType, oldRef, newRef, newType)
                    joinedStuff :: [(Symbol, (Reference, Reference, Decl.DataDeclaration Symbol Ann))]
                    joinedStuff =
                      Map.toList (Map.intersectionWith f declMap hashedComponents')
                    f (oldRef, _) (newRef, newType) = (oldRef, newRef, newType)
                    typeEdits' = typeEdits <> (Map.fromList . fmap toEdit) joinedStuff
                    toEdit (_, (r, r', _)) = (r, TypeEdit.Replace r')
                    typeReplacements' =
                      typeReplacements
                        <> (Map.fromList . fmap toReplacement) joinedStuff
                    toReplacement (_, (r, r', _)) = (r, r')
                    -- New types this iteration
                    newNewTypes = (Map.fromList . fmap toNewType) joinedStuff
                    -- Accumulated new types
                    newTypes' = newTypes <> newNewTypes
                    toNewType (v, (_, r', tp)) =
                      ( r',
                        case Map.lookup v componentMap of
                          Just (_, Left _) -> Left (Decl.EffectDeclaration tp)
                          Just (_, Right _) -> Right tp
                          _ -> error "It's not gone well!"
                      )
                    seen' = seen <> Set.fromList (view _1 . view _2 <$> joinedStuff)
                    writeTypes = traverse_ $ \case
                      (ReferenceDerived id, tp) -> Codebase.putTypeDeclaration codebase id tp
                      _ -> error "propagate: Expected DerivedId"
                    !newCtorMappings =
                      let r = propagateCtorMapping componentMap hashedComponents'
                       in if debugMode then traceShow ("constructorMappings: " :: Text, r) r else r
                    constructorReplacements' = constructorReplacements <> newCtorMappings
                writeTypes $ Map.toList newNewTypes
                pure
                  ( Just $
                      Edits
                        termEdits
                        (newCtorMappings <> termReplacements)
                        newTerms
                        typeEdits'
                        typeReplacements'
                        newTypes'
                        constructorReplacements',
                    seen'
                  )
              doTerm :: Reference -> Sqlite.Transaction (Maybe (Edits Symbol), Set Reference)
              doTerm r = do
                when debugMode (traceM $ "Rewriting term: " <> show r)
                componentMap <- unhashTermComponent codebase r
                let seen' = seen <> Set.fromList (view _1 <$> Map.elems componentMap)
                mayComponent <- do
                  let componentMap' =
                        over
                          _2
                          (Term.updateDependencies termReplacements typeReplacements)
                          <$> componentMap
                  verifyTermComponent codebase componentMap' es
                case mayComponent of
                  Nothing -> do
                    when debugMode (traceM $ refName r <> " did not typecheck after substitutions")
                    pure (Nothing, seen')
                  Just componentMap'' -> do
                    let joinedStuff =
                          toList (Map.intersectionWith f componentMap componentMap'')
                        f (oldRef, _oldTerm, oldType) (newRef, _newWatchKind, newTerm, newType) =
                          (oldRef, newRef, newTerm, oldType, newType')
                          where
                            -- Don't replace the type if it hasn't changed.

                            newType'
                              | Typechecker.isEqual oldType newType = oldType
                              | otherwise = newType
                        -- collect the hashedComponents into edits/replacements/newterms/seen
                        termEdits' =
                          termEdits <> (Map.fromList . fmap toEdit) joinedStuff
                        toEdit (r, r', _newTerm, oldType, newType) =
                          (r, TermEdit.Replace r' $ TermEdit.typing newType oldType)
                        termReplacements' =
                          termReplacements
                            <> (Map.fromList . fmap toReplacement) joinedStuff
                        toReplacement (r, r', _, _, _) = (Referent.Ref r, Referent.Ref r')
                        newTerms' =
                          newTerms <> (Map.fromList . fmap toNewTerm) joinedStuff
                        toNewTerm (_, r', tm, _, tp) = (r', (tm, tp))
                        writeTerms =
                          traverse_ \case
                            (ReferenceDerived id, (tm, tp)) -> Codebase.putTerm codebase id tm tp
                            _ -> error "propagate: Expected DerivedId"
                    writeTerms
                      [(r, (tm, ty)) | (_old, r, tm, _oldTy, ty) <- joinedStuff]
                    pure
                      ( Just $
                          Edits
                            termEdits'
                            termReplacements'
                            newTerms'
                            typeEdits
                            typeReplacements
                            newTypes
                            constructorReplacements,
                        seen'
                      )

      collectEdits
        ( Edits
            initialTermEdits
            (initialTermReplacements initialCtorMappings initialTermEdits)
            mempty
            initialTypeEdits
            initialTypeReplacements
            mempty
            initialCtorMappings
        )
        mempty -- things to skip
        (getOrdered initialDirty)
  where
    initialTermReplacements ctors es =
      ctors
        <> (Map.mapKeys Referent.Ref . fmap Referent.Ref . Map.mapMaybe TermEdit.toReference) es
    sortDependentsGraph :: Set Reference -> Set Reference -> Sqlite.Transaction (Map Reference Int)
    sortDependentsGraph dependencies restrictTo = do
      closure <-
        transitiveClosure
          (fmap (Set.intersection restrictTo) . Codebase.dependents Queries.ExcludeOwnComponent)
          dependencies
      dependents <-
        traverse
          (\r -> (r,) <$> (Codebase.dependents Queries.ExcludeOwnComponent) r)
          (toList closure)
      let graphEdges = [(r, r, toList deps) | (r, deps) <- toList dependents]
          (graph, getReference, _) = Graph.graphFromEdges graphEdges
      pure $
        Map.fromList
          (zip (view _1 . getReference <$> Graph.topSort graph) [0 ..])
    -- vertex i precedes j whenever i has an edge to j and not vice versa.
    -- vertex i precedes j when j is a dependent of i.
    validatePatch ::
      Patch -> Maybe (Map Reference TermEdit, Map Reference TypeEdit)
    validatePatch p =
      (,) <$> R.toMap (Patch._termEdits p) <*> R.toMap (Patch._typeEdits p)

    -- Turns a cycle of references into a term with free vars that we can edit
    -- and hash again.
    -- todo: Maybe this an others can be moved to HandleCommand, in the
    --  Free (Command m i v) monad, passing in the actions that are needed.
    -- However, if we want this to be parametric in the annotation type, then
    -- Command would have to be made parametric in the annotation type too.
    unhashTermComponent ::
      Codebase m Symbol Ann ->
      Reference ->
      Sqlite.Transaction (Map Symbol (Reference, Term Symbol Ann, Type Symbol Ann))
    unhashTermComponent codebase r = case Reference.toId r of
      Nothing -> pure mempty
      Just r -> do
        unhashed <- unhashTermComponent' codebase (Reference.idToHash r)
        pure $ fmap (over _1 ReferenceDerived) unhashed

    unhashTermComponent' ::
      Codebase m Symbol Ann ->
      Hash ->
      Sqlite.Transaction (Map Symbol (Reference.Id, Term Symbol Ann, Type Symbol Ann))
    unhashTermComponent' codebase h = do
      maybeTermsWithTypes <- Codebase.getTermComponentWithTypes codebase h
      pure do
        foldMap (\termsWithTypes -> unhash $ Map.fromList (Reference.componentFor h termsWithTypes)) maybeTermsWithTypes
      where
        unhash m =
          -- this grabs the corresponding input map values (with types)
          -- and arranges them with the newly unhashed terms.
          let f (_oldTm, typ) (v, newTm) = (v, newTm, typ)
              m' = Map.intersectionWith f m (Term.unhashComponent (fst <$> m))
           in Map.fromList
                [(v, (r, tm, tp)) | (r, (v, tm, tp)) <- Map.toList m']

    verifyTermComponent ::
      Codebase IO Symbol Ann ->
      Map Symbol (Reference, Term Symbol Ann, a) ->
      Edits Symbol ->
      Sqlite.Transaction (Maybe (Map Symbol (Reference, Maybe WatchKind, Term Symbol Ann, Type Symbol Ann)))
    verifyTermComponent codebase componentMap Edits {..} = do
      -- If the term contains references to old patterns, we can't update it.
      -- If the term had a redunant type signature, it's discarded and a new type
      -- is inferred. If it wasn't redunant, we have already substituted any updates
      -- into it and we're going to check against that signature.
      --
      -- Note: This only works if the type update is kind-preserving.
      let -- See if the constructor dependencies of any element of the cycle
          -- contains one of the old types.
          terms = Map.elems $ view _2 <$> componentMap
          oldTypes = Map.keysSet typeEdits
      if not . Set.null $
        Set.intersection
          (foldMap Term.constructorDependencies terms)
          oldTypes
        then pure Nothing
        else do
          let file =
                UnisonFileId
                  mempty
                  mempty
                  (componentMap <&> (\(_ref, tm, _) -> (External, tm)))
                  mempty
          typecheckingEnv <- Cli.computeTypecheckingEnvironment FileParsers.ShouldUseTndr'No codebase [] file
          let typecheckResult = FileParsers.synthesizeFile typecheckingEnv file
          Result.result typecheckResult
            & fmap UF.hashTerms
            & (fmap . fmap) (\(_ann, ref, wk, tm, tp) -> (ref, wk, tm, tp))
            & pure

-- TypecheckFile file ambient -> liftIO $ typecheck' ambient codebase file
unhashTypeComponent :: Reference -> Sqlite.Transaction (Map Symbol (Reference, Decl Symbol Ann))
unhashTypeComponent r = case Reference.toId r of
  Nothing -> pure mempty
  Just id -> do
    unhashed <- unhashTypeComponent' (Reference.idToHash id)
    pure $ over _1 Reference.DerivedId <$> unhashed

unhashTypeComponent' :: Hash -> Sqlite.Transaction (Map Symbol (Reference.Id, Decl Symbol Ann))
unhashTypeComponent' h =
  Codebase.getDeclComponent h <&> foldMap \decls ->
    unhash $ Map.fromList (Reference.componentFor h decls)
  where
    unhash =
      Map.fromList . map reshuffle . Map.toList . Decl.unhashComponent
      where
        reshuffle (r, (v, decl)) = (v, (r, decl))

applyDeprecations :: (Applicative m) => Patch -> Branch0 m -> Branch0 m
applyDeprecations patch =
  deleteDeprecatedTerms deprecatedTerms
    . deleteDeprecatedTypes deprecatedTypes
  where
    deprecatedTerms, deprecatedTypes :: Set Reference
    deprecatedTerms =
      Set.fromList
        [r | (r, TermEdit.Deprecate) <- R.toList (Patch._termEdits patch)]
    deprecatedTypes =
      Set.fromList
        [r | (r, TypeEdit.Deprecate) <- R.toList (Patch._typeEdits patch)]
    deleteDeprecatedTerms,
      deleteDeprecatedTypes ::
        Set Reference -> Branch0 m -> Branch0 m
    deleteDeprecatedTerms rs =
      over Branch.terms (Star2.deleteFact (Set.map Referent.Ref rs))
    deleteDeprecatedTypes rs = over Branch.types (Star2.deleteFact rs)

-- | Things in the patch are not marked as propagated changes, but every other
-- definition that is created by the `Edits` which is passed in is marked as
-- a propagated change.
applyPropagate :: forall m. (Applicative m) => Patch -> Edits Symbol -> Branch0 m -> Branch0 m
applyPropagate patch Edits {termReplacements, typeReplacements, constructorReplacements} = do
  -- recursively update names and delete deprecated definitions
  stepEverywhereButLib (updateLevel termReplacements typeReplacements)
  where
    -- Like Branch.stepEverywhere, but don't step the child named "lib"
    stepEverywhereButLib :: (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
    stepEverywhereButLib f branch =
      let children =
            Map.mapWithKey
              (\name child -> if name == NameSegment.libSegment then child else Branch.step (Branch.stepEverywhere f) child)
              (branch ^. Branch.children)
       in f (Branch.branch0 (branch ^. Branch.terms) (branch ^. Branch.types) children (branch ^. Branch.edits))
    isPropagated r = Set.notMember r allPatchTargets
    allPatchTargets = Patch.allReferenceTargets patch
    propagatedMd :: forall r. r -> (r, Metadata.Value)
    propagatedMd r = (r, IOSource.isPropagatedValue)

    updateLevel ::
      Map Referent Referent ->
      Map Reference Reference ->
      Branch0 m ->
      Branch0 m
    updateLevel termEdits typeEdits b =
      Branch.branch0 terms types (b ^. Branch.children) (b ^. Branch.edits)
      where
        isPropagatedReferent (Referent.Con _ _) = True
        isPropagatedReferent (Referent.Ref r) = isPropagated r

        terms0 :: Metadata.Star Referent NameSegment
        terms0 = Star2.replaceFacts replaceConstructor constructorReplacements (b ^. Branch.terms)
        terms :: Branch.Star Referent NameSegment
        terms =
          updateMetadatas $
            Star2.replaceFacts replaceTerm termEdits terms0
        types :: Branch.Star Reference NameSegment
        types =
          updateMetadatas $
            Star2.replaceFacts replaceType typeEdits (b ^. Branch.types)

        updateMetadatas ::
          (Ord r) =>
          Metadata.Star r NameSegment ->
          Metadata.Star r NameSegment
        updateMetadatas s = Star2.mapD2 go s
          where
            go v = case Map.lookup (Referent.Ref v) termEdits of
              Just (Referent.Ref r) -> r
              _ -> v

        replaceTerm :: Referent -> Referent -> Metadata.Star Referent NameSegment -> Metadata.Star Referent NameSegment
        replaceTerm _r r' s =
          ( if isPropagatedReferent r'
              then Metadata.insert (propagatedMd r')
              else Metadata.delete (propagatedMd r')
          )
            $ s

        replaceConstructor ::
          Referent ->
          Referent ->
          Metadata.Star Referent NameSegment ->
          Metadata.Star Referent NameSegment
        replaceConstructor (Referent.Con _ _) !new s =
          -- TODO: revisit this once patches have constructor mappings
          -- at the moment, all constructor replacements are autopropagated
          -- rather than added manually
          Metadata.insert (propagatedMd new) $ s
        replaceConstructor _ _ s = s

        replaceType _ r' s =
          ( if isPropagated r'
              then Metadata.insert (propagatedMd r')
              else Metadata.delete (propagatedMd r')
          )
            $ s

-- typePreservingTermEdits :: Patch -> Patch
-- typePreservingTermEdits Patch {..} = Patch termEdits mempty
--   where termEdits = R.filterRan TermEdit.isTypePreserving _termEdits

-- | Compute the set of "dirty" references. They each:
--
-- 1. Depend directly on some reference that was edited in the given patch
-- 2. Are not themselves edited in the given patch.
-- 3. Pass the given predicate.
computeDirty ::
  (Monad m) =>
  (Reference -> m (Set Reference)) -> -- eg Codebase.dependents codebase
  Patch ->
  (Reference -> Bool) ->
  m (Set Reference)
computeDirty getDependents patch shouldUpdate =
  foldMapM (\ref -> keepDirtyDependents <$> getDependents ref) edited
  where
    -- Given a set of dependent references (satisfying 1. above), keep only the dirty ones (per 2. and 3. above)
    keepDirtyDependents :: Set Reference -> Set Reference
    keepDirtyDependents =
      (`Set.difference` edited) . Set.filter shouldUpdate

    edited :: Set Reference
    edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)

nameNotInLibNamespace :: Name -> Bool
nameNotInLibNamespace name =
  not (Name.beginsWithSegment name NameSegment.libSegment)
