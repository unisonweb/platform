{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.Propagate where

import           Control.Error.Util             ( hush )
import           Control.Lens
import           Data.Configurator              ( )
import qualified Data.Graph                    as Graph
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import           Unison.Prelude
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Editor.Command
import           Unison.Codebase.Editor.Output
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.DataDeclaration         ( Decl )
import qualified Unison.DataDeclaration        as Decl
import           Unison.Names3                  ( Names0 )
import qualified Unison.Names2                 as Names
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Result                 as Result
import qualified Unison.Term                   as Term
import           Unison.Util.Free               ( Free
                                                , eval
                                                )
import qualified Unison.Util.Relation          as R
import           Unison.Util.TransitiveClosure  ( transitiveClosure )
import           Unison.Var                     ( Var )
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.Codebase.TermEdit       ( TermEdit(..) )
import qualified Unison.Codebase.TermEdit      as TermEdit
import           Unison.Codebase.TypeEdit       ( TypeEdit(..) )
import           Unison.UnisonFile              ( UnisonFile(..) )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import           Unison.Type                    ( Type )
import qualified Unison.Typechecker            as Typechecker

type F m i v = Free (Command m i v)
type Term v a = Term.AnnotatedTerm v a

data Edits v = Edits
  { termEdits :: Map Reference TermEdit
  , termReplacements :: Map Reference Reference
  , newTerms :: Map Reference (Term v Ann, Type v Ann)
  , typeEdits :: Map Reference TypeEdit
  , typeReplacements :: Map Reference Reference
  , newTypes :: Map Reference (Decl v Ann)
  } deriving (Eq, Show)

noEdits :: Edits v
noEdits = Edits mempty mempty mempty mempty mempty mempty

propagateAndApply
  :: forall m i v
   . (Applicative m, Var v)
  => Patch
  -> Branch m
  -> F m i v (Branch m)
propagateAndApply patch branch = do
  edits <- propagate patch branch
  f     <- applyPropagate edits
  pure $ Branch.step (f . applyDeprecations patch) branch

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
propagate
  :: forall m i v
   . (Applicative m, Var v)
  => Patch
  -> Branch m
  -> F m i v (Edits v)
propagate patch b = case validatePatch patch of
  Nothing -> do
    eval $ Notify PatchNeedsToBeConflictFree
    pure noEdits
  Just (initialTermEdits, initialTypeEdits) -> do
    initialDirty <-
      R.dom <$> computeFrontier (eval . GetDependents) patch names0
    missing :: Set Reference <- missingDependents
      initialDirty
      ( Set.fromList
      . mapMaybe Referent.toTermReference
      . toList
      . Branch.deepReferents
      . Branch.head
      $ b
      )
    if not $ Set.null missing
      then pure noEdits
      else do
        order <- sortDependentsGraph initialDirty
        let
          getOrdered :: Set Reference -> Map Int Reference
          getOrdered rs = Map.fromList
            [ (i, r) | r <- toList rs, Just i <- [Map.lookup r order] ]
          collectEdits
            :: (Applicative m, Var v)
            => Edits v
            -> Set Reference
            -> Map Int Reference
            -> F m i v (Edits v)
          collectEdits es@Edits {..} seen todo = case Map.minView todo of
            Nothing        -> pure es
            Just (r, todo) -> case r of
              Reference.Builtin   _ -> collectEdits es seen todo
              Reference.DerivedId _ -> go r todo
           where
            go r todo =
              if Map.member r termEdits
                 || Map.member r typeEdits
                 || Set.member r seen
              then
                collectEdits es seen todo
              else
                do
                  haveType <- eval $ IsType r
                  haveTerm <- eval $ IsTerm r
                  let message =
                        "This reference is not a term nor a type " <> show r
                      mmayEdits | haveTerm  = doTerm r
                                | haveType  = doType r
                                | otherwise = fail message
                  mayEdits <- mmayEdits
                  case mayEdits of
                    (Nothing    , seen') -> collectEdits es seen' todo
                    (Just edits', seen') -> do
                      -- plan to update the dependents of this component too
                      dependents <-
                        fmap Set.unions
                        . traverse (eval . GetDependents)
                        . toList
                        . Reference.members
                        $ Reference.componentFor r
                      let todo' = todo <> getOrdered dependents
                      collectEdits edits' seen' todo'
            doType :: Reference -> F m i v (Maybe (Edits v), Set Reference)
            doType r =
              do
                componentMap <- unhashTypeComponent r
                let componentMap' =
                      over _2 (Decl.updateDependencies typeReplacements)
                      <$> componentMap
                    declMap = over _2 (either Decl.toDataDecl id) <$> componentMap'
                    -- TODO: kind-check the new components
                    hashedDecls =
                      Decl.hashDecls $ view _2 <$> declMap
                hashedComponents' <-
                  case hashedDecls of
                    Left _ ->
                      fail $
                        "Edit propagation failed because some of the dependencies of "
                        <> show r <> " could not be resolved."
                    Right c -> pure . Map.fromList $ (\(v,r,d) -> (v, (r,d))) <$> c
                let joinedStuff
                      :: [(v, (Reference, Reference, Decl.DataDeclaration' v _))]
                    joinedStuff = Map.toList
                      (Map.intersectionWith f declMap hashedComponents')
                    f (oldRef, _) (newRef, newType) =
                      (oldRef, newRef, newType)
                    typeEdits' =
                      typeEdits <> (Map.fromList . fmap toEdit) joinedStuff
                    toEdit (_, (r, r', _)) =
                      (r, TypeEdit.Replace r')
                    typeReplacements' = typeReplacements
                      <> (Map.fromList . fmap toReplacement) joinedStuff
                    toReplacement (_, (r, r', _)) = (r, r')
                    newTypes' =
                      newTypes <> (Map.fromList . fmap toNewType) joinedStuff
                    toNewType (v, (_, r', tp)) =
                      (r', case Map.lookup v componentMap of
                             Just (_, Left _) -> Left (Decl.EffectDeclaration tp)
                             Just (_, Right _) -> Right tp
                             _ -> error "It's not gone well!"
                      )
                    seen' = seen <> Set.fromList (view _1 . view _2 <$> joinedStuff)
                    writeTypes =
                      traverse_ (\(Reference.DerivedId id, tp) -> eval $ PutDecl id tp)
                writeTypes (Map.elems componentMap)
                pure ( Just $ Edits termEdits
                               termReplacements
                               newTerms
                               typeEdits'
                               typeReplacements'
                               newTypes'
                     , seen')
            doTerm :: Reference -> F m i v (Maybe (Edits v), Set Reference)
            doTerm r = do
              componentMap <- unhashTermComponent r
              let componentMap' =
                    over
                        _2
                        (Term.updateDependencies termReplacements
                                                 typeReplacements
                        )
                      <$> componentMap
                  seen' = seen <> Set.fromList (view _1 <$> Map.elems componentMap)
              mayComponent <- verifyTermComponent componentMap' es
              case mayComponent of
                Nothing -> pure (Nothing, seen')
                Just componentMap'' -> do
                  let
                    joinedStuff = toList
                      (Map.intersectionWith f componentMap componentMap'')
                    f (oldRef, _oldTerm, oldType) (newRef, newTerm, newType) =
                      (oldRef, newRef, newTerm, oldType, newType')
                        -- Don't replace the type if it hasn't changed.
                        where newType'
                               | Typechecker.isEqual oldType newType = oldType
                               | otherwise = newType
                -- collect the hashedComponents into edits/replacements/newterms/seen
                    termEdits' =
                      termEdits <> (Map.fromList . fmap toEdit) joinedStuff
                    toEdit (r, r', _newTerm, oldType, newType) =
                      (r, TermEdit.Replace r' $ TermEdit.typing newType oldType)
                    termReplacements' = termReplacements
                      <> (Map.fromList . fmap toReplacement) joinedStuff
                    toReplacement (r, r', _, _, _) = (r, r')
                    newTerms' =
                      newTerms <> (Map.fromList . fmap toNewTerm) joinedStuff
                    toNewTerm (_, r', tm, _, tp) = (r', (tm, tp))
                    writeTerms =
                      traverse_ (\(Reference.DerivedId id, (tm, tp)) -> eval $ PutTerm id tm tp)
                  writeTerms [ (r, (tm,ty)) | (_old,r,tm,_oldTy,ty) <- joinedStuff ]
                  pure
                    ( Just $ Edits termEdits'
                                   termReplacements'
                                   newTerms'
                                   typeEdits
                                   typeReplacements
                                   newTypes
                    , seen'
                    )
        collectEdits
          (Edits initialTermEdits
                 (Map.mapMaybe TermEdit.toReference initialTermEdits)
                 mempty
                 initialTypeEdits
                 (Map.mapMaybe TypeEdit.toReference initialTypeEdits)
                 mempty
          )
          mempty -- things to skip
          (getOrdered initialDirty)
 where
  missingDependents :: Set Reference -> Set Reference -> _ (Set Reference)
  missingDependents dirty known = do
    closure <- transitiveClosure (eval . GetDependents) dirty
    pure $ Set.difference closure known
  sortDependentsGraph :: Set Reference -> _ (Map Reference Int)
  sortDependentsGraph rs = do
    closure    <- transitiveClosure (eval . GetDependents) rs
    dependents <- traverse (\r -> (r, ) <$> (eval . GetDependents) r)
                           (toList closure)
    let graphEdges = [ (r, r, toList deps) | (r, deps) <- toList dependents ]
        (graph, getReference, _) = Graph.graphFromEdges graphEdges
    pure $ Map.fromList
      (zip (view _1 . getReference <$> Graph.topSort graph) [0 ..])
    -- vertex i precedes j whenever i has an edge to j and not vice versa.
    -- vertex i precedes j when j is a dependent of i.
  names0 = (Branch.toNames0 . Branch.head) b
  validatePatch
    :: Patch -> Maybe (Map Reference TermEdit, Map Reference TypeEdit)
  validatePatch p =
    (,) <$> R.toMap (Patch._termEdits p) <*> R.toMap (Patch._typeEdits p)
  -- Turns a cycle of references into a term with free vars that we can edit
  -- and hash again.
  -- todo: Maybe this an others can be moved to HandleCommand, in the
  --  Free (Command m i v) monad, passing in the actions that are needed.
  -- However, if we want this to be parametric in the annotation type, then
  -- Command would have to be made parametric in the annotation type too.
  unhashTermComponent
    :: forall m v
     . (Applicative m, Var v)
    => Reference
    -> F m i v (Map v (Reference, Term v _, Type v _))
  unhashTermComponent ref = do
    let
      component = Reference.members $ Reference.componentFor ref
      termInfo
        :: Reference
        -> F m i v (Maybe (Reference, (Term v Ann, Type v Ann)))
      termInfo termRef = do
        tpm <- eval $ LoadTypeOfTerm termRef
        tp  <- maybe (fail $ "Missing type for term " <> show termRef) pure tpm
        case termRef of
          Reference.DerivedId id -> do
            mtm <- eval $ LoadTerm id
            tm  <- maybe (fail $ "Missing term with id " <> show id) pure mtm
            pure $ Just (termRef, (tm, tp))
          _ -> pure Nothing
      unhash m =
        let f (_oldTm,oldTyp) (v,newTm) = (v,newTm,oldTyp)
            m' = Map.intersectionWith f m (Term.unhashComponent (fst <$> m))
        in Map.fromList [ (v, (r, tm, tp)) | (r, (v, tm, tp)) <- Map.toList m' ]
    unhash . Map.fromList . catMaybes <$> traverse termInfo (toList component)
  unhashTypeComponent
    :: forall m v
     . (Applicative m, Var v)
    => Reference
    -> F m i v (Map v (Reference, Decl v _))
  unhashTypeComponent ref = do
    let
      component = Reference.members $ Reference.componentFor ref
      typeInfo :: Reference -> F m i v (Maybe (Reference, Decl v Ann))
      typeInfo typeRef = case typeRef of
        Reference.DerivedId id -> do
          declm <- eval $ LoadType id
          decl  <- maybe (fail $ "Missing type declaration " <> show typeRef)
                         pure
                         declm
          pure $ Just (typeRef, decl)
        _ -> pure Nothing
      unhash = Map.fromList . map reshuffle . Map.toList . Decl.unhashComponent
        where reshuffle (r, (v, decl)) = (v, (r, decl))
    unhash . Map.fromList . catMaybes <$> traverse typeInfo (toList component)
  verifyTermComponent
    :: Map v (Reference, Term v _, a)
    -> Edits v
    -> F m i v (Maybe (Map v (Reference, Term v _, Type v _)))
  verifyTermComponent componentMap Edits {..} = do
    -- If the term contains references to old patterns, we can't update it.
    -- If the term had a redunant type signature, it's discarded and a new type
    -- is inferred. If it wasn't redunant, we have already substituted any updates
    -- into it and we're going to check against that signature.
    --
    -- Note: This only works if the type update is kind-preserving.
    let
      -- See if the constructor dependencies of any element of the cycle
      -- contains one of the old types.
        terms    = Map.elems $ view _2 <$> componentMap
        oldTypes = Map.keysSet typeEdits
    if not . Set.null $ Set.intersection
         (foldMap Term.constructorDependencies terms)
         oldTypes
      then pure Nothing
      else do
        let file = UnisonFile
              mempty
              mempty
              (Map.toList $ (\(_, tm, _) -> tm) <$> componentMap)
              mempty
        typecheckResult <- eval $ TypecheckFile file []
        pure . fmap UF.hashTerms $ runIdentity (
          Result.toMaybe typecheckResult) >>= hush

applyDeprecations :: Applicative m => Patch -> Branch0 m -> Branch0 m
applyDeprecations patch = deleteDeprecatedTerms deprecatedTerms
  . deleteDeprecatedTypes deprecatedTypes
 where
  deprecatedTerms, deprecatedTypes :: Set Reference
  deprecatedTerms = Set.fromList
    [ r | (r, TermEdit.Deprecate) <- R.toList (Patch._termEdits patch) ]
  deprecatedTypes = Set.fromList
    [ r | (r, TypeEdit.Deprecate) <- R.toList (Patch._typeEdits patch) ]
  deleteDeprecatedTerms, deleteDeprecatedTypes
    :: Set Reference -> Branch0 m -> Branch0 m
  deleteDeprecatedTerms rs =
    over Branch.terms (Star3.deleteFact (Set.map Referent.Ref rs))
  deleteDeprecatedTypes rs = over Branch.types (Star3.deleteFact rs)

applyPropagate
  :: Show v => Applicative m => Edits v -> F m i v (Branch0 m -> Branch0 m)
applyPropagate Edits {..} = do
  let termRefs        = Map.mapMaybe TermEdit.toReference termEdits
      typeRefs        = Map.mapMaybe TypeEdit.toReference typeEdits
  -- recursively update names and delete deprecated definitions
  pure $ Branch.stepEverywhere (updateNames termRefs typeRefs)
 where
  updateNames
    :: Map Reference Reference
    -> Map Reference Reference
    -> Branch0 m
    -> Branch0 m
  updateNames termEdits typeEdits Branch0 {..} = Branch.branch0 terms
                                                                types
                                                                _children
                                                                _edits
   where
    terms = foldl' replaceTerm _terms (Map.toList termEdits)
    types = foldl' replaceType _types (Map.toList typeEdits)
    replaceTerm s (r, r') =
      Star3.replaceFact (Referent.Ref r) (Referent.Ref r') s
    replaceType s (r, r') = Star3.replaceFact r r' s
  -- typePreservingTermEdits :: Patch -> Patch
  -- typePreservingTermEdits Patch {..} = Patch termEdits mempty
  --   where termEdits = R.filterRan TermEdit.isTypePreserving _termEdits

-- (d, f) when d is "dirty" (needs update),
--             f is in the frontier (an edited dependency of d),
--         and d depends on f
-- a ⋖ b = a depends directly on b
-- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
computeFrontier
  :: forall m
   . Monad m
  => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
  -> Patch
  -> Names0
  -> m (R.Relation Reference Reference)
computeFrontier getDependents patch names = do
      -- (r,r2) ∈ dependsOn if r depends on r2
  dependsOn <- foldM addDependents R.empty edited
  -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
  pure $ R.filterDom (not . flip Set.member edited) dependsOn
 where
  edited :: Set Reference
  edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)
  addDependents
    :: R.Relation Reference Reference
    -> Reference
    -> m (R.Relation Reference Reference)
  addDependents dependents ref =
    (\ds -> R.insertManyDom ds ref dependents)
      .   Set.filter (Names.contains names)
      <$> getDependents ref
