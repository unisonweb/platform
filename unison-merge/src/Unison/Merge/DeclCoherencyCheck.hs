-- | The "decl coherency check": a type declaration in a namespace is "coherent" if it satisfies both of the following
-- criteria.
--
--   1. For each naming of the type decl (say "Foo"#foohash), there exists exactly one name for each of its constructors
--      arbitrarily deep in the corresponding namespace ("Foo" in this example).
--
--      This allows us to render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which corresponds to the three names
--
--        "Foo"                     => #foohash
--        "Foo.Bar"                 => #foohash#0
--        "Foo.internal.hello.Bonk" => #foohash#1
--
--      We could not do if there was at least one constructor whose full name does not contain the full name of the type
--      decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. No constructor has a "stray" name that does not have a prefix that equals the type declaration's name. For
--      example, in the namespace
--
--        "Foo"                 => #foohash
--        "Foo.Bar"             => #foohash#0
--        "Deep.What.SomeAlias" => #foohash#0
--
--      the constructor "What.SomeAlias" is "stray", as the type decl #foohash has no name that matches any prefix
--      (i.e. "Deep.What" nor "Deep").
--
-- On to the implementation. We are going to traverse the namespace depth-first. As we go, we have a stateful mapping
-- between decl reference that we *have* seen a name for in one of our parent namespace, and its corresponding set of
-- constructors that we *haven't* yet seen names for, but expect to, before fully searching the corresponding
-- sub-namespace (e.g. the child namespace named "Foo" of the namepace that declares a decl "Foo").
--
-- When processing a namespace, we first process all terms. Each constructor will fall into one of three cases:
--
-- > +----------------------------------------------------------------------------------------------------------------+
-- > | Case         | Mapping before       | Encountered constructor | Mapping after                                  |
-- > +----------------------------------------------------------------------------------------------------------------+
-- > | Happy path   | { #foo : {0, 1, 2} } | #foo#1                  | { #foo : {0, 2} }                              |
-- > | Already seen | { #foo : {0, 1, 2} } | #foo#5                  | Error: duplicate naming for constructor #foo#5 |
-- > | Never seen   | { #foo : {0, 1, 2} } | #bar#2                  | Error: stray constructor #bar#2                |
-- > +----------------------------------------------------------------------------------------------------------------+
--
-- In "happy path", we see a naming of a constructor that we're expecting, and check it off.
-- In "already seen", we see a second naming of a constructor that we're no longer expecting, and fail.
-- In "never seen", we see a naming of a constructor before any naming of its decl, so we fail.
--
-- Next, we process all type decls. Each will again fall into one of three cases:
--
-- > +-----------------------------------------------------------------------------------------------------+
-- > | Case             | Mapping before       | Declaration | Num constructors | New mapping              |
-- > +-----------------------------------------------------------------------------------------------------+
-- > | Uninhabited decl |                      | #foo        | 0                |                          |
-- > | Inhabited decl   |                      | #foo        | 1 or more        | { #foo : {0, ..., n-1} } |
-- > | Already seen     | { foo : {0, 1, 2}  } | #foo        | Irrelevant       | Error: nested decl alias |
-- > +-----------------------------------------------------------------------------------------------------+
--
-- In "uninhabited decl", we find a decl with no constructors, so we don't expect anything new.
-- In "already seen", we find a second naming of a decl, whose constructors will necessarily violate coherency condition
--   (1) above.
--
-- In "inhabited decl", we find a decl with N constructors, and handle it by:
--   1. Adding to our state that we expect a name for each.
--   2. Recursing into the child namespace whose name matches the decl.
--   3. (If we return from the recursion without short-circuiting) remove the mapping added in step (1) and assert that
--      its value is the empty set (meaning we encountered a name for every constructor).
--
-- Note: This check could be moved into SQLite (with sufficient schema support) some day, but for now, we just do this
-- in memory.
--
-- Note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent". Thus, this
-- machinery was invented.
module Unison.Merge.DeclCoherencyCheck
  ( IncoherentDeclReason (..),
    checkDeclCoherency,
    lenientCheckDeclCoherency,

    -- * Getting all failures rather than just the first
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,
  )
where

import Control.Lens ((%=), (.=), _2)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.State.Strict (State)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import U.Codebase.Reference (Reference' (..), TypeReference, TypeReferenceId)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Map qualified as Map (deleteLookup, deleteLookupJust, upsertF)
import Unison.Util.Nametree (Nametree (..))

data IncoherentDeclReason
  = -- | A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    IncoherentDeclReason'ConstructorAlias !Name !Name !Name -- type, first constructor, second constructor
  | IncoherentDeclReason'MissingConstructorName !Name
  | -- | A second naming of a decl was discovered underneath its name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo
    IncoherentDeclReason'NestedDeclAlias !Name !Name -- shorter name, longer name
  | IncoherentDeclReason'StrayConstructor !TypeReferenceId !Name
  deriving stock (Eq, Show)

checkDeclCoherency ::
  (HasCallStack) =>
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  Map TypeReferenceId Int ->
  Either IncoherentDeclReason DeclNameLookup
checkDeclCoherency nametree numConstructorsById =
  checkDeclCoherencyWith
    (\refId -> Right (expectNumConstructors refId numConstructorsById))
    OnIncoherentDeclReasons
      { onConstructorAlias = \x y z -> Left (IncoherentDeclReason'ConstructorAlias x y z),
        onMissingConstructorName = \x -> Left (IncoherentDeclReason'MissingConstructorName x),
        onNestedDeclAlias = \x y -> Left (IncoherentDeclReason'NestedDeclAlias x y),
        onStrayConstructor = \x y -> Left (IncoherentDeclReason'StrayConstructor x y)
      }
    nametree

data IncoherentDeclReasons = IncoherentDeclReasons
  { constructorAliases :: ![(Name, Name, Name)],
    missingConstructorNames :: ![Name],
    nestedDeclAliases :: ![(Name, Name)],
    strayConstructors :: ![(TypeReferenceId, Name)]
  }
  deriving stock (Eq, Generic)

-- | Like 'checkDeclCoherency', but returns info about all of the incoherent decls found, not just the first.
checkAllDeclCoherency ::
  forall m.
  (Monad m) =>
  (TypeReferenceId -> m Int) ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  m (Either IncoherentDeclReasons DeclNameLookup)
checkAllDeclCoherency loadDeclNumConstructors nametree = do
  State.runStateT doCheck emptyReasons <&> \(declNameLookup, reasons) ->
    if reasons == emptyReasons
      then Right declNameLookup
      else Left (reverseReasons reasons)
  where
    doCheck :: StateT IncoherentDeclReasons m DeclNameLookup
    doCheck =
      checkDeclCoherencyWith
        (lift . loadDeclNumConstructors)
        ( OnIncoherentDeclReasons
            { onConstructorAlias = \x y z -> #constructorAliases %= ((x, y, z) :),
              onMissingConstructorName = \x -> #missingConstructorNames %= (x :),
              onNestedDeclAlias = \x y -> #nestedDeclAliases %= ((x, y) :),
              onStrayConstructor = \x y -> #strayConstructors %= ((x, y) :)
            }
        )
        nametree

    emptyReasons :: IncoherentDeclReasons
    emptyReasons =
      IncoherentDeclReasons [] [] [] []

    reverseReasons :: IncoherentDeclReasons -> IncoherentDeclReasons
    reverseReasons reasons =
      IncoherentDeclReasons
        { constructorAliases = reverse reasons.constructorAliases,
          missingConstructorNames = reverse reasons.missingConstructorNames,
          nestedDeclAliases = reverse reasons.nestedDeclAliases,
          strayConstructors = reverse reasons.strayConstructors
        }

data OnIncoherentDeclReasons m = OnIncoherentDeclReasons
  { onConstructorAlias :: Name -> Name -> Name -> m (),
    onMissingConstructorName :: Name -> m (),
    onNestedDeclAlias :: Name -> Name -> m (),
    onStrayConstructor :: TypeReferenceId -> Name -> m ()
  }

checkDeclCoherencyWith ::
  forall m.
  (Monad m) =>
  (TypeReferenceId -> m Int) ->
  OnIncoherentDeclReasons m ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  m DeclNameLookup
checkDeclCoherencyWith loadDeclNumConstructors callbacks =
  fmap (view #declNameLookup)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty (DeclNameLookup Map.empty Map.empty))
    . go []
  where
    go ::
      [NameSegment] ->
      (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
      StateT DeclCoherencyCheckState m ()
    go prefix (Nametree defns children) = do
      for_
        (Map.toList defns.terms)
        ( checkDeclCoherencyWith_DoTerms
            callbacks
            prefix
        )
      childrenWeWentInto <-
        forMaybe
          (Map.toList defns.types)
          (checkDeclCoherencyWith_DoTypes loadDeclNumConstructors callbacks go prefix children)
      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child

checkDeclCoherencyWith_DoTerms ::
  forall m.
  (Monad m) =>
  OnIncoherentDeclReasons m ->
  [NameSegment] ->
  (NameSegment, Referent) ->
  StateT DeclCoherencyCheckState m ()
checkDeclCoherencyWith_DoTerms callbacks prefix (segment, ref) =
  whenJust (Referent.toConstructorReferenceId ref) \(ConstructorReference typeRef conId) -> do
    let f :: Maybe (Name, ConstructorNames) -> MaybeT m (Name, ConstructorNames)
        f = \case
          Nothing -> do
            lift (callbacks.onStrayConstructor typeRef conName)
            MaybeT (pure Nothing)
          Just (typeName, expected) ->
            case recordConstructorName conId conName expected of
              Left existingName -> do
                lift (callbacks.onConstructorAlias typeName existingName conName)
                MaybeT (pure Nothing)
              Right expected1 -> pure (typeName, expected1)
          where
            conName =
              Name.fromReverseSegments (segment :| prefix)
    state <- State.get
    whenJustM (lift (runMaybeT (Map.upsertF f typeRef state.expectedConstructors))) \expectedConstructors1 ->
      #expectedConstructors .= expectedConstructors1

checkDeclCoherencyWith_DoTypes ::
  forall m.
  (Monad m) =>
  (TypeReferenceId -> m Int) ->
  OnIncoherentDeclReasons m ->
  ( [NameSegment] ->
    (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
    StateT DeclCoherencyCheckState m ()
  ) ->
  [NameSegment] ->
  Map NameSegment (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  (NameSegment, TypeReference) ->
  StateT DeclCoherencyCheckState m (Maybe NameSegment)
checkDeclCoherencyWith_DoTypes loadDeclNumConstructors callbacks go prefix children (name, ref) =
  case Reference.toId ref of
    Nothing -> pure Nothing
    Just refId ->
      checkDeclCoherencyWith_DoTypes2 loadDeclNumConstructors callbacks go prefix children name refId

checkDeclCoherencyWith_DoTypes2 ::
  forall m.
  (Monad m) =>
  (TypeReferenceId -> m Int) ->
  OnIncoherentDeclReasons m ->
  ( [NameSegment] ->
    (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
    StateT DeclCoherencyCheckState m ()
  ) ->
  [NameSegment] ->
  Map NameSegment (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  NameSegment ->
  TypeReferenceId ->
  StateT DeclCoherencyCheckState m (Maybe NameSegment)
checkDeclCoherencyWith_DoTypes2 loadDeclNumConstructors callbacks go prefix children name typeRef = do
  state <- State.get
  lift (runMaybeT (getCompose (Map.upsertF recordNewDecl typeRef state.expectedConstructors))) >>= \case
    Nothing -> pure Nothing
    Just UninhabitedDecl -> do
      #declNameLookup . #declToConstructors %= Map.insert typeName []
      pure Nothing
    Just (InhabitedDecl expectedConstructors1) -> do
      case Map.lookup name children of
        Nothing -> do
          lift (callbacks.onMissingConstructorName typeName)
          pure Nothing
        Just child -> do
          #expectedConstructors .= expectedConstructors1
          go (name : prefix) child
          state <- State.get
          -- fromJust is safe here because we upserted `typeRef` key above
          let (fromJust -> (_typeName, maybeConstructorNames), expectedConstructors1) =
                Map.deleteLookup typeRef state.expectedConstructors
          #expectedConstructors .= expectedConstructors1
          case sequence (IntMap.elems maybeConstructorNames) of
            Nothing -> lift (callbacks.onMissingConstructorName typeName)
            Just constructorNames -> do
              #declNameLookup . #constructorToDecl %= \constructorToDecl ->
                List.foldl'
                  (\acc constructorName -> Map.insert constructorName typeName acc)
                  constructorToDecl
                  constructorNames
              #declNameLookup . #declToConstructors %= Map.insert typeName constructorNames
          pure (Just name)
  where
    typeName :: Name
    typeName =
      Name.fromReverseSegments (name :| prefix)

    recordNewDecl :: Maybe (Name, ConstructorNames) -> Compose (MaybeT m) WhatHappened (Name, ConstructorNames)
    recordNewDecl =
      Compose . \case
        Just (shorterTypeName, _) -> do
          lift (callbacks.onNestedDeclAlias shorterTypeName typeName)
          MaybeT (pure Nothing)
        Nothing ->
          lift (loadDeclNumConstructors typeRef) <&> \case
            0 -> UninhabitedDecl
            n -> InhabitedDecl (typeName, emptyConstructorNames n)

-- | A lenient variant of 'checkDeclCoherency' - so lenient it can't even fail! It returns partial decl name lookup,
-- which doesn't require a name for every constructor, and allows a constructor with a nameless decl.
--
-- This function exists merely to extract a best-effort name mapping for the LCA of a merge. We require Alice and Bob to
-- have coherent decls, but their LCA is out of the user's control and may have incoherent decls, and whether or not it
-- does, we still need to compute *some* syntactic hash for its decls.
lenientCheckDeclCoherency ::
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  Map TypeReferenceId Int ->
  PartialDeclNameLookup
lenientCheckDeclCoherency nametree numConstructorsById =
  nametree
    & go []
    & (`State.execState` LenientDeclCoherencyCheckState Map.empty (PartialDeclNameLookup Map.empty Map.empty))
    & view #declNameLookup
  where
    go ::
      [NameSegment] ->
      (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
      State LenientDeclCoherencyCheckState ()
    go prefix (Nametree defns children) = do
      for_ (Map.toList defns.terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ConstructorReference (ReferenceBuiltin _) _) _) -> pure ()
        (name, Referent.Con (ConstructorReference (ReferenceDerived typeRef) conId) _) -> do
          #expectedConstructors %= Map.adjust (Map.map (lenientRecordConstructorName conId (fullName name))) typeRef

      childrenWeWentInto <-
        forMaybe (Map.toList defns.types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            state <- State.get
            let whatHappened =
                  let recordNewDecl :: WhatHappened (Map Name ConstructorNames)
                      recordNewDecl =
                        case expectNumConstructors typeRef numConstructorsById of
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (Map.singleton typeName (emptyConstructorNames n))
                   in Map.upsertF (\_ -> recordNewDecl) typeRef state.expectedConstructors
            case whatHappened of
              UninhabitedDecl -> do
                #declNameLookup . #declToConstructors %= Map.insert typeName []
                pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                let child = Map.findWithDefault (Nametree (Defns Map.empty Map.empty) Map.empty) name children
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                state <- State.get
                let (constructorNames0, expectedConstructors) =
                      Map.alterF f typeRef state.expectedConstructors
                      where
                        f ::
                          Maybe (Map Name ConstructorNames) ->
                          (ConstructorNames, Maybe (Map Name ConstructorNames))
                        f =
                          -- fromJust is safe here because we upserted `typeRef` key above
                          -- deleteLookupJust is safe here because we upserted `typeName` key above
                          fromJust
                            >>> Map.deleteLookupJust typeName
                            >>> over _2 \m -> if Map.null m then Nothing else Just m

                    constructorNames :: [Maybe Name]
                    constructorNames =
                      IntMap.elems constructorNames0

                #expectedConstructors .= expectedConstructors
                #declNameLookup . #constructorToDecl %= \constructorToDecl ->
                  List.foldl'
                    ( \acc -> \case
                        Nothing -> acc
                        Just constructorName -> Map.insert constructorName typeName acc
                    )
                    constructorToDecl
                    constructorNames
                #declNameLookup . #declToConstructors %= Map.insert typeName constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (Name, ConstructorNames)),
    declNameLookup :: !DeclNameLookup
  }
  deriving stock (Generic)

data LenientDeclCoherencyCheckState = LenientDeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (Map Name ConstructorNames)),
    declNameLookup :: !PartialDeclNameLookup
  }
  deriving stock (Generic)

-- A partial mapping from constructor id to name; a collection of constructor names starts out with the correct number
-- of keys (per the number of data constructors) all mapped to Nothing. Then, as names are discovered by walking a
-- name tree, Nothings become Justs.
type ConstructorNames =
  IntMap (Maybe Name)

-- Make an empty set of constructor names given the number of constructors.
emptyConstructorNames :: Int -> ConstructorNames
emptyConstructorNames numConstructors =
  IntMap.fromAscList [(i, Nothing) | i <- [0 .. numConstructors - 1]]

recordConstructorName :: (HasCallStack) => ConstructorId -> Name -> ConstructorNames -> Either Name ConstructorNames
recordConstructorName conId conName =
  IntMap.alterF f (fromIntegral @Word64 @Int conId)
  where
    f :: Maybe (Maybe Name) -> Either Name (Maybe (Maybe Name))
    f = \case
      Nothing -> error (reportBug "E397219" ("recordConstructorName: didn't expect constructor id " ++ show conId))
      Just Nothing -> Right (Just (Just conName))
      Just (Just existingName) -> Left existingName

lenientRecordConstructorName :: ConstructorId -> Name -> ConstructorNames -> ConstructorNames
lenientRecordConstructorName conId conName =
  IntMap.adjust f (fromIntegral @Word64 @Int conId)
  where
    f :: Maybe Name -> Maybe Name
    f = \case
      Nothing -> Just conName
      -- Ignore constructor alias, just keep first name we found
      Just existingName -> Just existingName

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)

expectNumConstructors :: (HasCallStack) => TypeReferenceId -> Map TypeReferenceId Int -> Int
expectNumConstructors refId numConstructorsById =
  case Map.lookup refId numConstructorsById of
    Just numConstructors -> numConstructors
    Nothing ->
      error $
        reportBug "E061715" ("type ref " ++ show refId ++ " not found in map " ++ show numConstructorsById)
