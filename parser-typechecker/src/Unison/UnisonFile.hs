{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.UnisonFile
  ( -- * UnisonFile
    UnisonFile (..),
    pattern UnisonFile,
    emptyUnisonFile,
    allWatches,
    dataDeclarations,
    declsToTypeLookup,
    dependencies,
    effectDeclarations,
    typecheckingTerm,
    watchesOfKind,
    definitionLocation,
    termBindings,
    leftBiasedMerge,

    -- * TypecheckedUnisonFile
    TypecheckedUnisonFile (..),
    allTerms,
    dataDeclarations',
    discardTypes,
    effectDeclarations',
    hashConstructors,
    constructorsForDecls,
    hashTerms,
    indexByReference,
    lookupDecl,
    nonEmpty,
    termSignatureExternalLabeledDependencies,
    topLevelComponents,
    typecheckedToTypeLookup,
    typecheckedUnisonFile,
    Unison.UnisonFile.rewrite,
    prepareRewrite,
    termNamespaceBindings,
    typeNamespaceBindings,
  )
where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Hash qualified as Hash
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.Reference (Reference, TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile.Type (TypecheckedUnisonFile (..), UnisonFile (..), pattern TypecheckedUnisonFile, pattern UnisonFile)
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.List qualified as List
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind, pattern TestWatch)
import Unison.WatchKind qualified as WatchKind

-- | An empty Unison file.
emptyUnisonFile :: UnisonFile v a
emptyUnisonFile =
  UnisonFileId
    { dataDeclarationsId = Map.empty,
      effectDeclarationsId = Map.empty,
      terms = Map.empty,
      watches = Map.empty
    }

leftBiasedMerge :: forall v a. (Ord v) => UnisonFile v a -> UnisonFile v a -> UnisonFile v a
leftBiasedMerge lhs rhs =
  let mergedTerms = Map.foldlWithKey' (addNotIn lhsTermNames) lhs.terms rhs.terms
      mergedWatches = Map.foldlWithKey' addWatch (watches lhs) (watches rhs)
      mergedDataDecls = Map.foldlWithKey' (addNotIn lhsTypeNames) (dataDeclarationsId lhs) (dataDeclarationsId rhs)
      mergedEffectDecls = Map.foldlWithKey' (addNotIn lhsTypeNames) (effectDeclarationsId lhs) (effectDeclarationsId rhs)
   in UnisonFileId
        { dataDeclarationsId = mergedDataDecls,
          effectDeclarationsId = mergedEffectDecls,
          terms = mergedTerms,
          watches = mergedWatches
        }
  where
    lhsTermNames =
      Map.keysSet lhs.terms
        <> foldMap (\x -> Set.fromList [v | (v, _, _) <- x]) (watches lhs)

    lhsTypeNames =
      Map.keysSet (dataDeclarationsId lhs)
        <> Map.keysSet (effectDeclarationsId lhs)

    addNotIn :: forall x. Set v -> Map v x -> v -> x -> Map v x
    addNotIn namesToAvoid b k v = case Set.member k namesToAvoid of
      True -> b
      False -> Map.insert k v b

    addWatch :: Map WatchKind [(v, a, Term v a)] -> WatchKind -> [(v, a, Term v a)] -> Map WatchKind [(v, a, Term v a)]
    addWatch b k v = case filter (\(x, _, _) -> not $ Set.member x lhsTermNames) v of
      [] -> b
      v -> Map.insertWith (++) k v b

dataDeclarations :: UnisonFile v a -> Map v (Reference, DataDeclaration v a)
dataDeclarations = fmap (first Reference.DerivedId) . dataDeclarationsId

effectDeclarations :: UnisonFile v a -> Map v (Reference, EffectDeclaration v a)
effectDeclarations = fmap (first Reference.DerivedId) . effectDeclarationsId

watchesOfKind :: WatchKind -> UnisonFile v a -> [(v, a, Term v a)]
watchesOfKind kind uf = Map.findWithDefault [] kind (watches uf)

watchesOfOtherKinds :: WatchKind -> UnisonFile v a -> [(v, a, Term v a)]
watchesOfOtherKinds kind uf =
  join [ws | (k, ws) <- Map.toList (watches uf), k /= kind]

allWatches :: UnisonFile v a -> [(v, a, Term v a)]
allWatches = join . Map.elems . watches

-- | Get the location of a given definition in the file.
definitionLocation :: (Var v) => v -> UnisonFile v a -> Maybe a
definitionLocation v uf =
  uf.terms ^? ix v . _1
    <|> watches uf ^? folded . folded . filteredBy (_1 . only v) . _2
    <|> dataDeclarations uf ^? ix v . _2 . to DD.annotation
    <|> effectDeclarations uf ^? ix v . _2 . to (DD.annotation . DD.toDataDecl)

-- Converts a file to a single let rec with a body of `()`, for
-- purposes of typechecking.
typecheckingTerm :: (Var v, Monoid a) => UnisonFile v a -> Term v a
typecheckingTerm uf =
  Term.letRec' True bindings $
    DD.unitTerm mempty
  where
    bindings =
      termBindings uf <> testWatches <> watchesOfOtherKinds TestWatch uf
    -- we make sure each test has type Test.Result
    f w = let wa = ABT.annotation w in Term.ann wa w (DD.testResultListType wa)
    testWatches = map (second f) $ watchesOfKind TestWatch uf

termBindings :: UnisonFile v a -> [(v, a, Term v a)]
termBindings uf =
  Map.foldrWithKey (\k (a, t) b -> (k, a, t) : b) [] uf.terms

-- backwards compatibility with the old data type
dataDeclarations' :: TypecheckedUnisonFile v a -> Map v (Reference, DataDeclaration v a)
dataDeclarations' = fmap (first Reference.DerivedId) . dataDeclarationsId'

effectDeclarations' :: TypecheckedUnisonFile v a -> Map v (Reference, EffectDeclaration v a)
effectDeclarations' = fmap (first Reference.DerivedId) . effectDeclarationsId'

hashTerms :: TypecheckedUnisonFile v a -> Map v (a, Reference, Maybe WatchKind, Term v a, Type v a)
hashTerms = fmap (over _2 Reference.DerivedId) . hashTermsId

mapTerms :: (Term v a -> Term v a) -> UnisonFile v a -> UnisonFile v a
mapTerms f (UnisonFileId datas effects terms watches) =
  UnisonFileId datas effects terms' watches'
  where
    terms' = over (mapped . _2) f terms
    watches' = over (mapped . mapped . _3) f watches

-- | This function should be called in preparation for a call to
-- UnisonFile.rewrite. It prevents the possibility of accidental
-- variable capture while still allowing the rules to capture variables
-- where that's the intent. For example:
--
--   f x = x + 42
--   ex = List.map (x -> Nat.increment x) [1,2,3]
--
--   rule1 f = @rewrite term (x -> f x) ==> f
--   rule2 = @rewrite term (x -> f x) ==> f
--
-- Here, `rule1` introduces a variable `f`, which can stand for
-- any definition. Whereas `rule2` refers to the top-level `f`
-- function in the file.
--
-- This function returns a tuple of: (prepareRule, preparedFile, finish)
--   `prepareRule` should be called on any `@rewrite` block to do
--                 prevent accidental capture. It receives the [v] of
--                 variables bound locally by the rule (`rule1` above binds `f`).
--   `preparedFile` should be passed to `UnisonFile.rewrite`
--   `finish` should be called on the result of `UnisonFile.rewrite`
--
-- Internally, the function works by replacing all free variables in the file
-- with a unique reference, performing the rewrite using the ABT machinery,
-- then converting back to a "regular" UnisonFile with free variables in the
-- terms.
prepareRewrite :: (Monoid a, Var v) => UnisonFile v a -> ([v] -> Term v a -> Term v a, UnisonFile v a, UnisonFile v a -> UnisonFile v a)
prepareRewrite uf@(UnisonFileId _datas _effects _terms watches) =
  (freshen, mapTerms substs uf, mapTerms refToVar)
  where
    -- fn to replace free vars with unique refs
    substs = ABT.substsInheritAnnotation varToRef
    -- fn to replace free variables of a @rewrite block with unique refs
    --   subtlety: we freshen any vars which are used in the file to avoid
    --   accidental capture
    freshen vs tm = case ABT.freshenWrt Var.bakeId (typecheckingTerm uf) [tm1] of
      [tm] -> tm
      _ -> error "prepareRewrite bug (in freshen)"
      where
        -- logic to leave alone variables bound by @rewrite block
        tm0 = ABT.absChain' (repeat (ABT.annotation tm) `zip` vs) tm
        tm1 = ABT.dropAbs (length vs) (substs tm0)
    -- An arbitrary, random unique hash, generated via /dev/urandom
    -- we just need something that won't collide with refs
    h = Hash.fromByteString "f0dd645e2382aba2035350297fb2a26263d1891965e5f351e19ae69317b1c866"
    varToRef =
      [(v, Term.ref () (Reference.Derived h i)) | (v, i) <- vs `zip` [0 ..]]
      where
        vs = (view _1 <$> (termBindings uf)) <> (toList watches >>= map (view _1))
    vars = Vector.fromList (fst <$> varToRef)
    -- function to convert unique refs back to free variables
    refToVar = ABT.rebuildUp' go
      where
        go tm@(Term.Ref' (Reference.Derived h0 i)) | h == h0 =
          case vars Vector.!? (fromIntegral i) of
            Just v -> Term.var (ABT.annotation tm) v
            Nothing -> error $ "UnisonFile.prepareRewrite bug, index out of bounds: " ++ show i
        go tm = tm

-- Rewrite a UnisonFile using a function for transforming terms.
-- The function should return `Nothing` if the term is unchanged.
-- This function returns what symbols were modified.
-- The `Set v` is symbols that should be left alone.
rewrite :: (Var v, Eq a) => Set v -> (Term v a -> Maybe (Term v a)) -> UnisonFile v a -> ([v], UnisonFile v a)
rewrite leaveAlone rewriteFn uf@(UnisonFileId datas effects _terms watches) =
  (rewritten, UnisonFileId datas effects (Map.fromList $ unEitherTerms terms') (unEither <$> watches'))
  where
    terms' = go (termBindings uf)
    watches' = go <$> watches
    go tms = [(v, a, tm') | (v, a, tm) <- tms, tm' <- f v tm]
      where
        f v tm | Set.member v leaveAlone = [Left tm]
        f _ tm = maybe [Left tm] (pure . Right) (rewriteFn tm)
    rewritten = [v | (v, _, Right _) <- terms' <> join (toList watches')]
    unEitherTerms = fmap (\(v, a, e) -> (v, (a, either id id e)))
    unEither = fmap (\(v, a, e) -> (v, a, either id id e))

typecheckedUnisonFile ::
  forall v a.
  (Var v) =>
  Map v (Reference.Id, DataDeclaration v a) ->
  Map v (Reference.Id, EffectDeclaration v a) ->
  [[(v, a, Term v a, Type v a)]] ->
  [(WatchKind, [(v, a, Term v a, Type v a)])] ->
  TypecheckedUnisonFile v a
typecheckedUnisonFile datas effects tlcs watches =
  TypecheckedUnisonFileId datas effects tlcs watches hashImpl
  where
    hashImpl :: (Map v (a, Reference.Id, Maybe WatchKind, Term v a, Type v a))
    hashImpl =
      let -- includes watches
          allTerms :: [(v, a, Term v a, Type v a)]
          allTerms = join tlcs ++ join (snd <$> watches)
          types :: Map v (Type v a)
          types = Map.fromList [(v, t) | (v, _a, _, t) <- allTerms]
          watchKinds :: Map v (Maybe WatchKind)
          watchKinds =
            Map.fromList $
              [(v, Nothing) | (v, _a, _e, _t) <- join tlcs]
                ++ [(v, Just wk) | (wk, wkTerms) <- watches, (v, _a, _e, _t) <- wkTerms]
          hcs :: Map v (Reference.Id, Term v a, Type v a, a)
          hcs = Hashing.hashTermComponents $ Map.fromList $ (\(v, a, e, t) -> (v, (e, t, a))) <$> allTerms
       in Map.fromList
            [ (v, (a, r, wk, e, t))
              | (v, (r, e, _typ, a)) <- Map.toList hcs,
                Just t <- [Map.lookup v types],
                wk <- [Map.findWithDefault (error $ show v ++ " missing from watchKinds") v watchKinds]
            ]

lookupDecl ::
  (Ord v) =>
  v ->
  TypecheckedUnisonFile v a ->
  Maybe (Reference.Id, DD.Decl v a)
lookupDecl v uf =
  over _2 Right <$> (Map.lookup v (dataDeclarationsId' uf))
    <|> over _2 Left <$> (Map.lookup v (effectDeclarationsId' uf))

indexByReference ::
  TypecheckedUnisonFile v a ->
  (Map Reference.Id (a, Term v a, Type v a), Map Reference.Id (DD.Decl v a))
indexByReference uf = (tms, tys)
  where
    tys =
      Map.fromList (over _2 Right <$> toList (dataDeclarationsId' uf))
        <> Map.fromList (over _2 Left <$> toList (effectDeclarationsId' uf))
    tms =
      Map.fromList
        [ (r, (a, tm, ty)) | (a, Reference.DerivedId r, _wk, tm, ty) <- Map.elems (hashTerms uf)
        ]

-- | A mapping of all terms in the file by their var name.
-- The returned terms refer to other definitions in the file by their
-- var, not by reference.
-- Includes test watches.
allTerms :: (Ord v) => TypecheckedUnisonFile v a -> Map v (Term v a)
allTerms uf =
  Map.fromList [(v, t) | (v, _a, t, _) <- join $ topLevelComponents uf]

-- | the top level components (no watches) plus test watches.
topLevelComponents ::
  TypecheckedUnisonFile v a ->
  [[(v, a, Term v a, Type v a)]]
topLevelComponents file =
  topLevelComponents' file ++ [comp | (TestWatch, comp) <- watchComponents file]

-- External type references that appear in the types of the file's terms
termSignatureExternalLabeledDependencies ::
  (Ord v) => TypecheckedUnisonFile v a -> Set LabeledDependency
termSignatureExternalLabeledDependencies
  (TypecheckedUnisonFile dataDeclarations' effectDeclarations' _ _ hashTerms) =
    Set.difference
      ( Set.map LD.typeRef
          . foldMap Type.dependencies
          . fmap (\(_a, _r, _wk, _e, t) -> t)
          . toList
          $ hashTerms
      )
      -- exclude any references that are defined in this file
      ( Set.fromList $
          (map (LD.typeRef . fst) . toList) dataDeclarations'
            <> (map (LD.typeRef . fst) . toList) effectDeclarations'
      )

-- Returns the dependencies of the `UnisonFile` input. Needed so we can
-- load information about these dependencies before starting typechecking.
dependencies :: (Monoid a, Var v) => UnisonFile v a -> DefnsF Set TermReference TypeReference
dependencies file =
  fold
    [ Defns
        { terms = Set.empty,
          types =
            Set.unions
              [ foldMap (DD.typeDependencies . snd) file.dataDeclarationsId,
                foldMap (DD.typeDependencies . DD.toDataDecl . snd) file.effectDeclarationsId
              ]
        },
      foldMap (Term.dependencies . snd) file.terms,
      foldMap (foldMap (Term.dependencies . view _3)) file.watches
    ]

discardTypes :: (Ord v) => TypecheckedUnisonFile v a -> UnisonFile v a
discardTypes (TypecheckedUnisonFileId datas effects terms watches _) =
  let watches' = g . mconcat <$> List.multimap watches
      g tup3s = [(v, a, e) | (v, a, e, _t) <- tup3s]
   in UnisonFileId (coerce datas) (coerce effects) (Map.fromList [(v, (a, trm)) | (v, a, trm, _typ) <- join terms]) watches'

declsToTypeLookup :: (Var v) => UnisonFile v a -> TL.TypeLookup v a
declsToTypeLookup uf =
  TL.TypeLookup
    mempty
    (wrangle (dataDeclarations uf))
    (wrangle (effectDeclarations uf))
  where
    wrangle = Map.fromList . Map.elems

typecheckedToTypeLookup :: TypecheckedUnisonFile v a -> TL.TypeLookup v a
typecheckedToTypeLookup tuf =
  TL.TypeLookup
    mempty
    (wrangle (dataDeclarations' tuf))
    (wrangle (effectDeclarations' tuf))
  where
    wrangle = Map.fromList . Map.elems

-- Returns true if the file has any definitions or watches
nonEmpty :: TypecheckedUnisonFile v a -> Bool
nonEmpty uf =
  not (Map.null (dataDeclarations' uf))
    || not (Map.null (effectDeclarations' uf))
    || any (not . null) (topLevelComponents' uf)
    || any (not . null) (watchComponents uf)

hashConstructors ::
  forall v a. (Ord v) => TypecheckedUnisonFile v a -> Map v Referent.Id
hashConstructors file =
  let ctors1 =
        Map.elems (dataDeclarationsId' file) >>= \(ref, dd) ->
          [(v, Referent.ConId (ConstructorReference ref i) CT.Data) | (v, i) <- DD.constructorVars dd `zip` [0 ..]]
      ctors2 =
        Map.elems (effectDeclarationsId' file) >>= \(ref, dd) ->
          [(v, Referent.ConId (ConstructorReference ref i) CT.Effect) | (v, i) <- DD.constructorVars (DD.toDataDecl dd) `zip` [0 ..]]
   in Map.fromList (ctors1 ++ ctors2)

-- | Returns the set of constructor names for decls whose names in the given Set.
constructorsForDecls :: (Ord v) => Set v -> TypecheckedUnisonFile v a -> Set v
constructorsForDecls types uf =
  let dataConstructors =
        dataDeclarationsId' uf
          & Map.filterWithKey (\k _ -> Set.member k types)
          & Map.elems
          & fmap snd
          & concatMap DD.constructorVars
      effectConstructors =
        effectDeclarationsId' uf
          & Map.filterWithKey (\k _ -> Set.member k types)
          & Map.elems
          & fmap (DD.toDataDecl . snd)
          & concatMap DD.constructorVars
   in Set.fromList (dataConstructors <> effectConstructors)

-- | All bindings in the term namespace: terms, test watches (since those are the only watches that are actually stored
-- in the codebase), data constructors, and effect constructors.
termNamespaceBindings :: (Ord v) => TypecheckedUnisonFile v a -> Set v
termNamespaceBindings uf =
  terms <> tests <> datacons <> effcons
  where
    terms = foldMap (Set.fromList . map (view _1)) uf.topLevelComponents'
    tests =
      uf.watchComponents & foldMap \case
        (WatchKind.TestWatch, watches) -> Set.fromList (map (view _1) watches)
        _ -> Set.empty
    datacons = foldMap (Set.fromList . DataDeclaration.constructorVars . view _2) uf.dataDeclarationsId'
    effcons =
      foldMap
        (Set.fromList . DataDeclaration.constructorVars . DataDeclaration.toDataDecl . view _2)
        uf.effectDeclarationsId'

-- | All bindings in the term namespace: data declarations and effect declarations.
typeNamespaceBindings :: (Ord v) => TypecheckedUnisonFile v a -> Set v
typeNamespaceBindings uf =
  datas <> effs
  where
    datas = Map.keysSet uf.dataDeclarationsId'
    effs = Map.keysSet uf.effectDeclarationsId'
