{-# LANGUAGE RecordWildCards #-}

module Unison.DataDeclaration
  ( DataDeclaration (..),
    EffectDeclaration (..),
    Decl,
    DeclOrBuiltin (..),
    Modifier (..),
    allVars,
    asDataDecl,
    bindReferences,
    constructorCount,
    constructorNames,
    constructors,
    constructorType,
    constructorTypes,
    constructorVars,
    constructorIds,
    declConstructorReferents,
    declTypeDependencies,
    labeledDeclTypeDependencies,
    labeledDeclDependenciesIncludingSelf,
    declFields,
    typeDependencies,
    labeledTypeDependencies,
    unhashComponent,
    mkDataDecl',
    mkEffectDecl',
    typeOfConstructor,
    withEffectDeclM,
    amap,
    updateDependencies,
    constructors_,
    asDataDecl_,
    declAsDataDecl_,
    setConstructorNames,
  )
where

import Control.Lens (Iso', Lens', imap, iso, lens, _2, _3)
import Control.Monad.State (evalState)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.LabeledDependency qualified as LD
import Unison.Name qualified as Name
import Unison.Names.ResolutionResult qualified as Names
import Unison.Prelude
import Unison.Reference (Reference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (cycle)

type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data DeclOrBuiltin v a
  = Builtin CT.ConstructorType
  | Decl (Decl v a)
  deriving (Eq, Ord, Show)

asDataDecl :: Decl v a -> DataDeclaration v a
asDataDecl = either toDataDecl id

declTypeDependencies :: (Ord v) => Decl v a -> Set Reference
declTypeDependencies = either (typeDependencies . toDataDecl) typeDependencies

labeledDeclTypeDependencies :: (Ord v) => Decl v a -> Set LD.LabeledDependency
labeledDeclTypeDependencies = Set.map LD.TypeReference . declTypeDependencies

-- | Compute the dependencies of a data declaration,
-- including the type itself and references for each of its constructors.
--
-- NOTE: You may prefer labeledDeclDependenciesIncludingSelfAndFieldAccessors in
-- Unison.DataDeclaration.Dependencies, it also includes Referents for accessors of record
-- fields.
labeledDeclDependenciesIncludingSelf :: (Ord v) => Reference.TypeReference -> Decl v a -> Set LD.LabeledDependency
labeledDeclDependenciesIncludingSelf selfRef decl =
  labeledDeclTypeDependencies decl <> (Set.singleton $ LD.TypeReference selfRef) <> labeledConstructorRefs
  where
    labeledConstructorRefs :: Set LD.LabeledDependency
    labeledConstructorRefs =
      case selfRef of
        Reference.Builtin {} -> mempty
        Reference.DerivedId selfRefId ->
          declConstructorReferents selfRefId decl
            & fmap (LD.TermReferent . fmap Reference.DerivedId)
            & Set.fromList

constructorType :: Decl v a -> CT.ConstructorType
constructorType = \case
  Left {} -> CT.Effect
  Right {} -> CT.Data

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DataDeclaration v a = DataDeclaration
  { modifier :: Modifier,
    annotation :: a,
    bound :: [v],
    constructors' :: [(a, v, Type v a)]
  }
  deriving (Eq, Ord, Show, Functor, Generic)

constructorCount :: DataDeclaration v a -> Int
constructorCount DataDeclaration {constructors'} = length constructors'

constructors_ :: Lens' (DataDeclaration v a) [(a, v, Type v a)]
constructors_ = lens getter setter
  where
    getter = constructors'
    setter dd ctors = dd {constructors' = ctors}

newtype EffectDeclaration v a = EffectDeclaration
  { toDataDecl :: DataDeclaration v a
  }
  deriving (Eq, Ord, Show, Functor)

declAsDataDecl_ :: Lens' (Decl v a) (DataDeclaration v a)
declAsDataDecl_ = lens get set
  where
    get (Left ed) = toDataDecl ed
    get (Right dd) = dd
    set decl dd = bimap (EffectDeclaration . const dd) (const dd) decl

asDataDecl_ :: Iso' (EffectDeclaration v a) (DataDeclaration v a)
asDataDecl_ = iso toDataDecl EffectDeclaration

withEffectDeclM ::
  (Functor f) =>
  (DataDeclaration v a -> f (DataDeclaration v' a')) ->
  EffectDeclaration v a ->
  f (EffectDeclaration v' a')
withEffectDeclM f = fmap EffectDeclaration . f . toDataDecl

constructorTypes :: DataDeclaration v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

-- what is declFields? —AI
declFields :: (Var v) => Decl v a -> Either [Int] [Int]
declFields = bimap cf cf . first toDataDecl
  where
    cf = fmap fields . constructorTypes
    fields (Type.ForallsNamed' _ ty) = fields ty
    fields (Type.Arrows' spine) = length spine - 1
    fields _ = 0

typeOfConstructor :: DataDeclaration v a -> ConstructorId -> Maybe (Type v a)
typeOfConstructor dd i = constructorTypes dd `atMay` fromIntegral i

constructors :: DataDeclaration v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v, t) | (_, v, t) <- ctors]

constructorVars :: DataDeclaration v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: (Var v) => DataDeclaration v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

-- | Overwrite the constructor names with the given list, given in canonical order, which is assumed to be of the
-- correct length.
--
-- Presumably this is called because the decl was loaded from the database outside of the context of a namespace,
-- since it's not stored with names there, so we had plugged in dummy names like "Constructor1", "Constructor2", ...
--
-- Then, at some point, we discover the constructors' names in a namespace, and now we'd like to combine the two
-- together to get a Decl structure in memory with good/correct names for constructors.
setConstructorNames :: [v] -> Decl v a -> Decl v a
setConstructorNames constructorNames =
  over
    (declAsDataDecl_ . constructors_)
    (zipWith (set _2) constructorNames)

-- This function is unsound, since the `rid` and the `decl` have to match.
-- It should probably be hashed directly from the Decl, once we have a
-- reliable way of doing that. —AI
declConstructorReferents :: Reference.TypeReferenceId -> Decl v a -> [Referent.Id]
declConstructorReferents rid decl =
  [Referent'.Con' (ConstructorReference rid i) ct | i <- constructorIds (asDataDecl decl)]
  where
    ct = constructorType decl

-- | The constructor ids for the given data declaration.
constructorIds :: DataDeclaration v a -> [ConstructorId]
constructorIds dd =
  imap (\i _ -> fromIntegral i) (constructorTypes dd)

-- | All variables mentioned in the given data declaration.
-- Includes both term and type variables, both free and bound.
allVars :: (Ord v) => DataDeclaration v a -> Set v
allVars (DataDeclaration _ _ bound ctors) =
  Set.unions $
    Set.fromList bound : [Set.insert v (Set.fromList $ ABT.allVars tp) | (_, v, tp) <- ctors]

-- | All variables mentioned in the given declaration.
-- Includes both term and type variables, both free and bound.
allVars' :: (Ord v) => Decl v a -> Set v
allVars' = allVars . either toDataDecl id

bindReferences ::
  (Var v) =>
  (v -> Name.Name) ->
  Set v ->
  Map Name.Name Reference ->
  DataDeclaration v a ->
  Names.ResolutionResult a (DataDeclaration v a)
bindReferences unsafeVarToName keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.bindReferences unsafeVarToName keepFree names ty
  pure $ DataDeclaration m a bound constructors

-- | All references to types mentioned in the given data declaration's fields/constructors
-- Note: Does not include references to the constructors or the decl itself
-- (unless the decl is self-referential)
-- Note: Does NOT include the referents for fields and field accessors.
-- Those must be computed separately because we need access to the typechecker to do so.
typeDependencies :: (Ord v) => DataDeclaration v a -> Set TypeReference
typeDependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

labeledTypeDependencies :: (Ord v) => DataDeclaration v a -> Set LD.LabeledDependency
labeledTypeDependencies = Set.map LD.TypeReference . typeDependencies

mkEffectDecl' ::
  Modifier -> a -> [v] -> [(a, v, Type v a)] -> EffectDeclaration v a
mkEffectDecl' m a b cs = EffectDeclaration (DataDeclaration m a b cs)

mkDataDecl' ::
  Modifier -> a -> [v] -> [(a, v, Type v a)] -> DataDeclaration v a
mkDataDecl' = DataDeclaration

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable, Show)

updateDependencies :: (Ord v) => Map Reference Reference -> Decl v a -> Decl v a
updateDependencies typeUpdates decl =
  back $
    dataDecl
      { constructors' =
          over _3 (Type.updateDependencies typeUpdates)
            <$> constructors' dataDecl
      }
  where
    dataDecl = either toDataDecl id decl
    back = either (const $ Left . EffectDeclaration) (const Right) decl

-- | This converts `Reference`s it finds that are in the input `Map`
-- back to free variables.
--
-- In the result map, any of the references inside the Decls which are keys of the input map;
-- have been replaced with the corresponding output `v`s in the output `Decl`s,
-- which are fresh with respect to all input Decls.
unhashComponent ::
  forall v a. (Var v) => Map Reference.Id (Decl v a) -> Map Reference.Id (v, Decl v a)
unhashComponent m =
  let usedVars :: Set v
      usedVars = foldMap allVars' m
      -- We assign fresh names to each reference/decl pair.
      -- We haven't modified the decls yet, but we will, further below.
      m' :: Map Reference.Id (v, Decl v a)
      m' = evalState (Map.traverseWithKey assignVar m) usedVars
        where
          assignVar r d = (,d) <$> ABT.freshenS (Var.unnamedRef r)
      unhash1 :: ABT.Term Type.F v a -> ABT.Term Type.F v a
      unhash1 = ABT.rebuildUp' go
        where
          go e@(Type.Ref' (Reference.DerivedId r)) = case Map.lookup r m' of
            Nothing -> e
            Just (v, _) -> Type.var (ABT.annotation e) v
          go e = e
      unhash2 (Right dd@DataDeclaration {}) = Right $ unhash3 dd
      unhash2 (Left (EffectDeclaration dd)) =
        Left . EffectDeclaration $ unhash3 dd
      unhash3 dd@DataDeclaration {..} =
        dd {constructors' = fmap (over _3 unhash1) constructors'}
   in second unhash2 <$> m'

amap :: (a -> a2) -> Decl v a -> Decl v a2
amap f (Left e) = Left (f <$> e)
amap f (Right d) = Right (f <$> d)
