module U.Codebase.Decl where

import Control.Lens hiding (List)
import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (Reference')
import U.Codebase.Reference qualified as Reference
import U.Codebase.Type (TypeR)
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import U.Core.ABT.Var qualified as ABT
import Unison.Hash (Hash)
import Unison.Prelude
import Unison.Util.Recursion

type ConstructorId = Word64

data DeclType = Data | Effect
  deriving (Eq, Ord, Show, Enum)

type Decl v = DeclR TypeRef v

type HashableDecl v = DeclR HashableTypeRef v

type TypeRef = Reference' Text (Maybe Hash)

type HashableTypeRef = Reference' Text Hash

type Type v = TypeR TypeRef v

type HashableType v = TypeR HashableTypeRef v

data Modifier = Structural | Unique Text
  deriving (Eq, Ord, Show)

data DeclR r v = DataDeclaration
  { declType :: DeclType,
    modifier :: Modifier,
    bound :: [v],
    constructorTypes :: [TypeR r v]
  }
  deriving (Show)

allVars :: (Ord v) => DeclR r v -> Set v
allVars (DataDeclaration _ _ bound constructorTypes) =
  (Set.fromList $ foldMap ABT.allVars constructorTypes) <> Set.fromList bound

vmap :: (Ord v') => (v -> v') -> DeclR r v -> DeclR r v'
vmap f (DataDeclaration {declType, modifier, bound, constructorTypes}) =
  DataDeclaration
    { declType,
      modifier,
      bound = f <$> bound,
      constructorTypes = ABT.vmap f <$> constructorTypes
    }

rmap :: (Ord v) => (r -> r') -> DeclR r v -> DeclR r' v
rmap f (DataDeclaration {declType, modifier, bound, constructorTypes}) =
  DataDeclaration
    { declType,
      modifier,
      bound,
      constructorTypes = Type.rmap f <$> constructorTypes
    }

-- * Hashing stuff

dependencies :: (Ord r, Ord v) => DeclR r v -> Set r
dependencies (DataDeclaration _ _ _ cts) = foldMap Type.dependencies cts

data V v = Bound v | Ctor Int

data F a
  = Type (Type.FD a)
  | LetRec [a] a
  | Constructors [a]
  | Modified DeclType Modifier a
  deriving (Functor, Foldable, Show)

-- | Given the pieces of a single decl component,
-- replaces all 'Nothing' self-referential hashes with a variable reference
-- to the relevant piece of the component in the component map.
unhashComponent ::
  forall v extra.
  (ABT.Var v) =>
  Hash ->
  -- | A function to convert a reference to a variable. The actual var names aren't important.
  (Reference.Id -> v) ->
  -- A SINGLE decl component. Self references should have a 'Nothing' hash in term
  -- references/term links
  Map Reference.Id (Decl v, extra) ->
  -- | The component with all self-references replaced with variable references.
  Map Reference.Id (v, HashableDecl v, extra)
unhashComponent componentHash refToVar m =
  withGeneratedVars
    & traversed . _2 %~ fillSelfReferences
  where
    usedVars :: Set v
    usedVars = foldMapOf (folded . _1) allVars m
    withGeneratedVars :: Map Reference.Id (v, Decl v, extra)
    withGeneratedVars = evalState (Map.traverseWithKey assignVar m) usedVars
    assignVar :: Reference.Id -> (trm, extra) -> StateT (Set v) Identity (v, trm, extra)
    assignVar r (trm, extra) = (,trm,extra) <$> ABT.freshenS (refToVar r)
    fillSelfReferences :: Decl v -> HashableDecl v
    fillSelfReferences DataDeclaration {declType, modifier, bound, constructorTypes} =
      DataDeclaration
        { declType,
          modifier,
          bound,
          constructorTypes = cata alg <$> constructorTypes
        }
      where
        rewriteTypeReference :: Reference.Id' (Maybe Hash) -> Either v Reference.Reference
        rewriteTypeReference rid@(Reference.Id mayH pos) =
          case mayH of
            Just h ->
              case Map.lookup (Reference.Id h pos) withGeneratedVars of
                -- No entry in the component map, so this is NOT a self-reference, keep it but
                -- replace the 'Maybe Hash' with a 'Hash'.
                Nothing -> Right (Reference.ReferenceDerived (Reference.Id h pos))
                -- Entry in the component map, so this is a self-reference, replace it with a
                -- Var.
                Just (v, _, _) -> Left v
            Nothing ->
              -- This is a self-reference, so we expect to find it in the component map.
              case Map.lookup (fromMaybe componentHash <$> rid) withGeneratedVars of
                Nothing -> error "unhashComponent: self-reference not found in component map"
                Just (v, _, _) -> Left v
        alg :: ABT.Term' (Type.F' TypeRef) v () (HashableType v) -> HashableType v
        alg (ABT.Term' _ () abt) = case abt of
          ABT.Var v -> ABT.var () v
          ABT.Cycle body -> ABT.cycle () body
          ABT.Abs v body -> ABT.abs () v body
          ABT.Tm t -> case t of
            Type.Ref (Reference.ReferenceDerived rid) ->
              rewriteTypeReference rid
                & either (ABT.var ()) (ABT.tm () . Type.Ref)
            Type.Ref (Reference.ReferenceBuiltin t) ->
              ABT.tm () $ Type.Ref (Reference.ReferenceBuiltin t)
            Type.Arrow a b -> ABT.tm () $ Type.Arrow a b
            Type.Ann a k -> ABT.tm () $ Type.Ann a k
            Type.App a b -> ABT.tm () $ Type.App a b
            Type.Effect a b -> ABT.tm () $ Type.Effect a b
            Type.Effects as -> ABT.tm () $ Type.Effects as
            Type.Forall a -> ABT.tm () $ Type.Forall a
            Type.IntroOuter a -> ABT.tm () $ Type.IntroOuter a
