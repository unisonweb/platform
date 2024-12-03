module Unison.Hashing.V2.Type
  ( Type,
    TypeF (..),
    bindExternal,
    bindReferences,

    -- * find by type index stuff
    typeToReference,
    typeToReferenceMentions,

    -- * builtin term references
    booleanRef,
    charRef,
    effectRef,
    floatRef,
    intRef,
    listRef,
    natRef,
    textRef,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.HashQualified qualified as HQ
import Unison.Hashing.V2.ABT qualified as ABT
import Unison.Hashing.V2.Kind qualified as K
import Unison.Hashing.V2.Reference (Reference (..), pattern ReferenceDerived)
import Unison.Hashing.V2.Tokenizable (Hashable1)
import Unison.Hashing.V2.Tokenizable qualified as Hashable
import Unison.Name qualified as Name
import Unison.Names.ResolutionResult qualified as Names
import Unison.Prelude
import Unison.Util.List qualified as List
import Unison.Var (Var)

-- | Base functor for types in the Unison language
data TypeF a
  = TypeRef Reference
  | TypeArrow a a
  | TypeAnn a K.Kind
  | TypeApp a a
  | TypeEffect a a
  | TypeEffects [a]
  | TypeForall a
  | TypeIntroOuter a -- binder like ∀, used to introduce variables that are
  -- bound by outer type signatures, to support scoped type
  -- variables
  deriving (Foldable, Functor, Traversable)

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v a = ABT.Term TypeF v a

freeVars :: Type v a -> Set v
freeVars = ABT.freeVars

bindExternal ::
  (ABT.Var v) => [(v, Reference)] -> Type v a -> Type v a
bindExternal bs = ABT.substsInheritAnnotation [(v, ref () r) | (v, r) <- bs]

bindReferences ::
  (Var v) =>
  (v -> Name.Name) ->
  Set v ->
  Map Name.Name Reference ->
  Type v a ->
  Names.ResolutionResult a (Type v a)
bindReferences unsafeVarToName keepFree ns t =
  let fvs = ABT.freeVarOccurrences keepFree t
      rs = [(v, a, Map.lookup (unsafeVarToName v) ns) | (v, a) <- fvs]
      ok (v, _a, Just r) = pure (v, r)
      ok (v, a, Nothing) = Left (pure (Names.TypeResolutionFailure (HQ.NameOnly (unsafeVarToName v)) a Names.NotFound))
   in List.validate ok rs <&> \es -> bindExternal es t

-- some smart patterns
pattern TypeRef' :: Reference -> ABT.Term TypeF v a
pattern TypeRef' r <- ABT.Tm' (TypeRef r)

pattern ForallsNamed' :: [v] -> Type v a -> Type v a
pattern ForallsNamed' vs body <- (unForalls -> Just (vs, body))

pattern ForallNamed' :: v -> ABT.Term TypeF v a -> ABT.Term TypeF v a
pattern ForallNamed' v body <- ABT.Tm' (TypeForall (ABT.out -> ABT.Abs v body))

unForalls :: Type v a -> Maybe ([v], Type v a)
unForalls t = go t []
  where
    go (ForallNamed' v body) vs = go body (v : vs)
    go _body [] = Nothing
    go body vs = Just (reverse vs, body)

-- some smart constructors
ref :: (Ord v) => a -> Reference -> Type v a
ref a = ABT.tm' a . TypeRef

intRef, natRef, floatRef, booleanRef, textRef, charRef, listRef, effectRef :: Reference
intRef = ReferenceBuiltin "Int"
natRef = ReferenceBuiltin "Nat"
floatRef = ReferenceBuiltin "Float"
booleanRef = ReferenceBuiltin "Boolean"
textRef = ReferenceBuiltin "Text"
charRef = ReferenceBuiltin "Char"
listRef = ReferenceBuiltin "Sequence"
effectRef = ReferenceBuiltin "Effect"

forAll :: (Ord v) => a -> v -> Type v a -> Type v a
forAll a v body = ABT.tm' a (TypeForall (ABT.abs' a v body))

-- | Bind the given variables with an outer `forall`, if they are used in `t`.
generalize :: (Ord v) => [v] -> Type v a -> Type v a
generalize vs t = foldr f t vs
  where
    f v t =
      if Set.member v (ABT.freeVars t) then forAll (ABT.annotation t) v t else t

unforall' :: Type v a -> ([v], Type v a)
unforall' (ForallsNamed' vs t) = (vs, t)
unforall' t = ([], t)

typeToReference :: (Ord v, Show v) => Type v a -> Reference
typeToReference (TypeRef' r) = r
-- a bit of normalization - any unused type parameters aren't part of the hash
typeToReference (ForallNamed' v body) | not (Set.member v (ABT.freeVars body)) = typeToReference body
typeToReference t = ReferenceDerived (ABT.hash t) 0

typeToReferenceMentions :: (Ord v, Show v) => Type v a -> Set Reference
typeToReferenceMentions ty =
  let (vs, _) = unforall' ty
      gen ty = generalize (Set.toList (freeVars ty)) $ generalize vs ty
   in Set.fromList $ typeToReference . gen <$> ABT.subterms ty

instance Hashable1 TypeF where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
     in -- Note: start each layer with leading `0` byte, to avoid collisions with
        -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
        Hashable.accumulate $
          tag 0 : case e of
            TypeRef r -> [tag 0, Hashable.accumulateToken r]
            TypeArrow a b -> [tag 1, hashed (hash a), hashed (hash b)]
            TypeApp a b -> [tag 2, hashed (hash a), hashed (hash b)]
            TypeAnn a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k]
            -- Example:
            --   a) {Remote, Abort} (() -> {Remote} ()) should hash the same as
            --   b) {Abort, Remote} (() -> {Remote} ()) but should hash differently from
            --   c) {Remote, Abort} (() -> {Abort} ())
            TypeEffects es ->
              let (hs, _) = hashCycle es
               in tag 4 : map hashed hs
            TypeEffect e t -> [tag 5, hashed (hash e), hashed (hash t)]
            TypeForall a -> [tag 6, hashed (hash a)]
            TypeIntroOuter a -> [tag 7, hashed (hash a)]
