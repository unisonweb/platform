{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type where

import Unison.Prelude

import qualified Control.Monad.Writer.Strict as Writer
import Data.Functor.Identity (runIdentity)
import Data.Functor.Const (Const(..), getConst)
import Data.Monoid (Any(..))
import           Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude.Extras (Eq1(..),Show1(..),Ord1(..))
import qualified Unison.ABT as ABT
import           Unison.Hashable (Hashable1)
import qualified Unison.Hashable as Hashable
import qualified Unison.Kind as K
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Reference.Util as ReferenceUtil
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Settings as Settings
import qualified Unison.Util.Relation as R
import qualified Unison.Names3 as Names
import qualified Unison.Name as Name
import qualified Unison.Util.List as List

-- | Base functor for types in the Unison language
data F a
  = Ref Reference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a -- binder like ∀, used to introduce variables that are
                 -- bound by outer type signatures, to support scoped type
                 -- variables
  deriving (Foldable,Functor,Generic,Generic1,Eq,Ord,Traversable)

instance Eq1 F where (==#) = (==)
instance Ord1 F where compare1 = compare
instance Show1 F where showsPrec1 = showsPrec

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v a = ABT.Term F v a

wrapV :: Ord v => Type v a -> Type (ABT.V v) a
wrapV = ABT.vmap ABT.Bound

freeVars :: Type v a -> Set v
freeVars = ABT.freeVars

bindExternal
  :: ABT.Var v => [(v, Reference)] -> Type v a -> Type v a
bindExternal bs = ABT.substsInheritAnnotation [ (v, ref () r) | (v, r) <- bs ]

bindNames
  :: Var v
  => Set v
  -> Names.Names0
  -> Type v a
  -> Names.ResolutionResult v a (Type v a)
bindNames keepFree ns t = let
  fvs = ABT.freeVarOccurrences keepFree t
  rs = [(v, a, R.lookupDom (Name.fromVar v) (Names.types0 ns)) | (v,a) <- fvs ]
  ok (v, a, rs) = if Set.size rs == 1 then pure (v, Set.findMin rs)
                  else Left (pure (Names.TypeResolutionFailure v a rs))
  in List.validate ok rs <&> \es -> bindExternal es t

newtype Monotype v a = Monotype { getPolytype :: Type v a } deriving Eq

instance (Show v) => Show (Monotype v a) where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: ABT.Var v => Type v a -> Maybe (Monotype v a)
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _) = Just Nothing
  isMono _ = Nothing

arity :: Type v a -> Int
arity (ForallNamed' _ body) = arity body
arity (Arrow' _ o) = 1 + arity o
arity (Ann' a _) = arity a
arity _ = 0

-- some smart patterns
pattern Ref' r <- ABT.Tm' (Ref r)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Arrows' spine <- (unArrows -> Just spine)
pattern EffectfulArrows' fst rest <- (unEffectfulArrows -> Just (fst, rest))
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Apps' f args <- (unApps -> Just (f, args))
pattern Pure' t <- (unPure -> Just t)
pattern Effects' es <- ABT.Tm' (Effects es)
-- Effect1' must match at least one effect
pattern Effect1' e t <- ABT.Tm' (Effect e t)
pattern Effect' es t <- (unEffects1 -> Just (es, t))
pattern Effect'' es t <- (unEffect0 -> (es, t))
-- Effect0' may match zero effects
pattern Effect0' es t <- (unEffect0 -> (es, t))
pattern Forall' subst <- ABT.Tm' (Forall (ABT.Abs' subst))
pattern IntroOuter' subst <- ABT.Tm' (IntroOuter (ABT.Abs' subst))
pattern IntroOuterNamed' v body <- ABT.Tm' (IntroOuter (ABT.out -> ABT.Abs v body))
pattern ForallsNamed' vs body <- (unForalls -> Just (vs, body))
pattern ForallNamed' v body <- ABT.Tm' (Forall (ABT.out -> ABT.Abs v body))
pattern Var' v <- ABT.Var' v
pattern Cycle' xs t <- ABT.Cycle' xs t
pattern Abs' subst <- ABT.Abs' subst

unPure :: Ord v => Type v a -> Maybe (Type v a)
unPure (Effect'' [] t) = Just t
unPure (Effect'' _ _) = Nothing
unPure t = Just t

unArrows :: Type v a -> Maybe [Type v a]
unArrows t =
  case go t of [_] -> Nothing; l -> Just l
  where go (Arrow' i o) = i : go o
        go o = [o]

unEffectfulArrows
  :: Type v a -> Maybe (Type v a, [(Maybe [Type v a], Type v a)])
unEffectfulArrows t = case t of
  Arrow' i o -> Just (i, go o)
  _          -> Nothing
 where
  go (Effect1' (Effects' es) (Arrow' i o)) =
    (Just $ es >>= flattenEffects, i) : go o
  go (Effect1' (Effects' es) t) = [(Just $ es >>= flattenEffects, t)]
  go (Arrow'   i             o) = (Nothing, i) : go o
  go t                          = [(Nothing, t)]

unApps :: Type v a -> Maybe (Type v a, [Type v a])
unApps t = case go t [] of
  []       -> Nothing
  [ _ ]    -> Nothing
  f : args -> Just (f, args)
 where
  go (App' i o) acc  = go i (o : acc)
  go fn         args = fn : args

unIntroOuters :: Type v a -> Maybe ([v], Type v a)
unIntroOuters t = go t []
  where go (IntroOuterNamed' v body) vs = go body (v:vs)
        go _body [] = Nothing
        go body vs = Just (reverse vs, body)

-- Most code doesn't care about `introOuter` binders and is fine dealing with the
-- these outer variable references as free variables. This function strips out
-- one or more `introOuter` binders, so `outer a b . (a, b)` becomes `(a, b)`.
stripIntroOuters :: Type v a -> Type v a
stripIntroOuters t = case unIntroOuters t of
  Just (_, t) -> t
  Nothing     -> t

unForalls :: Type v a -> Maybe ([v], Type v a)
unForalls t = go t []
  where go (ForallNamed' v body) vs = go body (v:vs)
        go _body [] = Nothing
        go body vs = Just(reverse vs, body)

unEffect0 :: Ord v => Type v a -> ([Type v a], Type v a)
unEffect0 (Effect1' e a) = (flattenEffects e, a)
unEffect0 t              = ([], t)

unEffects1 :: Ord v => Type v a -> Maybe ([Type v a], Type v a)
unEffects1 (Effect1' (Effects' es) a) = Just (es, a)
unEffects1 _                          = Nothing

-- | True if the given type is a function, possibly quantified
isArrow :: ABT.Var v => Type v a -> Bool
isArrow (ForallNamed' _ t) = isArrow t
isArrow (Arrow' _ _) = True
isArrow _ = False

-- some smart constructors

--vectorOf :: Ord v => a -> Type v a -> Type v
--vectorOf a t = vector `app` t

ref :: Ord v => a -> Reference -> Type v a
ref a = ABT.tm' a . Ref

termLink :: Ord v => a -> Type v a
termLink a = ABT.tm' a . Ref $ termLinkRef

typeLink :: Ord v => a -> Type v a
typeLink a = ABT.tm' a . Ref $ typeLinkRef

derivedBase32Hex :: Ord v => Reference -> a -> Type v a
derivedBase32Hex r a = ref a r

-- derivedBase58' :: Text -> Reference
-- derivedBase58' base58 = Reference.derivedBase58 base58 0 1

intRef, natRef, floatRef, booleanRef, textRef, charRef, vectorRef, bytesRef, effectRef, termLinkRef, typeLinkRef :: Reference
intRef = Reference.Builtin "Int"
natRef = Reference.Builtin "Nat"
floatRef = Reference.Builtin "Float"
booleanRef = Reference.Builtin "Boolean"
textRef = Reference.Builtin "Text"
charRef = Reference.Builtin "Char"
vectorRef = Reference.Builtin "Sequence"
bytesRef = Reference.Builtin "Bytes"
effectRef = Reference.Builtin "Effect"
termLinkRef = Reference.Builtin "Link.Term"
typeLinkRef = Reference.Builtin "Link.Type"

builtin :: Ord v => a -> Text -> Type v a
builtin a = ref a . Reference.Builtin

int :: Ord v => a -> Type v a
int a = ref a intRef

nat :: Ord v => a -> Type v a
nat a = ref a natRef

float :: Ord v => a -> Type v a
float a = ref a floatRef

boolean :: Ord v => a -> Type v a
boolean a = ref a booleanRef

text :: Ord v => a -> Type v a
text a = ref a textRef

char :: Ord v => a -> Type v a
char a = ref a charRef

vector :: Ord v => a -> Type v a
vector a = ref a vectorRef

bytes :: Ord v => a -> Type v a
bytes a = ref a bytesRef

effectType :: Ord v => a -> Type v a
effectType a = ref a effectRef

app :: Ord v => a -> Type v a -> Type v a -> Type v a
app a f arg = ABT.tm' a (App f arg)

-- `f x y z` means `((f x) y) z` and the annotation paired with `y` is the one
-- meant for `app (f x) y`
apps :: Ord v => Type v a -> [(a, Type v a)] -> Type v a
apps = foldl' go where go f (a, t) = app a f t

app' :: (Ord v, Semigroup a) => Type v a -> Type v a -> Type v a
app' f arg = app (ABT.annotation f <> ABT.annotation arg) f arg

apps' :: (Semigroup a, Ord v) => Type v a -> [Type v a] -> Type v a
apps' = foldl app'

arrow :: Ord v => a -> Type v a -> Type v a -> Type v a
arrow a i o = ABT.tm' a (Arrow i o)

arrow' :: (Semigroup a, Ord v) => Type v a -> Type v a -> Type v a
arrow' i o = arrow (ABT.annotation i <> ABT.annotation o) i o

ann :: Ord v => a -> Type v a -> K.Kind -> Type v a
ann a e t = ABT.tm' a (Ann e t)

forall :: Ord v => a -> v -> Type v a -> Type v a
forall a v body = ABT.tm' a (Forall (ABT.abs' a v body))

introOuter :: Ord v => a -> v -> Type v a -> Type v a
introOuter a v body = ABT.tm' a (IntroOuter (ABT.abs' a v body))

iff :: Var v => Type v ()
iff = forall () aa $ arrows (f <$> [boolean(), a, a]) a
  where aa = Var.named "a"
        a = var () aa
        f x = ((), x)

iff' :: Var v => a -> Type v a
iff' loc = forall loc aa $ arrows (f <$> [boolean loc, a, a]) a
  where aa = Var.named "a"
        a = var loc aa
        f x = (loc, x)

iff2 :: Var v => a -> Type v a
iff2 loc = forall loc aa $ arrows (f <$> [a, a]) a
  where aa = Var.named "a"
        a = var loc aa
        f x = (loc, x)

andor :: Ord v => Type v ()
andor = arrows (f <$> [boolean(), boolean()]) $ boolean()
  where f x = ((), x)

andor' :: Ord v => a -> Type v a
andor' a = arrows (f <$> [boolean a, boolean a]) $ boolean a
  where f x = (a, x)

var :: Ord v => a -> v -> Type v a
var = ABT.annotatedVar

v' :: Var v => Text -> Type v ()
v' s = ABT.var (Var.named s)

-- Like `v'`, but creates an annotated variable given an annotation
av' :: Var v => a -> Text -> Type v a
av' a s = ABT.annotatedVar a (Var.named s)

forall' :: Var v => a -> [Text] -> Type v a -> Type v a
forall' a vs body = foldr (forall a) body (Var.named <$> vs)

foralls :: Ord v => a -> [v] -> Type v a -> Type v a
foralls a vs body = foldr (forall a) body vs

-- Note: `a -> b -> c` parses as `a -> (b -> c)`
-- the annotation associated with `b` will be the annotation for the `b -> c`
-- node
arrows :: Ord v => [(a, Type v a)] -> Type v a -> Type v a
arrows ts result = foldr go result ts where
  go = uncurry arrow

-- The types of effectful computations
effect :: Ord v => a -> [Type v a] -> Type v a -> Type v a
effect a es (Effect1' fs t) =
  let es' = (es >>= flattenEffects) ++ flattenEffects fs
  in ABT.tm' a (Effect (ABT.tm' a (Effects es')) t)
effect a es t = ABT.tm' a (Effect (ABT.tm' a (Effects es)) t)

effects :: Ord v => a -> [Type v a] -> Type v a
effects a es = ABT.tm' a (Effects $ es >>= flattenEffects)

effect1 :: Ord v => a -> Type v a -> Type v a -> Type v a
effect1 a es (Effect1' fs t) =
  let es' = flattenEffects es ++ flattenEffects fs
  in ABT.tm' a (Effect (ABT.tm' a (Effects es')) t)
effect1 a es t = ABT.tm' a (Effect es t)

flattenEffects :: Type v a -> [Type v a]
flattenEffects (Effects' es) = es >>= flattenEffects
flattenEffects es = [es]

-- The types of first-class effect values
-- which get deconstructed in effect handlers.
effectV :: Ord v => a -> (a, Type v a) -> (a, Type v a) -> Type v a
effectV builtinA e t = apps (builtin builtinA "Effect") [e, t]

-- Strips effects from a type. E.g. `{e} a` becomes `a`.
stripEffect :: Ord v => Type v a -> ([Type v a], Type v a)
stripEffect (Effect' e t) = case stripEffect t of (ei, t) -> (e ++ ei, t)
stripEffect t = ([], t)

-- The type of the flipped function application operator:
-- `(a -> (a -> b) -> b)`
flipApply :: Var v => Type v () -> Type v ()
flipApply t = forall() b $ arrow() (arrow() t (var() b)) (var() b)
  where b = ABT.fresh t (Var.named "b")

generalize' :: Var v => Var.Type -> Type v a -> Type v a
generalize' k t = generalize vsk t where
  vsk = [ v | v <- Set.toList (freeVars t), Var.typeOf v == k ]

-- | Bind the given variables with an outer `forall`, if they are used in `t`.
generalize :: Ord v => [v] -> Type v a -> Type v a
generalize vs t = foldr f t vs
 where
  f v t =
    if Set.member v (freeVars t) then forall (ABT.annotation t) v t else t

unforall :: Type v a -> Type v a
unforall (ForallsNamed' _ t) = t
unforall t = t

unforall' :: Type v a -> ([v], Type v a)
unforall' (ForallsNamed' vs t) = (vs, t)
unforall' t = ([], t)

dependencies :: Ord v => Type v a -> Set Reference
dependencies t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where f t@(Ref r) = Writer.tell [r] $> t
        f t = pure t

updateDependencies :: Ord v => Map Reference Reference -> Type v a -> Type v a
updateDependencies typeUpdates = ABT.rebuildUp go
 where
  go (Ref r) = Ref (Map.findWithDefault r r typeUpdates)
  go f       = f

usesEffects :: Ord v => Type v a -> Bool
usesEffects t = getAny . getConst $ ABT.visit go t where
  go (Effect1' _ _) = Just (Const (Any True))
  go _ = Nothing

-- Returns free effect variables in the given type, for instance, in:
--
--   ∀ e3 . a ->{e,e2} b ->{e3} c
--
-- This function would return the set {e, e2}, but not `e3` since `e3`
-- is bound by the enclosing forall.
freeEffectVars :: Ord v => Type v a -> Set v
freeEffectVars t =
  Set.fromList . join . runIdentity $
    ABT.foreachSubterm go (snd <$> ABT.annotateBound t)
  where
    go t@(Effects' es) =
      let frees = Set.fromList [ v | Var' v <- es >>= flattenEffects ]
      in pure . Set.toList $ frees `Set.difference` ABT.annotation t
    go t@(Effect1' e _) =
      let frees = Set.fromList [ v | Var' v <- flattenEffects e ]
      in pure . Set.toList $ frees `Set.difference` ABT.annotation t
    go _ = pure []

existentializeArrows :: (Ord v, Monad m) => m v -> Type v a -> m (Type v a)
existentializeArrows freshVar = ABT.visit go
 where
  go t@(Arrow' a b) = case b of
    Effect1' _ _ -> Nothing
    _            -> Just $ do
      e <- freshVar
      a <- existentializeArrows freshVar a
      b <- existentializeArrows freshVar b
      let ann = ABT.annotation t
      pure $ arrow ann a (effect ann [var ann e] b)
  go _ = Nothing

-- Remove free effect variables from the type that are in the set
removeEffectVars :: ABT.Var v => Set v -> Type v a -> Type v a
removeEffectVars removals t =
  let z = effects () []
      t' = ABT.substsInheritAnnotation ((,z) <$> Set.toList removals) t
      -- leave explicitly empty `{}` alone
      removeEmpty (Effect1' (Effects' []) v) = Just (ABT.visitPure removeEmpty v)
      removeEmpty t@(Effect1' e v) =
        case flattenEffects e of
          [] -> Just (ABT.visitPure removeEmpty v)
          es -> Just (effect (ABT.annotation t) es $ ABT.visitPure removeEmpty v)
      removeEmpty t@(Effects' es) =
        Just $ effects (ABT.annotation t) (es >>= flattenEffects)
      removeEmpty _ = Nothing
  in ABT.visitPure removeEmpty t'

-- Remove all effect variables from the type.
-- Used for type-based search, we apply this transformation to both the
-- indexed type and the query type, so the user can supply `a -> b` that will
-- match `a ->{e} b` (but not `a ->{IO} b`).
removeAllEffectVars :: ABT.Var v => Type v a -> Type v a
removeAllEffectVars t = let
  allEffectVars = foldMap go (ABT.subterms t)
  go (Effects' vs) = Set.fromList [ v | Var' v <- vs]
  go (Effect1' (Var' v) _) = Set.singleton v
  go _ = mempty
  (vs, tu) = unforall' t
  in generalize vs (removeEffectVars allEffectVars tu)

removePureEffects :: ABT.Var v => Type v a -> Type v a
removePureEffects t | not Settings.removePureEffects = t
                    | otherwise =
  generalize vs $ removeEffectVars (Set.filter isPure fvs) tu
  where
    (vs, tu) = unforall' t
    fvs = freeEffectVars tu `Set.difference` freeVars t
    -- If an effect variable is mentioned only once, it is on
    -- an arrow `a ->{e} b`. Generalizing this to
    -- `∀ e . a ->{e} b` gives us the pure arrow `a -> b`.
    isPure v = ABT.occurrences v tu <= 1

editFunctionResult
  :: forall v a
   . Ord v
  => (Type v a -> Type v a)
  -> Type v a
  -> Type v a
editFunctionResult f = go
 where
  go :: Type v a -> Type v a
  go (ABT.Term s a t) = case t of
    ABT.Tm (Forall t) ->
      (\x -> ABT.Term (s <> freeVars x) a . ABT.Tm $ Forall x) $ go t
    ABT.Tm (Arrow i o) ->
      (\x -> ABT.Term (s <> freeVars x) a . ABT.Tm $ Arrow i x) $ go o
    ABT.Abs v r ->
      (\x -> ABT.Term (s <> freeVars x) a $ ABT.Abs v x) $ go r
    _ -> f (ABT.Term s a t)

functionResult :: Type v a -> Maybe (Type v a)
functionResult = go False
 where
  go inArr  (ForallNamed' _  body) = go inArr body
  go _inArr (Arrow'       _i o   ) = go True o
  go inArr  t                      = if inArr then Just t else Nothing


-- | Bind all free variables (not in `except`) that start with a lowercase
-- letter and are unqualified with an outer `forall`.
-- `a -> a` becomes `∀ a . a -> a`
-- `B -> B` becomes `B -> B` (not changed)
-- `.foo -> .foo` becomes `.foo -> .foo` (not changed)
-- `.foo.bar -> blarrg.woot` becomes `.foo.bar -> blarrg.woot` (unchanged)
generalizeLowercase :: Var v => Set v -> Type v a -> Type v a
generalizeLowercase except t = foldr (forall (ABT.annotation t)) t vars
 where
  vars =
    [ v | v <- Set.toList (freeVars t `Set.difference` except), Var.universallyQuantifyIfFree v ]

-- Convert all free variables in `allowed` to variables bound by an `introOuter`.
freeVarsToOuters :: Ord v => Set v -> Type v a -> Type v a
freeVarsToOuters allowed t = foldr (introOuter (ABT.annotation t)) t vars
  where vars = Set.toList $ freeVars t `Set.intersection` allowed

-- | This function removes all variable shadowing from the types and reduces
-- fresh ids to the minimum possible to avoid ambiguity. Useful when showing
-- two different types.
cleanupVars :: Var v => [Type v a] -> [Type v a]
cleanupVars ts | not Settings.cleanupTypes = ts
cleanupVars ts = let
  changedVars = cleanupVarsMap ts
  in cleanupVars1' changedVars <$> ts

-- Compute a variable replacement map from a collection of types, which
-- can be passed to `cleanupVars1'`. This is used to cleanup variable ids
-- for multiple related types, like when reporting a type error.
cleanupVarsMap :: Var v => [Type v a] -> Map.Map v v
cleanupVarsMap ts = let
  varsByName = foldl' step Map.empty (ts >>= ABT.allVars)
  step m v = Map.insertWith (++) (Var.name $ Var.reset v) [v] m
  changedVars = Map.fromList [ (v, Var.freshenId i v)
                             | (_, vs) <- Map.toList varsByName
                             , (v,i) <- nubOrd vs `zip` [0..]]
  in changedVars

cleanupVars1' :: Var v => Map.Map v v -> Type v a -> Type v a
cleanupVars1' = ABT.changeVars

-- | This function removes all variable shadowing from the type and reduces
-- fresh ids to the minimum possible to avoid ambiguity.
cleanupVars1 :: Var v => Type v a -> Type v a
cleanupVars1 t | not Settings.cleanupTypes = t
cleanupVars1 t = let [t'] = cleanupVars [t] in t'

-- This removes duplicates and normalizes the order of ability lists
cleanupAbilityLists :: Var v => Type v a -> Type v a
cleanupAbilityLists = ABT.visitPure go
 where
  -- leave explicitly empty `{}` alone
  go (Effect1' (Effects' []) _v) = Nothing
  go t@(Effect1' e v) =
    let es = Set.toList . Set.fromList $ flattenEffects e
    in  case es of
          [] -> Just (ABT.visitPure go v)
          _  -> Just (effect (ABT.annotation t) es $ ABT.visitPure go v)
  go _ = Nothing

cleanups :: Var v => [Type v a] -> [Type v a]
cleanups ts = cleanupVars $ map cleanupAbilityLists ts

cleanup :: Var v => Type v a -> Type v a
cleanup t | not Settings.cleanupTypes = t
cleanup t = cleanupVars1 . cleanupAbilityLists $ t

toReference :: (ABT.Var v, Show v) => Type v a -> Reference
toReference (Ref' r) = r
-- a bit of normalization - any unused type parameters aren't part of the hash
toReference (ForallNamed' v body) | not (Set.member v (freeVars body)) = toReference body
toReference t = Reference.Derived (ABT.hash t) 0 1

toReferenceMentions :: (ABT.Var v, Show v) => Type v a -> Set Reference
toReferenceMentions ty =
  let (vs, _) = unforall' ty
      gen ty = generalize (Set.toList (freeVars ty)) $ generalize vs ty
  in Set.fromList $ toReference . gen <$> ABT.subterms ty

hashComponents
  :: Var v => Map v (Type v a) -> Map v (Reference, Type v a)
hashComponents = ReferenceUtil.hashComponents $ ref ()

instance Hashable1 F where
  hash1 hashCycle hash e =
    let
      (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `0` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 0 : case e of
      Ref r -> [tag 0, Hashable.accumulateToken r]
      Arrow a b -> [tag 1, hashed (hash a), hashed (hash b) ]
      App a b -> [tag 2, hashed (hash a), hashed (hash b) ]
      Ann a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k ]
      -- Example:
      --   a) {Remote, Abort} (() -> {Remote} ()) should hash the same as
      --   b) {Abort, Remote} (() -> {Remote} ()) but should hash differently from
      --   c) {Remote, Abort} (() -> {Abort} ())
      Effects es -> let
        (hs, _) = hashCycle es
        in tag 4 : map hashed hs
      Effect e t -> [tag 5, hashed (hash e), hashed (hash t)]
      Forall a -> [tag 6, hashed (hash a)]
      IntroOuter a -> [tag 7, hashed (hash a)]

instance Show a => Show (F a) where
  showsPrec = go where
    go _ (Ref r) = shows r
    go p (Arrow i o) =
      showParen (p > 0) $ showsPrec (p+1) i <> s" -> " <> showsPrec p o
    go p (Ann t k) =
      showParen (p > 1) $ shows t <> s":" <> shows k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go p (Effects es) = showParen (p > 0) $
      s"{" <> shows es <> s"}"
    go p (Effect e t) = showParen (p > 0) $
     showParen True $ shows e <> s" " <> showsPrec p t
    go p (Forall body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"∀ " <> shows body
    go p (IntroOuter body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"outer " <> shows body
    (<>) = (.)
    s = showString

