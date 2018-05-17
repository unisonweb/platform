{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Term where

import Data.List (foldl')
import Data.Map (Map)
import Data.Set (Set, union)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Prelude.Extras (Eq1(..), Show1(..))
import Text.Show
import Unison.Hash (Hash)
import Unison.Hashable (Hashable1, accumulateToken)
import Unison.Literal
import Unison.Pattern (Pattern)
import Unison.Reference (Reference(..))
import Unison.Type (Type)
import Unison.Var (Var)
import Unsafe.Coerce
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
import qualified Unison.Reference as Reference
import qualified Unison.Type as Type

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data F typeVar a
  = Lit Literal
  | Blank -- An expression that has not been filled in, has type `forall a . a`
  | Ref Reference
  | Constructor Reference Int -- First argument identifies the data type, second argument identifies the constructor
  | App a a
  | Ann a (Type typeVar)
  | Vector (Vector a)
  | Lam a
  -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
  -- variables as there are bindings
  | LetRec [a] a
  -- Note: first parameter is the binding, second is the expression which may refer
  -- to this let bound variable. Constructed as `Let b (abs v e)`
  | Let a a
  -- Pattern matching / eliminating data types, example:
  --  case x of
  --    Just n -> rhs1
  --    Nothing -> rhs2
  --
  -- translates to
  --
  --   Match x
  --     [ (Constructor 0 [Var], ABT.abs n rhs1)
  --     , (Constructor 1 [], rhs2) ]
  | Match a [(Pattern, a)]
  deriving (Eq,Foldable,Functor,Generic,Generic1,Traversable)

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type AnnotatedTerm v a = ABT.Term (F v) v a
-- | Allow type variables and term variables to differ
type AnnotatedTerm' vt v a = ABT.Term (F vt) v a

-- | Terms are represented as ABTs over the base functor F, with variables in `v`
type Term v = AnnotatedTerm v ()
-- | Terms with type variables in `vt`, and term variables in `v`
type Term' vt v = AnnotatedTerm' vt v ()

vmap :: Ord v2 => (v -> v2) -> AnnotatedTerm v a -> AnnotatedTerm v2 a
vmap f = ABT.vmap f . typeMap (ABT.vmap f)

typeMap :: Ord v2 => (Type v -> Type v2) -> AnnotatedTerm v a -> ABT.Term (F v2) v a
typeMap f t = go t where
  go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
    ABT.Abs v t -> ABT.Abs v (go t)
    ABT.Var v -> ABT.Var v
    ABT.Cycle t -> ABT.Cycle (go t)
    ABT.Tm (Ann e t) -> ABT.Tm (Ann (go e) (f t))
    -- Safe since `Ann` is only ctor that has embedded `Type v` arg
    -- otherwise we'd have to manually match on every non-`Ann` ctor
    ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

wrapV :: Ord v => AnnotatedTerm v a -> AnnotatedTerm (ABT.V v) a
wrapV = vmap ABT.Bound

freeVars :: Term v -> Set v
freeVars = ABT.freeVars

freeTypeVars :: Ord vt => AnnotatedTerm' vt v a -> Set vt
freeTypeVars t = go t where
  go :: Ord vt => AnnotatedTerm' vt v a -> Set vt
  go (ABT.Term _ _ t) = case t of
    ABT.Abs _ t -> go t
    ABT.Var _ -> Set.empty
    ABT.Cycle t -> go t
    ABT.Tm (Ann e t) -> Type.freeVars t `union` go e
    ABT.Tm ts -> foldMap go ts

-- nicer pattern syntax

pattern Var' v <- ABT.Var' v
pattern Lit' l <- (ABT.out -> ABT.Tm (Lit l))
pattern Number' n <- Lit' (Number n)
pattern Text' s <- Lit' (Text s)
pattern If' <- Lit' If
pattern Blank' <- (ABT.out -> ABT.Tm Blank)
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))
pattern Builtin' r <- (ABT.out -> ABT.Tm (Ref (Builtin r)))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern Match' scrutinee branches <- (ABT.out -> ABT.Tm (Match scrutinee branches))
pattern Constructor' ref n <- (ABT.out -> ABT.Tm (Constructor ref n))
pattern Apps' f args <- (unApps -> Just (f, args))
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))
pattern Vector' xs <- (ABT.out -> ABT.Tm (Vector xs))
pattern Lam' subst <- ABT.Tm' (Lam (ABT.Abs' subst))
pattern LamNamed' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body))))
pattern Let1' b subst <- (unLet1 -> Just (b, subst))
pattern Let1Named' v b e <- (ABT.Tm' (Let b (ABT.out -> ABT.Abs v e)))
pattern Lets' bs e <- (unLet -> Just (bs, e))
pattern LetRecNamed' bs e <- (unLetRecNamed -> Just (bs,e))
pattern LetRec' subst <- (unLetRec -> Just subst)

fresh :: Var v => Term v -> v -> v
fresh = ABT.fresh

-- some smart constructors

var :: v -> Term v
var = ABT.var

var' :: Var v => Text -> Term v
var' = var . ABT.v'

derived :: Ord v => Hash -> Term v
derived = ref . Reference.Derived

derived' :: Ord v => Text -> Maybe (Term v)
derived' base58 = derived <$> Hash.fromBase58 base58

ref :: Ord v => Reference -> Term v
ref r = ABT.tm (Ref r)

builtin :: Ord v => Text -> Term v
builtin n = ref (Reference.Builtin n)

num :: Ord v => Double -> Term v
num = lit . Number

text :: Ord v => Text -> Term v
text = lit . Text

lit :: Ord v => Literal -> Term v
lit l = ABT.tm (Lit l)

blank :: Ord v => Term v
blank = ABT.tm Blank

app :: Ord v => Term v -> Term v -> Term v
app f arg = ABT.tm (App f arg)

match :: Ord v => Term v -> [(Pattern, Term v)] -> Term v
match scrutinee branches = ABT.tm (Match scrutinee branches)

constructor :: Ord v => Reference -> Int -> Term v
constructor ref n = ABT.tm (Constructor ref n)

apps :: Ord v => Term v -> [Term v] -> Term v
apps f = foldl' app f

ann :: Ord v => Term v -> Type v -> Term v
ann e t = ABT.tm (Ann e t)

vector :: Ord v => [Term v] -> Term v
vector es = ABT.tm (Vector (Vector.fromList es))

vectorForced :: Ord v => [Term v] -> Term v
vectorForced = app (builtin "Vector.force") . vector

vector' :: Ord v => Vector (Term v) -> Term v
vector' es = ABT.tm (Vector es)

lam :: Ord v => v -> Term v -> Term v
lam v body = ABT.tm (Lam (ABT.abs v body))

lam' :: Var v => [Text] -> Term v -> Term v
lam' vs body = foldr lam body (map ABT.v' vs)

lam'' :: Ord v => [v] -> Term v -> Term v
lam'' vs body = foldr lam body vs

-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec :: Ord v => [(v,Term v)] -> Term v -> Term v
letRec [] e = e
letRec bindings e = ABT.cycle (foldr ABT.abs z (map fst bindings))
  where
    z = ABT.tm (LetRec (map snd bindings) e)

letRec' :: Var v => [(Text, Term v)] -> Term v -> Term v
letRec' bs e = letRec [(ABT.v' name, b) | (name,b) <- bs] e

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
let1 :: Ord v => [(v,Term v)] -> Term v -> Term v
let1 bindings e = foldr f e bindings
  where
    f (v,b) body = ABT.tm (Let b (ABT.abs v body))

let1' :: Var v => [(Text,Term v)] -> Term v -> Term v
let1' bs e = let1 [(ABT.v' name, b) | (name,b) <- bs ] e

unLet1 :: Var v => Term v -> Maybe (Term v, ABT.Subst (F v) v ())
unLet1 (ABT.Tm' (Let b (ABT.Abs' subst))) = Just (b, subst)
unLet1 _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet :: Term v -> Maybe ([(v, Term v)], Term v)
unLet t = fixup (go t) where
  go (ABT.Tm' (Let b (ABT.out -> ABT.Abs v t))) =
    case go t of (env,t) -> ((v,b):env, t)
  go t = ([], t)
  fixup ([], _) = Nothing
  fixup bst = Just bst

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRecNamed :: Term v -> Maybe ([(v, Term v)], Term v)
unLetRecNamed (ABT.Cycle' vs (ABT.Tm' (LetRec bs e)))
  | length vs == length vs = Just (zip vs bs, e)
unLetRecNamed _ = Nothing

unLetRec :: Monad m => Var v => Term v -> Maybe ((v -> m v) -> m ([(v, Term v)], Term v))
unLetRec (unLetRecNamed -> Just (bs, e)) = Just $ \freshen -> do
  vs <- sequence [ freshen v | (v,_) <- bs ]
  let sub = ABT.substs (map fst bs `zip` map ABT.var vs)
  pure (vs `zip` [ sub b | (_,b) <- bs ], sub e)
unLetRec _ = Nothing

unApps :: Term v -> Maybe (Term v, [Term v])
unApps t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc = go i (o:acc)
  go _ [] = []
  go fn args = fn:args

pattern LamsNamed' vs body <- (unLams' -> Just (vs, body))

unLams' :: Term v -> Maybe ([v], Term v)
unLams' (LamNamed' v body) = case unLams' body of
  Nothing -> Just ([v], body)
  Just (vs, body) -> Just (v:vs, body)
unLams' _ = Nothing

dependencies' :: Ord v => Term v -> Set Reference
dependencies' t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where f t@(Ref r) = Writer.tell [r] *> pure t
        f t = pure t

dependencies :: Ord v => Term v -> Set Hash
dependencies e = Set.fromList [ h | Reference.Derived h <- Set.toList (dependencies' e) ]

referencedDataDeclarations :: Term v -> Set Reference
referencedDataDeclarations _ =
  Set.empty -- TODO: referenced data declarations, should gather up data decl refs from pattern matching

updateDependencies :: Ord v => Map Reference Reference -> Term v -> Term v
updateDependencies u tm = ABT.rebuildUp go tm where
  go (Ref r) = Ref (Map.findWithDefault r r u)
  go f = f

countBlanks :: Ord v => Term v -> Int
countBlanks t = Monoid.getSum . Writer.execWriter $ ABT.visit' f t
  where f Blank = Writer.tell (Monoid.Sum (1 :: Int)) *> pure Blank
        f t = pure t

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Var v => Term v -> Term v
betaReduce (App' (Lam' f) arg) = ABT.bind f arg
betaReduce e = e

instance Var v => Hashable1 (F v) where
  hash1 hashCycle hash e =
    let
      (tag, hashed, varint) = (Hashable.Tag, Hashable.Hashed, Hashable.VarInt)
    in case e of
      -- So long as `Reference.Derived` ctors are created using the same hashing
      -- function as is used here, this case ensures that references are 'transparent'
      -- wrt hash and hashing is unaffected by whether expressions are linked.
      -- So for example `x = 1 + 1` and `y = x` hash the same.
      Ref (Reference.Derived h) -> Hashable.fromBytes (Hash.toBytes h)
      -- Note: start each layer with leading `1` byte, to avoid collisions with
      -- types, which start each layer with leading `0`. See `Hashable1 Type.F`
      _ -> Hashable.accumulate $ tag 1 : case e of
        Lit l -> [tag 0, accumulateToken l]
        Blank -> [tag 1]
        Ref (Reference.Builtin name) -> [tag 2, accumulateToken name]
        Ref (Reference.Derived _) -> error "handled above, but GHC can't figure this out"
        App a a2 -> [tag 3, hashed (hash a), hashed (hash a2)]
        Ann a t -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
        Vector as -> tag 5 : varint (Vector.length as) : map (hashed . hash) (Vector.toList as)
        Lam a -> [tag 6, hashed (hash a) ]
        -- note: we use `hashCycle` to ensure result is independent of let binding order
        LetRec as a -> case hashCycle as of
          (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
        -- here, order is significant, so don't use hashCycle
        Let b a -> [tag 8, hashed (hash b), hashed (hash a)]
        Constructor r n -> [tag 12, accumulateToken r, varint n]
        Match e branches -> tag 13 : hashed (hash e) : concatMap h branches where
          h (pat, branch) = [accumulateToken pat, hashed (hash branch)]

-- mostly boring serialization code below ...

instance Var v => Eq1 (F v) where (==#) = (==)
instance Var v => Show1 (F v) where showsPrec1 = showsPrec

instance (Var v, Show a) => Show (F v a) where
  showsPrec p fa = go p fa where
    go _ (Lit l) = showsPrec 0 l
    go p (Ann t k) = showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go _ (Lam body) = showParen True (s"λ " <> showsPrec 0 body)
    go _ (Vector vs) = showListWith (showsPrec 0) (Vector.toList vs)
    go _ Blank = s"_"
    go _ (Ref r) = showsPrec 0 r
    go _ (Let b body) = showParen True (s"let " <> showsPrec 0 b <> s" in " <> showsPrec 0 body)
    go _ (LetRec bs body) = showParen True (s"let rec" <> showsPrec 0 bs <> s" in " <> showsPrec 0 body)
    go _ (Constructor r n) = showsPrec 0 r <> showsPrec 0 n
    go _ (Match _ _) = s"match"
    (<>) = (.)
    s = showString
