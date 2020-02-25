{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Term where

import Unison.Prelude

import Prelude hiding (and,or)
import           Control.Monad.State (evalState)
import qualified Control.Monad.Writer.Strict as Writer
import           Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import           Prelude.Extras (Eq1(..), Show1(..))
import           Text.Show
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import qualified Unison.Hash as Hash
import           Unison.Hashable (Hashable1, accumulateToken)
import qualified Unison.Hashable as Hashable
import           Unison.Names3 ( Names0 )
import qualified Unison.Names3 as Names
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Reference (Reference, pattern Builtin)
import qualified Unison.Reference as Reference
import qualified Unison.Reference.Util as ReferenceUtil
import           Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as Rel
import qualified Unison.ConstructorType as CT
import Unison.Util.List (multimap, validate)
import           Unison.Var (Var)
import qualified Unison.Var as Var
import           Unsafe.Coerce
import Unison.Symbol (Symbol)
import qualified Unison.Name as Name
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)

type ConstructorId = Pattern.ConstructorId

data MatchCase loc a = MatchCase (Pattern loc) (Maybe a) a
  deriving (Show,Eq,Foldable,Functor,Generic,Generic1,Traversable)

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data F typeVar typeAnn patternAnn a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text Text
  | Char Char
  | Blank (B.Blank typeAnn)
  | Ref Reference
  -- First argument identifies the data type,
  -- second argument identifies the constructor
  | Constructor Reference Int
  | Request Reference Int
  | Handle a a
  | App a a
  | Ann a (Type typeVar typeAnn)
  | Sequence (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
  -- variables as there are bindings
  | LetRec IsTop [a] a
  -- Note: first parameter is the binding, second is the expression which may refer
  -- to this let bound variable. Constructed as `Let b (abs v e)`
  | Let IsTop a a
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
  | Match a [MatchCase patternAnn a]
  | TermLink Referent
  | TypeLink Reference
  deriving (Foldable,Functor,Generic,Generic1,Traversable)

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type AnnotatedTerm v a = AnnotatedTerm2 v a a v a
-- | Allow type variables and term variables to differ
type AnnotatedTerm' vt v a = AnnotatedTerm2 vt a a v a
-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type AnnotatedTerm2 vt at ap v a = ABT.Term (F vt at ap) v a
-- | Like `AnnotatedTerm v a`, but with only () for type and pattern annotations.
type AnnotatedTerm3 v a = AnnotatedTerm2 v () () v a

-- | Terms are represented as ABTs over the base functor F, with variables in `v`
type Term v = AnnotatedTerm v ()
-- | Terms with type variables in `vt`, and term variables in `v`
type Term' vt v = AnnotatedTerm' vt v ()

-- bindExternals
--   :: forall v a b b2
--    . Var v
--   => [(v, AnnotatedTerm2 v b a v b2)]
--   -> [(v, Reference)]
--   -> AnnotatedTerm2 v b a v a
--   -> AnnotatedTerm2 v b a v a
-- bindBuiltins termBuiltins typeBuiltins = f . g
--  where
--   f :: AnnotatedTerm2 v b a v a -> AnnotatedTerm2 v b a v a
--   f = typeMap (Type.bindBuiltins typeBuiltins)
--   g :: AnnotatedTerm2 v b a v a -> AnnotatedTerm2 v b a v a
--   g = ABT.substsInheritAnnotation termBuiltins
bindNames
  :: forall v a . Var v
  => Set v
  -> Names0
  -> AnnotatedTerm v a
  -> Names.ResolutionResult v a (AnnotatedTerm v a)
-- bindNames keepFreeTerms _ _ | trace "Keep free terms:" False
--                            || traceShow keepFreeTerms False = undefined
bindNames keepFreeTerms ns e = do
  let freeTmVars = [ (v,a) | (v,a) <- ABT.freeVarOccurrences keepFreeTerms e ]
      -- !_ = trace "free term vars: " ()
      -- !_ = traceShow $ fst <$> freeTmVars
      freeTyVars = [ (v, a) | (v,as) <- Map.toList (freeTypeVarAnnotations e)
                            , a <- as ]
      -- !_ = trace "free type vars: " ()
      -- !_ = traceShow $ fst <$> freeTyVars
      okTm :: (v,a) -> Names.ResolutionResult v a (v, AnnotatedTerm v a)
      okTm (v,a) = case Rel.lookupDom (Name.fromVar v) (Names.terms0 ns) of
        rs | Set.size rs == 1 ->
               pure (v, fromReferent a $ Set.findMin rs)
           | otherwise -> Left (pure (Names.TermResolutionFailure v a rs))
      okTy (v,a) = case Rel.lookupDom (Name.fromVar v) (Names.types0 ns) of
        rs | Set.size rs == 1 -> pure (v, Type.ref a $ Set.findMin rs)
           | otherwise -> Left (pure (Names.TypeResolutionFailure v a rs))
  termSubsts <- validate okTm freeTmVars
  typeSubsts <- validate okTy freeTyVars
  pure . substTypeVars typeSubsts . ABT.substsInheritAnnotation termSubsts $ e

bindSomeNames
  :: forall v a . Var v
  => Names0
  -> AnnotatedTerm v a
  -> Names.ResolutionResult v a (AnnotatedTerm v a)
-- bindSomeNames ns e | trace "Term.bindSome" False
--                   || trace "Names =" False
--                   || traceShow ns False
--                   || trace "Free type vars:" False
--                   || traceShow (freeTypeVars e) False
--                   || traceShow e False
--                   = undefined
bindSomeNames ns e = bindNames keepFree ns e where
  keepFree = Set.difference (freeVars e)
                            (Set.map Name.toVar $ Rel.dom (Names.terms0 ns))

-- Prepare a term for type-directed name resolution by replacing
-- any remaining free variables with blanks to be resolved by TDNR
prepareTDNR :: Var v => ABT.Term (F vt b ap) v b -> ABT.Term (F vt b ap) v b
prepareTDNR t = fmap fst . ABT.visitPure f $ ABT.annotateBound t
  where f (ABT.Term _ (a, bound) (ABT.Var v)) | Set.notMember v bound =
          Just $ resolve (a, bound) a (Text.unpack $ Var.name v)
        f _ = Nothing

amap :: Ord v => (a -> a2) -> AnnotatedTerm v a -> AnnotatedTerm v a2
amap f = fmap f . patternMap (fmap f) . typeMap (fmap f)

patternMap :: (Pattern ap -> Pattern ap2) -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap2 v a
patternMap f = go where
  go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
    ABT.Abs v t -> ABT.Abs v (go t)
    ABT.Var v -> ABT.Var v
    ABT.Cycle t -> ABT.Cycle (go t)
    ABT.Tm (Match e cases) -> ABT.Tm (Match (go e) [
      MatchCase (f p) (go <$> g) (go a) | MatchCase p g a <- cases ])
    -- Safe since `Match` is only ctor that has embedded `Pattern ap` arg
    ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

vmap :: Ord v2 => (v -> v2) -> AnnotatedTerm v a -> AnnotatedTerm v2 a
vmap f = ABT.vmap f . typeMap (ABT.vmap f)

vtmap :: Ord vt2 => (vt -> vt2) -> AnnotatedTerm' vt v a -> AnnotatedTerm' vt2 v a
vtmap f = typeMap (ABT.vmap f)

typeMap
  :: Ord vt2
  => (Type vt at -> Type vt2 at2)
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt2 at2 ap v a
typeMap f = go
 where
  go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
    ABT.Abs v t         -> ABT.Abs v (go t)
    ABT.Var   v         -> ABT.Var v
    ABT.Cycle t         -> ABT.Cycle (go t)
    ABT.Tm    (Ann e t) -> ABT.Tm (Ann (go e) (f t))
    -- Safe since `Ann` is only ctor that has embedded `Type v` arg
    -- otherwise we'd have to manually match on every non-`Ann` ctor
    ABT.Tm    ts        -> unsafeCoerce $ ABT.Tm (fmap go ts)

extraMap'
  :: (Ord vt, Ord vt')
  => (vt -> vt')
  -> (at -> at')
  -> (ap -> ap')
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt' at' ap' v a
extraMap' vtf atf apf = ABT.extraMap (extraMap vtf atf apf)

extraMap
  :: (Ord vt, Ord vt')
  => (vt -> vt')
  -> (at -> at')
  -> (ap -> ap')
  -> F vt at ap a
  -> F vt' at' ap' a
extraMap vtf atf apf = \case
  Int x -> Int x
  Nat x -> Nat x
  Float x -> Float x
  Boolean x -> Boolean x
  Text x -> Text x
  Char x -> Char x
  Blank x -> Blank (fmap atf x)
  Ref x -> Ref x
  Constructor x y -> Constructor x y
  Request x y -> Request x y
  Handle x y -> Handle x y
  App x y -> App x y
  Ann tm x -> Ann tm (ABT.amap atf (ABT.vmap vtf x))
  Sequence x -> Sequence x
  If x y z -> If x y z
  And x y -> And x y
  Or x y -> Or x y
  Lam x -> Lam x
  LetRec x y z -> LetRec x y z
  Let x y z -> Let x y z
  Match tm l -> Match tm (map (matchCaseExtraMap apf) l)
  TermLink r -> TermLink r
  TypeLink r -> TypeLink r

matchCaseExtraMap :: (loc -> loc') -> MatchCase loc a -> MatchCase loc' a
matchCaseExtraMap f (MatchCase p x y) = MatchCase (fmap f p) x y

unannotate
  :: forall vt at ap v a . Ord v => AnnotatedTerm2 vt at ap v a -> Term' vt v
unannotate = go
 where
  go :: AnnotatedTerm2 vt at ap v a -> Term' vt v
  go (ABT.out -> ABT.Abs v body) = ABT.abs v (go body)
  go (ABT.out -> ABT.Cycle body) = ABT.cycle (go body)
  go (ABT.Var' v               ) = ABT.var v
  go (ABT.Tm'  f               ) = case go <$> f of
    Ann e t -> ABT.tm (Ann e (void t))
    Match scrutinee branches ->
      let unann (MatchCase pat guard body) = MatchCase (void pat) guard body
      in  ABT.tm (Match scrutinee (unann <$> branches))
    f' -> ABT.tm (unsafeCoerce f')
  go _ = error "unpossible"

wrapV :: Ord v => AnnotatedTerm v a -> AnnotatedTerm (ABT.V v) a
wrapV = vmap ABT.Bound

-- | All variables mentioned in the given term.
-- Includes both term and type variables, both free and bound.
allVars :: Ord v => AnnotatedTerm v a -> Set v
allVars tm = Set.fromList $
  ABT.allVars tm ++ [ v | tp <- allTypes tm, v <- ABT.allVars tp ]
  where
  allTypes tm = case tm of
    Ann' e tp -> tp : allTypes e
    _ -> foldMap allTypes $ ABT.out tm

freeVars :: AnnotatedTerm' vt v a -> Set v
freeVars = ABT.freeVars

freeTypeVars :: Ord vt => AnnotatedTerm' vt v a -> Set vt
freeTypeVars t = Map.keysSet $ freeTypeVarAnnotations t

freeTypeVarAnnotations :: Ord vt => AnnotatedTerm' vt v a -> Map vt [a]
freeTypeVarAnnotations e = multimap $ go Set.empty e where
  go bound tm = case tm of
    Var' _ -> mempty
    Ann' e (Type.stripIntroOuters -> t1) -> let
      bound' = case t1 of Type.ForallsNamed' vs _ -> bound <> Set.fromList vs
                          _                       -> bound
      in go bound' e <> ABT.freeVarOccurrences bound t1
    ABT.Tm' f -> foldMap (go bound) f
    (ABT.out -> ABT.Abs _ body) -> go bound body
    (ABT.out -> ABT.Cycle body) -> go bound body
    _ -> error "unpossible"

substTypeVars :: (Ord v, Var vt)
  => [(vt, Type vt b)]
  -> AnnotatedTerm' vt v a
  -> AnnotatedTerm' vt v a
substTypeVars subs e = foldl' go e subs where
  go e (vt, t) = substTypeVar vt t e

-- Capture-avoiding substitution of a type variable inside a term. This
-- will replace that type variable wherever it appears in type signatures of
-- the term, avoiding capture by renaming ∀-binders.
substTypeVar
  :: (Ord v, ABT.Var vt)
  => vt
  -> Type vt b
  -> AnnotatedTerm' vt v a
  -> AnnotatedTerm' vt v a
substTypeVar vt ty = go Set.empty where
  go bound tm | Set.member vt bound = tm
  go bound tm = let loc = ABT.annotation tm in case tm of
    Var' _ -> tm
    Ann' e t -> uncapture [] e (Type.stripIntroOuters t) where
      fvs = ABT.freeVars ty
      -- if the ∀ introduces a variable, v, which is free in `ty`, we pick a new
      -- variable name for v which is unique, v', and rename v to v' in e.
      uncapture vs e t@(Type.Forall' body) | Set.member (ABT.variable body) fvs = let
        v = ABT.variable body
        v2 = Var.freshIn (ABT.freeVars t) . Var.freshIn (Set.insert vt fvs) $ v
        t2 = ABT.bindInheritAnnotation body (Type.var() v2)
        in uncapture ((ABT.annotation t, v2):vs) (renameTypeVar v v2 e) t2
      uncapture vs e t0 = let
        t = foldl (\body (loc,v) -> Type.forall loc v body) t0 vs
        bound' = case Type.unForalls (Type.stripIntroOuters t) of
          Nothing -> bound
          Just (vs, _) -> bound <> Set.fromList vs
        t' = ABT.substInheritAnnotation vt ty (Type.stripIntroOuters t)
        in ann loc (go bound' e) (Type.freeVarsToOuters bound t')
    ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
    (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
    (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
    _ -> error "unpossible"

renameTypeVar :: (Ord v, ABT.Var vt) => vt -> vt -> AnnotatedTerm' vt v a -> AnnotatedTerm' vt v a
renameTypeVar old new = go Set.empty where
  go bound tm | Set.member old bound = tm
  go bound tm = let loc = ABT.annotation tm in case tm of
    Var' _ -> tm
    Ann' e t -> let
      bound' = case Type.unForalls (Type.stripIntroOuters t) of
        Nothing -> bound
        Just (vs, _) -> bound <> Set.fromList vs
      t' = ABT.rename old new (Type.stripIntroOuters t)
      in ann loc (go bound' e) (Type.freeVarsToOuters bound t')
    ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
    (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
    (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
    _ -> error "unpossible"

-- Converts free variables to bound variables using forall or introOuter. Example:
--
-- foo : x -> x
-- foo a =
--   r : x
--   r = a
--   r
--
-- This becomes:
--
-- foo : ∀ x . x -> x
-- foo a =
--   r : outer x . x -- FYI, not valid syntax
--   r = a
--   r
--
-- More specifically: in the expression `e : t`, unbound lowercase variables in `t`
-- are bound with foralls, and any ∀-quantified type variables are made bound in
-- `e` and its subexpressions. The result is a term with no lowercase free
-- variables in any of its type signatures, with outer references represented
-- with explicit `introOuter` binders. The resulting term may have uppercase
-- free variables that are still unbound.
generalizeTypeSignatures :: (Var vt, Var v) => AnnotatedTerm' vt v a -> AnnotatedTerm' vt v a
generalizeTypeSignatures = go Set.empty where
  go bound tm = let loc = ABT.annotation tm in case tm of
    Var' _ -> tm
    Ann' e (Type.generalizeLowercase bound -> t) -> let
      bound' = case Type.unForalls t of
        Nothing -> bound
        Just (vs, _) -> bound <> Set.fromList vs
      in ann loc (go bound' e) (Type.freeVarsToOuters bound t)
    ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
    (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
    (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
    _ -> error "unpossible"

-- nicer pattern syntax

pattern Var' v <- ABT.Var' v
pattern Cycle' xs t <- ABT.Cycle' xs t
pattern Abs' subst <- ABT.Abs' subst
pattern Int' n <- (ABT.out -> ABT.Tm (Int n))
pattern Nat' n <- (ABT.out -> ABT.Tm (Nat n))
pattern Float' n <- (ABT.out -> ABT.Tm (Float n))
pattern Boolean' b <- (ABT.out -> ABT.Tm (Boolean b))
pattern Text' s <- (ABT.out -> ABT.Tm (Text s))
pattern Char' c <- (ABT.out -> ABT.Tm (Char c))
pattern Blank' b <- (ABT.out -> ABT.Tm (Blank b))
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))
pattern TermLink' r <- (ABT.out -> ABT.Tm (TermLink r))
pattern TypeLink' r <- (ABT.out -> ABT.Tm (TypeLink r))
pattern Builtin' r <- (ABT.out -> ABT.Tm (Ref (Builtin r)))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern Match' scrutinee branches <- (ABT.out -> ABT.Tm (Match scrutinee branches))
pattern Constructor' ref n <- (ABT.out -> ABT.Tm (Constructor ref n))
pattern Request' ref n <- (ABT.out -> ABT.Tm (Request ref n))
pattern RequestOrCtor' ref n <- (unReqOrCtor -> Just (ref, n))
pattern If' cond t f <- (ABT.out -> ABT.Tm (If cond t f))
pattern And' x y <- (ABT.out -> ABT.Tm (And x y))
pattern Or' x y <- (ABT.out -> ABT.Tm (Or x y))
pattern Handle' h body <- (ABT.out -> ABT.Tm (Handle h body))
pattern Apps' f args <- (unApps -> Just (f, args))
-- begin pretty-printer helper patterns
pattern AppsPred' f args <- (unAppsPred -> Just (f, args))
pattern BinaryApp' f arg1 arg2 <- (unBinaryApp -> Just (f, arg1, arg2))
pattern BinaryApps' apps lastArg <- (unBinaryApps -> Just (apps, lastArg))
pattern BinaryAppsPred' apps lastArg <- (unBinaryAppsPred -> Just (apps, lastArg))
-- end pretty-printer helper patterns
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))
pattern Sequence' xs <- (ABT.out -> ABT.Tm (Sequence xs))
pattern Lam' subst <- ABT.Tm' (Lam (ABT.Abs' subst))
pattern LamNamed' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body))))
pattern LamsNamed' vs body <- (unLams' -> Just (vs, body))
pattern LamsNamedOpt' vs body <- (unLamsOpt' -> Just (vs, body))
pattern LamsNamedPred' vs body <- (unLamsPred' -> Just (vs, body))
pattern LamsNamedOrDelay' vs body <- (unLamsUntilDelay' -> Just (vs, body))
pattern LamsNamedMatch' vs branches <- (unLamsMatch' -> Just (vs, branches))
pattern Let1' b subst <- (unLet1 -> Just (_, b, subst))
pattern Let1Top' top b subst <- (unLet1 -> Just (top, b, subst))
pattern Let1Named' v b e <- (ABT.Tm' (Let _ b (ABT.out -> ABT.Abs v e)))
pattern Let1NamedTop' top v b e <- (ABT.Tm' (Let top b (ABT.out -> ABT.Abs v e)))
pattern Lets' bs e <- (unLet -> Just (bs, e))
pattern LetRecNamed' bs e <- (unLetRecNamed -> Just (_,bs,e))
pattern LetRec' subst <- (unLetRec -> Just (_, subst))
pattern LetRecTop' top subst <- (unLetRec -> Just (top, subst))
pattern LetRecNamedAnnotated' ann bs e <- (unLetRecNamedAnnotated -> Just (_, ann, bs,e))
pattern LetRecNamedAnnotatedTop' top ann bs e <-
          (unLetRecNamedAnnotated -> Just (top, ann, bs,e))

fresh :: Var v => Term v -> v -> v
fresh = ABT.fresh

-- some smart constructors

var :: a -> v -> AnnotatedTerm2 vt at ap v a
var = ABT.annotatedVar

var' :: Var v => Text -> Term' vt v
var' = var() . Var.named

ref :: Ord v => a -> Reference -> AnnotatedTerm2 vt at ap v a
ref a r = ABT.tm' a (Ref r)

termLink :: Ord v => a -> Referent -> AnnotatedTerm2 vt at ap v a
termLink a r = ABT.tm' a (TermLink r)

typeLink :: Ord v => a -> Reference -> AnnotatedTerm2 vt at ap v a
typeLink a r = ABT.tm' a (TypeLink r)

builtin :: Ord v => a -> Text -> AnnotatedTerm2 vt at ap v a
builtin a n = ref a (Reference.Builtin n)

float :: Ord v => a -> Double -> AnnotatedTerm2 vt at ap v a
float a d = ABT.tm' a (Float d)

boolean :: Ord v => a -> Bool -> AnnotatedTerm2 vt at ap v a
boolean a b = ABT.tm' a (Boolean b)

int :: Ord v => a -> Int64 -> AnnotatedTerm2 vt at ap v a
int a d = ABT.tm' a (Int d)

nat :: Ord v => a -> Word64 -> AnnotatedTerm2 vt at ap v a
nat a d = ABT.tm' a (Nat d)

text :: Ord v => a -> Text -> AnnotatedTerm2 vt at ap v a
text a = ABT.tm' a . Text

char :: Ord v => a -> Char -> AnnotatedTerm2 vt at ap v a
char a = ABT.tm' a . Char

watch :: (Var v, Semigroup a) => a -> String -> AnnotatedTerm v a -> AnnotatedTerm v a
watch a note e =
  apps' (builtin a "Debug.watch") [text a (Text.pack note), e]

watchMaybe :: (Var v, Semigroup a) => Maybe String -> AnnotatedTerm v a -> AnnotatedTerm v a
watchMaybe Nothing     e = e
watchMaybe (Just note) e = watch (ABT.annotation e) note e

blank :: Ord v => a -> AnnotatedTerm2 vt at ap v a
blank a = ABT.tm' a (Blank B.Blank)

placeholder :: Ord v => a -> String -> AnnotatedTerm2 vt a ap v a
placeholder a s = ABT.tm' a . Blank $ B.Recorded (B.Placeholder a s)

resolve :: Ord v => at -> ab -> String -> AnnotatedTerm2 vt ab ap v at
resolve at ab s = ABT.tm' at . Blank $ B.Recorded (B.Resolve ab s)

constructor :: Ord v => a -> Reference -> Int -> AnnotatedTerm2 vt at ap v a
constructor a ref n = ABT.tm' a (Constructor ref n)

request :: Ord v => a -> Reference -> Int -> AnnotatedTerm2 vt at ap v a
request a ref n = ABT.tm' a (Request ref n)

-- todo: delete and rename app' to app
app_ :: Ord v => Term' vt v -> Term' vt v -> Term' vt v
app_ f arg = ABT.tm (App f arg)

app :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
app a f arg = ABT.tm' a (App f arg)

match :: Ord v => a -> AnnotatedTerm2 vt at a v a -> [MatchCase a (AnnotatedTerm2 vt at a v a)] -> AnnotatedTerm2 vt at a v a
match a scrutinee branches = ABT.tm' a (Match scrutinee branches)

handle :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
handle a h block = ABT.tm' a (Handle h block)

and :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
and a x y = ABT.tm' a (And x y)

or :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
or a x y = ABT.tm' a (Or x y)

seq :: Ord v => a -> [AnnotatedTerm2 vt at ap v a] -> AnnotatedTerm2 vt at ap v a
seq a es = seq' a (Sequence.fromList es)

seq' :: Ord v => a -> Seq (AnnotatedTerm2 vt at ap v a) -> AnnotatedTerm2 vt at ap v a
seq' a es = ABT.tm' a (Sequence es)

apps
  :: Ord v
  => AnnotatedTerm2 vt at ap v a
  -> [(a, AnnotatedTerm2 vt at ap v a)]
  -> AnnotatedTerm2 vt at ap v a
apps = foldl' (\f (a, t) -> app a f t)

apps'
  :: (Ord v, Semigroup a)
  => AnnotatedTerm2 vt at ap v a
  -> [AnnotatedTerm2 vt at ap v a]
  -> AnnotatedTerm2 vt at ap v a
apps' = foldl' (\f t -> app (ABT.annotation f <> ABT.annotation t) f t)

iff :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
iff a cond t f = ABT.tm' a (If cond t f)

ann_ :: Ord v => Term' vt v -> Type vt () -> Term' vt v
ann_ e t = ABT.tm (Ann e t)

ann :: Ord v
    => a
    -> AnnotatedTerm2 vt at ap v a
    -> Type vt at
    -> AnnotatedTerm2 vt at ap v a
ann a e t = ABT.tm' a (Ann e t)

-- arya: are we sure we want the two annotations to be the same?
lam :: Ord v => a -> v -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam a v body = ABT.tm' a (Lam (ABT.abs' a v body))

lam' :: Ord v => a -> [v] -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam' a vs body = foldr (lam a) body vs

lam'' :: Ord v => [(a,v)] -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam'' vs body = foldr (uncurry lam) body vs

isLam :: AnnotatedTerm2 vt at ap v a -> Bool
isLam t = arity t > 0

arity :: AnnotatedTerm2 vt at ap v a -> Int
arity (LamNamed' _ body) = 1 + arity body
arity (Ann' e _) = arity e
arity _ = 0

unLetRecNamedAnnotated
  :: AnnotatedTerm' vt v a
  -> Maybe
       (IsTop, a, [((a, v), AnnotatedTerm' vt v a)], AnnotatedTerm' vt v a)
unLetRecNamedAnnotated (ABT.CycleA' ann avs (ABT.Tm' (LetRec isTop bs e))) =
  Just (isTop, ann, avs `zip` bs, e)
unLetRecNamedAnnotated _ = Nothing

letRec'
  :: (Ord v, Monoid a)
  => Bool
  -> [(v, AnnotatedTerm' vt v a)]
  -> AnnotatedTerm' vt v a
  -> AnnotatedTerm' vt v a
letRec' isTop bindings body =
  letRec isTop
    (foldMap (ABT.annotation . snd) bindings <> ABT.annotation body)
    [ ((ABT.annotation b, v), b) | (v,b) <- bindings ]
    body

letRec
  :: Ord v
  => Bool
  -> a
  -> [((a, v), AnnotatedTerm' vt v a)]
  -> AnnotatedTerm' vt v a
  -> AnnotatedTerm' vt v a
letRec _ _ []       e     = e
letRec isTop a bindings e = ABT.cycle'
  a
  (foldr (uncurry ABT.abs' . fst) z bindings)
  where z = ABT.tm' a (LetRec isTop (map snd bindings) e)


-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec_ :: Ord v => IsTop -> [(v, Term' vt v)] -> Term' vt v -> Term' vt v
letRec_ _ [] e = e
letRec_ isTop bindings e = ABT.cycle (foldr (ABT.abs . fst) z bindings)
  where
    z = ABT.tm (LetRec isTop (map snd bindings) e)

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
-- todo: delete me
let1_ :: Ord v => IsTop -> [(v,Term' vt v)] -> Term' vt v -> Term' vt v
let1_ isTop bindings e = foldr f e bindings
  where
    f (v,b) body = ABT.tm (Let isTop b (ABT.abs v body))

-- | annotations are applied to each nested Let expression
let1
  :: Ord v
  => IsTop
  -> [((a, v), AnnotatedTerm2 vt at ap v a)]
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt at ap v a
let1 isTop bindings e = foldr f e bindings
  where f ((ann, v), b) body = ABT.tm' ann (Let isTop b (ABT.abs' ann v body))

let1'
  :: (Semigroup a, Ord v)
  => IsTop
  -> [(v, AnnotatedTerm2 vt at ap v a)]
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt at ap v a
let1' isTop bindings e = foldr f e bindings
 where
  ann = ABT.annotation
  f (v, b) body = ABT.tm' a (Let isTop b (ABT.abs' a v body))
    where a = ann b <> ann body

-- let1' :: Var v => [(Text, Term' vt v)] -> Term' vt v -> Term' vt v
-- let1' bs e = let1 [(ABT.v' name, b) | (name,b) <- bs ] e

unLet1
  :: Var v
  => AnnotatedTerm' vt v a
  -> Maybe (IsTop, AnnotatedTerm' vt v a, ABT.Subst (F vt a a) v a)
unLet1 (ABT.Tm' (Let isTop b (ABT.Abs' subst))) = Just (isTop, b, subst)
unLet1 _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet
  :: AnnotatedTerm2 vt at ap v a
  -> Maybe ([(IsTop, v, AnnotatedTerm2 vt at ap v a)], AnnotatedTerm2 vt at ap v a)
unLet t = fixup (go t)
 where
  go (ABT.Tm' (Let isTop b (ABT.out -> ABT.Abs v t))) = case go t of
    (env, t) -> ((isTop, v, b) : env, t)
  go t = ([], t)
  fixup ([], _) = Nothing
  fixup bst     = Just bst

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRecNamed
  :: AnnotatedTerm2 vt at ap v a
  -> Maybe
       ( IsTop
       , [(v, AnnotatedTerm2 vt at ap v a)]
       , AnnotatedTerm2 vt at ap v a
       )
unLetRecNamed (ABT.Cycle' vs (ABT.Tm' (LetRec isTop bs e)))
  | length vs == length bs = Just (isTop, zip vs bs, e)
unLetRecNamed _ = Nothing

unLetRec
  :: (Monad m, Var v)
  => AnnotatedTerm2 vt at ap v a
  -> Maybe
       (  IsTop
       ,  (v -> m v)
       -> m
            ( [(v, AnnotatedTerm2 vt at ap v a)]
            , AnnotatedTerm2 vt at ap v a
            )
       )
unLetRec (unLetRecNamed -> Just (isTop, bs, e)) = Just
  ( isTop
  , \freshen -> do
    vs <- sequence [ freshen v | (v, _) <- bs ]
    let sub = ABT.substsInheritAnnotation (map fst bs `zip` map ABT.var vs)
    pure (vs `zip` [ sub b | (_, b) <- bs ], sub e)
  )
unLetRec _ = Nothing

unApps
  :: AnnotatedTerm2 vt at ap v a
  -> Maybe (AnnotatedTerm2 vt at ap v a, [AnnotatedTerm2 vt at ap v a])
unApps t = unAppsPred (t, const True)

-- Same as unApps but taking a predicate controlling whether we match on a given function argument.
unAppsPred :: (AnnotatedTerm2 vt at ap v a, AnnotatedTerm2 vt at ap v a -> Bool) ->
                Maybe (AnnotatedTerm2 vt at ap v a, [AnnotatedTerm2 vt at ap v a])
unAppsPred (t, pred) = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc | pred o = go i (o:acc)
  go _ [] = []
  go fn args = fn:args

unBinaryApp :: AnnotatedTerm2 vt at ap v a
            -> Maybe (AnnotatedTerm2 vt at ap v a,
                      AnnotatedTerm2 vt at ap v a,
                      AnnotatedTerm2 vt at ap v a)
unBinaryApp t = case unApps t of
  Just (f, [arg1, arg2]) -> Just (f, arg1, arg2)
  _                      -> Nothing

-- "((a1 `f1` a2) `f2` a3)" becomes "Just ([(a2, f2), (a1, f1)], a3)"
unBinaryApps
  :: AnnotatedTerm2 vt at ap v a
  -> Maybe
       ( [(AnnotatedTerm2 vt at ap v a, AnnotatedTerm2 vt at ap v a)]
       , AnnotatedTerm2 vt at ap v a
       )
unBinaryApps t = unBinaryAppsPred (t, const True)

-- Same as unBinaryApps but taking a predicate controlling whether we match on a given binary function.
unBinaryAppsPred :: (AnnotatedTerm2 vt at ap v a
                    ,AnnotatedTerm2 vt at ap v a -> Bool)
                 -> Maybe ([(AnnotatedTerm2 vt at ap v a,
                             AnnotatedTerm2 vt at ap v a)],
                           AnnotatedTerm2 vt at ap v a)
unBinaryAppsPred (t, pred) = case unBinaryApp t of
  Just (f, x, y) | pred f -> case unBinaryAppsPred (x, pred) of
                               Just (as, xLast) -> Just ((xLast, f) : as, y)
                               Nothing          -> Just ([(x, f)], y)
  _                       -> Nothing

unLams'
  :: AnnotatedTerm2 vt at ap v a -> Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLams' t = unLamsPred' (t, const True)

-- Same as unLams', but always matches.  Returns an empty [v] if the term doesn't start with a
-- lambda extraction.
unLamsOpt' :: AnnotatedTerm2 vt at ap v a -> Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLamsOpt' t = case unLams' t of
  r@(Just _) -> r
  Nothing    -> Just ([], t)

-- Same as unLams', but stops at any variable named `()`, which indicates a
-- delay (`'`) annotation which we want to preserve.
unLamsUntilDelay'
  :: Var v
  => AnnotatedTerm2 vt at ap v a
  -> Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLamsUntilDelay' t = case unLamsPred' (t, (/=) $ Var.named "()") of
  r@(Just _) -> r
  Nothing    -> Just ([], t)

-- Same as unLamsUntilDelay', but only matches if the lambda body is a match
-- expression, where the scrutinee is also the last argument of the lambda
unLamsMatch'
  :: Var v
  => AnnotatedTerm2 vt at ap v a
  -> Maybe ([v], [MatchCase ap (AnnotatedTerm2 vt at ap v a)])
unLamsMatch' t = case unLamsUntilDelay' t of
    Just (reverse -> (v1:vs), Match' (Var' v1') branches) |
      (v1 == v1') && not (Set.member v1' (Set.unions $ freeVars <$> branches)) ->
        Just (reverse vs, branches)
    _ -> Nothing
  where
    freeVars (MatchCase _ g rhs) =
      let guardVars = (fromMaybe Set.empty $ ABT.freeVars <$> g)
          rhsVars = (ABT.freeVars rhs)
      in Set.union guardVars rhsVars

-- Same as unLams' but taking a predicate controlling whether we match on a given binary function.
unLamsPred' :: (AnnotatedTerm2 vt at ap v a, v -> Bool) ->
                 Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLamsPred' (LamNamed' v body, pred) | pred v = case unLamsPred' (body, pred) of
  Nothing -> Just ([v], body)
  Just (vs, body) -> Just (v:vs, body)
unLamsPred' _ = Nothing

unReqOrCtor :: AnnotatedTerm2 vt at ap v a -> Maybe (Reference, Int)
unReqOrCtor (Constructor' r cid) = Just (r, cid)
unReqOrCtor (Request' r cid)     = Just (r, cid)
unReqOrCtor _                         = Nothing

-- Dependencies including referenced data and effect decls
dependencies :: (Ord v, Ord vt) => AnnotatedTerm2 vt at ap v a -> Set Reference
dependencies t = Set.map (LD.fold id Referent.toReference) (labeledDependencies t)

typeDependencies :: (Ord v, Ord vt) => AnnotatedTerm2 vt at ap v a -> Set Reference
typeDependencies =
  Set.fromList . mapMaybe (LD.fold Just (const Nothing)) . toList . labeledDependencies

-- Gets the types to which this term contains references via patterns and
-- data constructors.
constructorDependencies
  :: (Ord v, Ord vt) => AnnotatedTerm2 vt at ap v a -> Set Reference
constructorDependencies =
  Set.unions
    . generalizedDependencies (const mempty)
                              (const mempty)
                              Set.singleton
                              (const . Set.singleton)
                              Set.singleton
                              (const . Set.singleton)
                              Set.singleton

generalizedDependencies
  :: (Ord v, Ord vt, Ord r)
  => (Reference -> r)
  -> (Reference -> r)
  -> (Reference -> r)
  -> (Reference -> ConstructorId -> r)
  -> (Reference -> r)
  -> (Reference -> ConstructorId -> r)
  -> (Reference -> r)
  -> AnnotatedTerm2 vt at ap v a
  -> Set r
generalizedDependencies termRef typeRef literalType dataConstructor dataType effectConstructor effectType
  = Set.fromList . Writer.execWriter . ABT.visit' f where
  f t@(Ref r) = Writer.tell [termRef r] $> t
  f t@(TermLink r) = Writer.tell [termRef $ Referent.toReference r] $> t
  f t@(TypeLink r) = Writer.tell [typeRef r] $> t
  f t@(Ann _ typ) =
    Writer.tell (map typeRef . toList $ Type.dependencies typ) $> t
  f t@(Nat      _) = Writer.tell [literalType Type.natRef] $> t
  f t@(Int      _) = Writer.tell [literalType Type.intRef] $> t
  f t@(Float    _) = Writer.tell [literalType Type.floatRef] $> t
  f t@(Boolean  _) = Writer.tell [literalType Type.booleanRef] $> t
  f t@(Text     _) = Writer.tell [literalType Type.textRef] $> t
  f t@(Sequence _) = Writer.tell [literalType Type.vectorRef] $> t
  f t@(Constructor r cid) =
    Writer.tell [dataType r, dataConstructor r cid] $> t
  f t@(Request r cid) =
    Writer.tell [effectType r, effectConstructor r cid] $> t
  f t@(Match _ cases) = traverse_ goPat cases $> t
  f t                 = pure t
  goPat (MatchCase pat _ _) =
    Writer.tell . toList $ Pattern.generalizedDependencies literalType
                                                           dataConstructor
                                                           dataType
                                                           effectConstructor
                                                           effectType
                                                           pat

labeledDependencies :: (Ord v, Ord vt)
                    => AnnotatedTerm2 vt at ap v a
                    -> Set LabeledDependency
labeledDependencies = generalizedDependencies LD.termRef LD.typeRef LD.typeRef LD.dataConstructor LD.typeRef LD.effectConstructor LD.typeRef

updateDependencies
  :: Ord v
  => Map Reference Reference
  -> Map Reference Reference
  -> AnnotatedTerm v a
  -> AnnotatedTerm v a
updateDependencies termUpdates typeUpdates = ABT.rebuildUp go
 where
  -- todo: this function might need tweaking if we ever allow type replacements
  -- would need to look inside pattern matching and constructor calls
  go (Ref r    ) = Ref (Map.findWithDefault r r termUpdates)
  go (TermLink (Referent.Ref r)) = TermLink (Referent.Ref $ Map.findWithDefault r r termUpdates)
  go (TypeLink r) = TypeLink (Map.findWithDefault r r typeUpdates)
  go (Ann tm tp) = Ann tm $ Type.updateDependencies typeUpdates tp
  go f           = f

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Var v => Term v -> Term v
betaReduce (App' (Lam' f) arg) = ABT.bind f arg
betaReduce e = e

betaNormalForm :: Var v => Term v -> Term v
betaNormalForm (App' f a) = betaNormalForm (betaReduce (app() (betaNormalForm f) a))
betaNormalForm e = e

-- x -> f x => f
etaNormalForm :: Eq v => Term v -> Term v
etaNormalForm (LamNamed' v (App' f (Var' v'))) | v == v' = etaNormalForm f
etaNormalForm t = t

-- This converts `Reference`s it finds that are in the input `Map`
-- back to free variables
unhashComponent :: forall v a. Var v
                => Map Reference (AnnotatedTerm v a)
                -> Map Reference (v, AnnotatedTerm v a)
unhashComponent m = let
  usedVars = foldMap (Set.fromList . ABT.allVars) m
  m' :: Map Reference (v, AnnotatedTerm v a)
  m' = evalState (Map.traverseWithKey assignVar m) usedVars where
    assignVar r t = (,t) <$> ABT.freshenS (Var.refNamed r)
  unhash1 = ABT.rebuildUp' go where
    go e@(Ref' r) = case Map.lookup r m' of
      Nothing -> e
      Just (v, _) -> var (ABT.annotation e) v
    go e = e
  in second unhash1 <$> m'

hashComponents
  :: Var v => Map v (AnnotatedTerm v a) -> Map v (Reference, AnnotatedTerm v a)
hashComponents = ReferenceUtil.hashComponents $ ref ()

-- The hash for a constructor
hashConstructor'
  :: (Reference -> Int -> Term Symbol) -> Reference -> Int -> Reference
hashConstructor' f r cid =
  let
-- this is a bit circuitous, but defining everything in terms of hashComponents
-- ensure the hashing is always done in the same way
      m = hashComponents (Map.fromList [(Var.named "_" :: Symbol, f r cid)])
  in  case toList m of
        [(r, _)] -> r
        _        -> error "unpossible"

hashConstructor :: Reference -> Int -> Reference
hashConstructor = hashConstructor' $ constructor ()

hashRequest :: Reference -> Int -> Reference
hashRequest = hashConstructor' $ request ()

fromReferent :: Ord v
             => a
             -> Referent
             -> AnnotatedTerm2 vt at ap v a
fromReferent a = \case
  Referent.Ref r -> ref a r
  Referent.Con r i ct -> case ct of
    CT.Data -> constructor a r i
    CT.Effect -> request a r i

instance Var v => Hashable1 (F v a p) where
  hash1 hashCycle hash e
    = let (tag, hashed, varint) =
            (Hashable.Tag, Hashable.Hashed, Hashable.Nat . fromIntegral)
      in
        case e of
        -- So long as `Reference.Derived` ctors are created using the same
        -- hashing function as is used here, this case ensures that references
        -- are 'transparent' wrt hash and hashing is unaffected by whether
        -- expressions are linked. So for example `x = 1 + 1` and `y = x` hash
        -- the same.
          Ref (Reference.Derived h 0 1) -> Hashable.fromBytes (Hash.toBytes h)
          Ref (Reference.Derived h i n) -> Hashable.accumulate
            [ tag 1
            , hashed $ Hashable.fromBytes (Hash.toBytes h)
            , Hashable.Nat i
            , Hashable.Nat n
            ]
          -- Note: start each layer with leading `1` byte, to avoid collisions
          -- with types, which start each layer with leading `0`.
          -- See `Hashable1 Type.F`
          _ ->
            Hashable.accumulate
              $ tag 1
              : case e of
                  Nat     i -> [tag 64, accumulateToken i]
                  Int     i -> [tag 65, accumulateToken i]
                  Float   n -> [tag 66, Hashable.Double n]
                  Boolean b -> [tag 67, accumulateToken b]
                  Text    t -> [tag 68, accumulateToken t]
                  Char    c -> [tag 69, accumulateToken c]
                  Blank   b -> tag 1 : case b of
                    B.Blank -> [tag 0]
                    B.Recorded (B.Placeholder _ s) ->
                      [tag 1, Hashable.Text (Text.pack s)]
                    B.Recorded (B.Resolve _ s) ->
                      [tag 2, Hashable.Text (Text.pack s)]
                  Ref (Reference.Builtin name) -> [tag 2, accumulateToken name]
                  Ref Reference.Derived {} ->
                    error "handled above, but GHC can't figure this out"
                  App a a2  -> [tag 3, hashed (hash a), hashed (hash a2)]
                  Ann a t   -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                  Sequence as -> tag 5 : varint (Sequence.length as) : map
                    (hashed . hash)
                    (toList as)
                  Lam a         -> [tag 6, hashed (hash a)]
                  -- note: we use `hashCycle` to ensure result is independent of
                  -- let binding order
                  LetRec _ as a -> case hashCycle as of
                    (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
                  -- here, order is significant, so don't use hashCycle
                  Let _ b a -> [tag 8, hashed $ hash b, hashed $ hash a]
                  If b t f ->
                    [tag 9, hashed $ hash b, hashed $ hash t, hashed $ hash f]
                  Request     r n -> [tag 10, accumulateToken r, varint n]
                  Constructor r n -> [tag 12, accumulateToken r, varint n]
                  Match e branches ->
                    tag 13 : hashed (hash e) : concatMap h branches
                   where
                    h (MatchCase pat guard branch) = concat
                      [ [accumulateToken pat]
                      , toList (hashed . hash <$> guard)
                      , [hashed (hash branch)]
                      ]
                  Handle h b -> [tag 15, hashed $ hash h, hashed $ hash b]
                  And    x y -> [tag 16, hashed $ hash x, hashed $ hash y]
                  Or     x y -> [tag 17, hashed $ hash x, hashed $ hash y]
                  TermLink r -> [tag 18, accumulateToken r]
                  TypeLink r -> [tag 19, accumulateToken r]
                  _ ->
                    error $ "unhandled case in hash: " <> show (void e)

-- mostly boring serialization code below ...

instance (Eq a, ABT.Var v) => Eq1 (F v a p) where (==#) = (==)
instance (Show v) => Show1 (F v a p) where showsPrec1 = showsPrec

instance (ABT.Var vt, Eq at, Eq a) => Eq (F vt at p a) where
  Int x == Int y = x == y
  Nat x == Nat y = x == y
  Float x == Float y = x == y
  Boolean x == Boolean y = x == y
  Text x == Text y = x == y
  Char x == Char y = x == y
  Blank b == Blank q = b == q
  Ref x == Ref y = x == y
  TermLink x == TermLink y = x == y
  TypeLink x == TypeLink y = x == y
  Constructor r cid == Constructor r2 cid2 = r == r2 && cid == cid2
  Request r cid == Request r2 cid2 = r == r2 && cid == cid2
  Handle h b == Handle h2 b2 = h == h2 && b == b2
  App f a == App f2 a2 = f == f2 && a == a2
  Ann e t == Ann e2 t2 = e == e2 && t == t2
  Sequence v == Sequence v2 = v == v2
  If a b c == If a2 b2 c2 = a == a2 && b == b2 && c == c2
  And a b == And a2 b2 = a == a2 && b == b2
  Or a b == Or a2 b2 = a == a2 && b == b2
  Lam a == Lam b = a == b
  LetRec _ bs body == LetRec _ bs2 body2 = bs == bs2 && body == body2
  Let _ binding body == Let _ binding2 body2 =
    binding == binding2 && body == body2
  Match scrutinee cases == Match s2 cs2 = scrutinee == s2 && cases == cs2
  _ == _ = False


instance (Show v, Show a) => Show (F v a0 p a) where
  showsPrec = go
   where
    showConstructor r n = shows r <> s "#" <> shows n
    go _ (Int     n    ) = (if n >= 0 then s "+" else s "") <> shows n
    go _ (Nat     n    ) = shows n
    go _ (Float   n    ) = shows n
    go _ (Boolean True ) = s "true"
    go _ (Boolean False) = s "false"
    go p (Ann t k) = showParen (p > 1) $ shows t <> s ":" <> shows k
    go p (App f x) = showParen (p > 9) $ showsPrec 9 f <> s " " <> showsPrec 10 x
    go _ (Lam    body  ) = showParen True (s "λ " <> shows body)
    go _ (Sequence vs    ) = showListWith shows (toList vs)
    go _ (Blank  b     ) = case b of
      B.Blank                        -> s "_"
      B.Recorded (B.Placeholder _ r) -> s ("_" ++ r)
      B.Recorded (B.Resolve     _ r) -> s r
    go _ (Ref r) = s "Ref(" <> shows r <> s ")"
    go _ (TermLink r) = s "TermLink(" <> shows r <> s ")"
    go _ (TypeLink r) = s "TypeLink(" <> shows r <> s ")"
    go _ (Let _ b body) =
      showParen True (s "let " <> shows b <> s " in " <> shows body)
    go _ (LetRec _ bs body) = showParen
      True
      (s "let rec" <> shows bs <> s " in " <> shows body)
    go _ (Handle b body) = showParen
      True
      (s "handle " <> shows b <> s " in " <> shows body)
    go _ (Constructor r         n    ) = showConstructor r n
    go _ (Match       scrutinee cases) = showParen
      True
      (s "case " <> shows scrutinee <> s " of " <> shows cases)
    go _ (Text s     ) = shows s
    go _ (Char c     ) = shows c
    go _ (Request r n) = showConstructor r n
    go p (If c t f) =
      showParen (p > 0)
        $  s "if "
        <> shows c
        <> s " then "
        <> shows t
        <> s " else "
        <> shows f
    go p (And x y) =
      showParen (p > 0) $ s "and " <> shows x <> s " " <> shows y
    go p (Or x y) =
      showParen (p > 0) $ s "or " <> shows x <> s " " <> shows y
    (<>) = (.)
    s    = showString
