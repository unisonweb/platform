{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Typechecker.Context
  ( synthesizeClosed,
    ErrorNote (..),
    CompilerBug (..),
    InfoNote (..),
    Cause (..),
    Context (..),
    ActualArgCount,
    ExpectedArgCount,
    ConstructorId,
    Element (..),
    PathElement (..),
    Term,
    Type,
    TypeVar,
    Result (..),
    PatternMatchCoverageCheckAndKindInferenceSwitch (..),
    errorTerms,
    innermostErrorTerm,
    lookupAnn,
    lookupSolved,
    apply,
    isEqual,
    isSubtype,
    fitsScheme,
    isRedundant,
    Suggestion (..),
    Replacement (..),
    SuggestionMatch (..),
    isExact,
    typeErrors,
    infoNotes,
    Unknown (..),
    relax,
    generalizeAndUnTypeVar,
  )
where

import Control.Lens (_2)
import Control.Monad.Fail qualified as MonadFail
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State
  ( MonadState,
    StateT,
    evalState,
    evalStateT,
    get,
    gets,
    put,
    runStateT,
  )
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.Functor.Compose
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as Nel
import Data.Map qualified as Map
import Data.Monoid (Ap (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Blank qualified as B
import Unison.Builtin.Decls qualified as DDB
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.ConstructorReference
  ( ConstructorReference,
    GConstructorReference (..),
    reference_,
  )
import Unison.DataDeclaration
  ( DataDeclaration,
    EffectDeclaration,
  )
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.KindInference qualified as KindInference
import Unison.Name (Name)
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.PatternMatchCoverage (checkMatch)
import Unison.PatternMatchCoverage.Class (EnumeratedConstructors (..), Pmc, traverseConstructorTypes)
import Unison.PatternMatchCoverage.Class qualified as Pmc
import Unison.PatternMatchCoverage.ListPat qualified as ListPat
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Syntax.TypePrinter qualified as TP
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Components (minimize')
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)
import Unison.Var qualified as Var

type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v

type Type v loc = Type.Type (TypeVar v loc) loc

type Term v loc = Term.Term' (TypeVar v loc) v loc

type Monotype v loc = Type.Monotype (TypeVar v loc) loc

type RedundantTypeAnnotation = Bool

type Wanted v loc = [(Maybe (Term v loc), Type v loc)]

pattern Universal :: v -> Element v loc
pattern Universal v = Var (TypeVar.Universal v)

pattern Existential :: B.Blank loc -> v -> Element v loc
pattern Existential b v <- Var (TypeVar.Existential b v)

existential :: v -> Element v loc
existential v = Var (TypeVar.Existential B.Blank v)

existential' :: (Ord v) => a -> B.Blank loc -> v -> Type.Type (TypeVar v loc) a
existential' a blank v = ABT.annotatedVar a (TypeVar.Existential blank v)

existentialp :: (Ord v) => a -> v -> Type v a
existentialp a = existential' a B.Blank

universal' :: (Ord v) => a -> v -> Type.Type (TypeVar v loc) a
universal' a v = ABT.annotatedVar a (TypeVar.Universal v)

-- | Elements of an ordered algorithmic context
data Element v loc
  = -- | A variable declaration
    Var (TypeVar v loc)
  | -- | `v` is solved to some monotype
    Solved (B.Blank loc) v (Monotype v loc)
  | -- | `v` has type `a`, maybe quantified
    Ann v (Type v loc)
  | -- | used for scoping
    Marker v

instance (Ord loc, Var v) => Eq (Element v loc) where
  Var v == Var v2 = v == v2
  Solved _ v t == Solved _ v2 t2 = v == v2 && t == t2
  Ann v t == Ann v2 t2 = v == v2 && t == t2
  Marker v == Marker v2 = v == v2
  _ == _ = False

-- The typechecking state
data Env v loc = Env {freshId :: Word64, ctx :: Context v loc}

type DataDeclarations v loc = Map Reference (DataDeclaration v loc)

type EffectDeclarations v loc = Map Reference (EffectDeclaration v loc)

data Result v loc a
  = Success !(Seq (InfoNote v loc)) !a
  | TypeError !(NESeq (ErrorNote v loc)) !(Seq (InfoNote v loc))
  | CompilerBug
      !(CompilerBug v loc)
      !(Seq (ErrorNote v loc)) -- type errors before hitting the bug
      !(Seq (InfoNote v loc)) -- info notes before hitting the bug
  deriving (Functor)

instance Applicative (Result v loc) where
  pure = Success mempty
  CompilerBug bug es is <*> _ = CompilerBug bug es is
  r <*> CompilerBug bug es' is' = CompilerBug bug (typeErrors r <> es') (infoNotes r <> is')
  TypeError es is <*> r' = TypeError (es NESeq.|>< (typeErrors r')) (is <> infoNotes r')
  Success is _ <*> TypeError es' is' = TypeError es' (is <> is')
  Success is f <*> Success is' a = Success (is <> is') (f a)
  {-# INLINE (<*>) #-}

instance Monad (Result v loc) where
  s@(Success _ a) >>= f = s *> f a
  TypeError es is >>= _ = TypeError es is
  CompilerBug bug es is >>= _ = CompilerBug bug es is
  {-# INLINE (>>=) #-}

instance MonadFix (Result v loc) where
  mfix f =
    let res = f theA
        theA = case res of
          Success _ a -> a
          _ -> error "mfix Result: forced an unsuccessful value"
     in res

btw' :: InfoNote v loc -> Result v loc ()
btw' note = Success (Seq.singleton note) ()

typeError :: Cause v loc -> Result v loc a
typeError cause = TypeError (pure $ ErrorNote cause mempty) mempty

compilerBug :: CompilerBug v loc -> Result v loc a
compilerBug bug = CompilerBug bug mempty mempty

typeErrors :: Result v loc a -> Seq (ErrorNote v loc)
typeErrors = \case
  TypeError es _ -> NESeq.toSeq es
  CompilerBug _ es _ -> es
  Success _ _ -> mempty

infoNotes :: Result v loc a -> Seq (InfoNote v loc)
infoNotes = \case
  TypeError _ is -> is
  CompilerBug _ _ is -> is
  Success is _ -> is

mapErrors :: (ErrorNote v loc -> ErrorNote v loc) -> Result v loc a -> Result v loc a
mapErrors f r = case r of
  TypeError es is -> TypeError (f <$> es) is
  CompilerBug bug es is -> CompilerBug bug (f <$> es) is
  s@(Success _ _) -> s

-- Allows modifying the stored notes in a scoped way.
-- This is based on the `pass` function in e.g. Control.Monad.Writer
adjustResultNotes ::
  Result v loc (a, InfoNote v loc -> InfoNote v loc) ->
  Result v loc a
adjustResultNotes (Success notes (r, f)) = Success (fmap f notes) r
adjustResultNotes (TypeError e i) = TypeError e i
adjustResultNotes (CompilerBug c e i) = CompilerBug c e i

data PatternMatchCoverageCheckAndKindInferenceSwitch
  = PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
  | PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled

newtype MT v loc f a = MT
  { runM ::
      -- for debug output
      PrettyPrintEnv ->
      PatternMatchCoverageCheckAndKindInferenceSwitch ->
      -- Data declarations in scope
      DataDeclarations v loc ->
      -- Effect declarations in scope
      EffectDeclarations v loc ->
      Env v loc ->
      f (a, Env v loc)
  }
  deriving stock (Functor)

-- | Typechecking monad
type M v loc = MT v loc (Result v loc)

-- | Typechecking computation that, unless it crashes
-- with a compiler bug, always produces a value.
type TotalM v loc = MT v loc (Either (CompilerBug v loc))

liftResult :: Result v loc a -> M v loc a
liftResult r = MT (\_ _ _ _ env -> (,env) <$> r)

liftTotalM :: TotalM v loc a -> M v loc a
liftTotalM (MT m) = MT $ \ppe pmcSwitch datas effects env -> case m ppe pmcSwitch datas effects env of
  Left bug -> CompilerBug bug mempty mempty
  Right a -> Success mempty a

-- Allows modifying the stored notes in a scoped way.
-- This is based on the `pass` function in e.g. Control.Monad.Writer
adjustNotes ::
  M v loc (a, InfoNote v loc -> InfoNote v loc) -> M v loc a
adjustNotes (MT m) = MT $ \ppe pmcSwitch datas effects env ->
  adjustResultNotes (twiddle <$> m ppe pmcSwitch datas effects env)
  where
    twiddle ((a, c), b) = ((a, b), c)

-- errorNote :: Cause v loc -> M v loc ()
-- errorNote = liftResult . errorNote

btw :: InfoNote v loc -> M v loc ()
btw = liftResult . btw'

modEnv :: (Env v loc -> Env v loc) -> M v loc ()
modEnv f = modEnv' $ ((),) . f

modEnv' :: (Env v loc -> (a, Env v loc)) -> M v loc a
modEnv' f = MT (\_ _ _ _ env -> pure . f $ env)

data Unknown = Data | Effect deriving (Show)

data CompilerBug v loc
  = UnknownDecl Unknown Reference (Map Reference (DataDeclaration v loc))
  | UnknownConstructor Unknown ConstructorReference (DataDeclaration v loc)
  | UndeclaredTermVariable v (Context v loc)
  | RetractFailure (Element v loc) (Context v loc)
  | EmptyLetRec (Term v loc) -- the body of the empty let rec
  | PatternMatchFailure
  | EffectConstructorHadMultipleEffects (Type v loc)
  | FreeVarsInTypeAnnotation (Set (TypeVar v loc))
  | UnannotatedReference Reference
  | MalformedPattern (Pattern loc)
  | UnknownTermReference Reference
  | UnknownExistentialVariable v (Context v loc)
  | -- `IllegalContextExtension ctx elem msg`
    --     extending `ctx` with `elem` would make `ctx` ill-formed, as explained by `msg`
    IllegalContextExtension (Context v loc) (Element v loc) String
  | OtherBug String
  deriving (Show)

data PathElement v loc
  = InSynthesize (Term v loc)
  | InSubtype (Type v loc) (Type v loc)
  | InEquate (Type v loc) (Type v loc)
  | InCheck (Term v loc) (Type v loc)
  | InInstantiateL v (Type v loc)
  | InInstantiateR (Type v loc) v
  | InSynthesizeApp (Type v loc) (Term v loc) Int
  | InFunctionCall [v] (Term v loc) (Type v loc) [Term v loc]
  | InAndApp
  | InOrApp
  | InIfCond
  | InIfBody loc -- location of `then` expression
  | InVectorApp loc -- location of 1st vector element
  | InMatch loc -- location of 1st case body
  | InMatchGuard
  | InMatchBody
  deriving (Show)

type ExpectedArgCount = Int

type ActualArgCount = Int

data SuggestionMatch = Exact | WrongType | WrongName
  deriving (Ord, Eq, Show)

data Suggestion v loc = Suggestion
  { suggestionName :: Name,
    suggestionType :: Type v loc,
    suggestionReplacement :: Replacement v,
    suggestionMatch :: SuggestionMatch
  }
  deriving stock (Eq, Show)

isExact :: Suggestion v loc -> Bool
isExact Suggestion {..} = suggestionMatch == Exact

data Replacement v
  = ReplacementRef Referent
  | ReplacementVar v
  deriving stock (Eq, Ord, Show)

data ErrorNote v loc = ErrorNote
  { cause :: Cause v loc,
    path :: Seq (PathElement v loc)
  }
  deriving (Show)

-- `Decision v loc fqn` is a decision to replace the name v at location loc
-- with the fully qualified name fqn.
data InfoNote v loc
  = SolvedBlank (B.Recorded loc) v (Type v loc)
  | Decision v loc (Term.Term v loc)
  | TopLevelComponent [(v, Type.Type v loc, RedundantTypeAnnotation)]
  deriving (Show)

topLevelComponent :: (Var v) => [(v, Type.Type v loc, RedundantTypeAnnotation)] -> InfoNote v loc
topLevelComponent = TopLevelComponent . fmap (over _2 removeSyntheticTypeVars)

-- Given a list of Elements that are going to be discarded from a
-- context, substitutes the informataion into the solved blank types
-- of an InfoNote. This should give better TDNR results, because it
-- allows the stored solutions to incorporate information from later
-- in the type checking process, instead of it being entirely reliant
-- on information in the local scope of the reference to be resolved.
--
-- Note: this does not take any care to abstract over the variables
-- stored in the notes, so it is _heavily_ reliant on the fact that we
-- never reuse variable names/numberings in the typechecker. If this
-- becomes untrue, then we need to revisit this and instead properly
-- generalize types stored in the notes.
substituteSolved ::
  (Var v, Ord loc) =>
  Map v (Monotype v loc) ->
  InfoNote v loc ->
  InfoNote v loc
substituteSolved ctx (SolvedBlank b v t) =
  SolvedBlank b v (applyCtx ctx t)
substituteSolved _ i = i

-- The typechecker generates synthetic type variables as part of type inference.
-- This function converts these synthetic type variables to regular named type
-- variables guaranteed to not collide with any other type variables.
--
-- It also attempts to pick "nice" type variable names, based on what sort of
-- synthetic type variable it is and what type variable names are not already
-- being used.
removeSyntheticTypeVars :: (Var v) => Type.Type v loc -> Type.Type v loc
removeSyntheticTypeVars typ =
  flip evalState (Set.fromList (ABT.allVars typ), mempty) $ ABT.vmapM go typ
  where
    go v
      | Var.User _ <- Var.typeOf v = pure v -- user-provided type variables left alone
      | otherwise = do
          (used, curMappings) <- get
          case Map.lookup v curMappings of
            Nothing -> do
              let v' = pickName used (Var.typeOf v)
              put (Set.insert v' used, Map.insert v v' curMappings)
              pure v'
            Just v' -> pure v'
    pickName used vt = ABT.freshIn used . Var.named $ case vt of
      -- for each type of variable, we have some preferred variable
      -- names that we like, if they aren't already being used
      Var.Inference Var.Ability -> pick ["g", "h", "m", "p"]
      Var.Inference Var.Input -> pick ["a", "b", "c", "i", "j"]
      Var.Inference Var.Output -> pick ["r", "o"]
      Var.Inference Var.Other -> pick ["t", "u", "w"]
      Var.Inference Var.TypeConstructor -> pick ["f", "k", "d"]
      Var.Inference Var.TypeConstructorArg -> pick ["v", "w", "y"]
      Var.User n -> n
      _ -> defaultName
      where
        used1CharVars =
          Set.fromList $
            ABT.allVars typ >>= \v ->
              case Text.unpack (Var.name . Var.reset $ v) of
                [ch] -> [Text.singleton ch]
                _ -> []
        pick ns@(n : _) = fromMaybe n $ find (`Set.notMember` used1CharVars) ns
        pick [] = error "impossible"
        defaultName = "x"

data Cause v loc
  = TypeMismatch (Context v loc)
  | IllFormedType (Context v loc)
  | UnknownSymbol loc v
  | UnknownTerm loc v [Suggestion v loc] (Type v loc)
  | AbilityCheckFailure [Type v loc] [Type v loc] (Context v loc) -- ambient, requested
  | AbilityEqFailure [Type v loc] [Type v loc] (Context v loc)
  | EffectConstructorWrongArgCount ExpectedArgCount ActualArgCount ConstructorReference
  | MalformedEffectBind (Type v loc) (Type v loc) [Type v loc] -- type of ctor, type of ctor result
  -- Type of ctor, number of arguments we got
  | PatternArityMismatch loc (Type v loc) Int
  | -- A variable is defined twice in the same block
    DuplicateDefinitions (NonEmpty (v, [loc]))
  | -- A let rec where things that aren't guarded cyclicly depend on each other
    UnguardedLetRecCycle [v] [(v, Term v loc)]
  | ConcatPatternWithoutConstantLength loc (Type v loc)
  | HandlerOfUnexpectedType loc (Type v loc)
  | DataEffectMismatch Unknown Reference (DataDeclaration v loc)
  | UncoveredPatterns loc (NonEmpty (Pattern ()))
  | RedundantPattern loc
  | KindInferenceFailure (KindInference.KindError v loc)
  | InaccessiblePattern loc
  deriving (Show)

errorTerms :: ErrorNote v loc -> [Term v loc]
errorTerms n =
  Foldable.toList (path n) >>= \e -> case e of
    InCheck e _ -> [e]
    InSynthesizeApp _ e _ -> [e]
    InSynthesize e -> [e]
    _ -> []

innermostErrorTerm :: ErrorNote v loc -> Maybe (Term v loc)
innermostErrorTerm n = listToMaybe $ errorTerms n

solveBlank :: B.Recorded loc -> v -> Type v loc -> M v loc ()
solveBlank blank v typ = btw $ SolvedBlank blank v typ

-- Add `p` onto the end of the `path` of this `ErrorNote`
scope' :: PathElement v loc -> ErrorNote v loc -> ErrorNote v loc
scope' p (ErrorNote cause path) = ErrorNote cause (path `mappend` pure p)

-- Add `p` onto the end of the `path` of any `ErrorNote`s emitted by the action
scope :: PathElement v loc -> M v loc a -> M v loc a
scope p (MT m) = MT \ppe pmcSwitch datas effects env -> mapErrors (scope' p) (m ppe pmcSwitch datas effects env)

data Context v loc = Context [(Element v loc, Info v loc)] (Map v (Monotype v loc))

data Info v loc = Info
  { existentialVars :: Set v, -- set of existentials seen so far
    solvedExistentials :: Map v (Monotype v loc), -- `v` is solved to some monotype
    universalVars :: Set v, -- set of universals seen so far
    termVarAnnotations :: Map v (Type v loc),
    allVars :: Set v -- all variables seen so far
  }

-- | The empty context
context0 :: Ord v => Context v loc
context0 = Context [] mempty

context :: Ord v => [(Element v loc, Info v loc)] -> Context v loc
context elems = Context elems (computeSolvedExistentials (fst <$> elems))

computeSolvedExistentials :: Ord a => [Element a loc] -> Map a (Monotype a loc)
computeSolvedExistentials elems =
      elems & mapMaybe (\case
        Solved _ v t -> Just (v, t)
        _ -> Nothing
                       )
           & Map.fromList

occursAnn :: (Var v) => (Ord loc) => TypeVar v loc -> Context v loc -> Bool
occursAnn v (Context eis solved) = any p es
  where
    es = fst <$> eis
    p (Ann _ ty) = v `Set.member` ABT.freeVars (applyCtx solved ty)
    p _ = False

-- | Focuses on the first element in the list that satisfies the predicate.
-- Returns `(prefix, focusedElem, suffix)`, where `prefix` is in reverse order.
focusAt :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
focusAt p xs = go [] xs
  where
    go _ [] = Nothing
    go l (h : t) = if p h then Just (l, h, t) else go (h : l) t

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Nothing` if the element is not found.
retract0 :: (Var v, Ord loc) => Element v loc -> Context v loc -> Maybe (Context v loc, [Element v loc])
retract0 e (Context ctx _) = case focusAt (\(e', _) -> e' == e) ctx of
  Just (discarded, _, remaining) ->
    -- note: no need to recompute used variables; any suffix of the
    -- context snoc list is also a valid context
    Just (context remaining, map fst discarded)
  Nothing -> Nothing

-- | Adds a marker to the end of the context, runs the `body` and then discards
-- from the end of the context up to and including the marker. Returns the result
-- of `body` and the discarded context (not including the marker), respectively.
-- Freshened `markerHint` is used to create the marker.
markThenRetract :: (Var v, Ord loc) => v -> M v loc a -> M v loc (a, [Element v loc], Map v (Monotype v loc))
markThenRetract hint body =
  markThenCallWithRetract hint \retract -> adjustNotes do
    r <- body
    (ctx, solved) <- retract
    pure ((r, ctx, solved), substituteSolved solved)

markThenRetract0 :: (Var v, Ord loc) => v -> M v loc a -> M v loc ()
markThenRetract0 markerHint body = () <$ markThenRetract markerHint body

markThenCallWithRetract ::
  (Var v, Ord loc) =>
  v ->
  (M v loc ([Element v loc], Map v (Monotype v loc)) -> M v loc a) ->
  M v loc a
markThenCallWithRetract hint k = do
  v <- freshenVar hint
  extendContext (Marker v)
  k (doRetract (Marker v))
  where
    doRetract :: (Var v, Ord loc) => Element v loc -> M v loc ([Element v loc], Map v (Monotype v loc))
    doRetract e = do
      ctx <- getContext
      case retract0 e ctx of
        Nothing -> compilerCrash (RetractFailure e ctx)
        Just (t, discarded) -> do
          let solved =
                [ (b, v, inst $ Type.getPolytype sa)
                  | Solved (B.Recorded b) v sa <- discarded
                ]
              unsolved =
                [ (b, v, inst $ existential' (B.loc b) b' v)
                  | Existential b'@(B.Recorded b) v <- discarded
                ]
              go (b, v, sa) = solveBlank b v sa
              inst = apply ctx
          Foldable.traverse_ go (solved ++ unsolved)
          setContext t
          pure (discarded, computeSolvedExistentials discarded)

-- unsolved' :: Context v loc -> [(B.Blank loc, v)]
-- unsolved' (Context ctx) = [(b,v) | (Existential b v, _) <- ctx]

replace :: (Var v, Ord loc) => Element v loc -> [Element v loc] -> Context v loc -> M v loc (Context v loc)
replace e focus ctx =
  case breakAt e ctx of
    Just (l, _, r) -> l `extendN` (focus <> r)
    Nothing -> pure ctx

breakAt ::
  (Var v, Ord loc) =>
  Element v loc ->
  Context v loc ->
  Maybe (Context v loc, Element v loc, [Element v loc])
breakAt m (Context xs _) =
  case focusAt (\(e, _) -> e === m) xs of
    Just (r, m, l) ->
      -- l is a suffix of xs and is already a valid context
      Just (context l, fst m, map fst r)
    Nothing -> Nothing
  where
    Existential _ v === Existential _ v2 | v == v2 = True
    Universal v === Universal v2 | v == v2 = True
    Marker v === Marker v2 | v == v2 = True
    _ === _ = False

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: (Var v, Ord loc) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = Set.member v (existentials (retract' (existential v2) ctx))
  where
    -- Like `retract`, but returns the empty context if retracting would remove
    -- all elements.
    retract' ::
      (Var v, Ord loc) => Element v loc -> Context v loc -> Context v loc
    retract' e ctx = maybe context0 fst $ retract0 e ctx

-- env0 :: Env v loc
-- env0 = Env 0 context0

debugEnabled :: Bool
debugEnabled = False

debugShow :: (Show a) => a -> Bool
debugShow e | debugEnabled = traceShow e False
debugShow _ = False

debugTrace :: String -> Bool
debugTrace e | debugEnabled = trace e False
debugTrace _ = False

showType :: (Var v) => Type.Type v a -> String
showType ty = TP.prettyStr (Just 120) PPE.empty ty

debugType :: (Var v) => String -> Type.Type v a -> Bool
debugType tag ty
  | debugEnabled = debugTrace $ "(" <> show tag <> "," <> showType ty <> ")"
  | otherwise = False

debugTypes :: (Var v) => String -> Type.Type v a -> Type.Type v a -> Bool
debugTypes tag t1 t2
  | debugEnabled = debugTrace $ "(" <> show tag <> ",\n  " <> showType t1 <> ",\n  " <> showType t2 <> ")"
  | otherwise = False

debugPatternsEnabled :: Bool
debugPatternsEnabled = False

_logContext :: (Ord loc, Var v) => String -> M v loc ()
_logContext msg = when debugEnabled $ do
  ctx <- getContext
  let !_ = trace ("\n" ++ msg ++ ": " ++ show ctx) ()
  setContext ctx

usedVars :: (Ord v) => Context v loc -> Set v
usedVars = allVars . info

getContext :: M v loc (Context v loc)
getContext = gets ctx

setContext :: Context v loc -> M v loc ()
setContext ctx = modEnv (\e -> e {ctx = ctx})

modifyContext :: (Context v loc -> M v loc (Context v loc)) -> M v loc ()
modifyContext f = do
  c <- getContext
  c <- f c
  setContext c

appendContext :: (Var v, Ord loc) => [Element v loc] -> M v loc ()
appendContext = traverse_ extendContext

markRetained :: (Var v, Ord loc) => Set v -> M v loc ()
markRetained keep = setContext . marks =<< getContext
  where
    marks (Context eis _) = context (fmap mark eis)
    mark (Existential B.Blank v, i)
      | v `Set.member` keep = (Var (TypeVar.Existential B.Retain v), i)
    mark (Solved B.Blank v t, i)
      | v `Set.member` keep = (Solved B.Retain v t, i)
    mark p = p

extendContext :: (Var v) => Element v loc -> M v loc ()
extendContext e =
  isReserved (varOf e) >>= \case
    True -> modifyContext (extend e)
    False ->
      getContext >>= \ctx ->
        compilerCrash $
          IllegalContextExtension ctx e $
            "Extending context with a variable that is not reserved by the typechecking environment."
              <> " That means `freshenVar` is allowed to return it as a fresh variable, which would be wrong."

replaceContext :: (Var v, Ord loc) => Element v loc -> [Element v loc] -> M v loc ()
replaceContext elem replacement = do
  env <- get
  case find (not . (`isReservedIn` env) . varOf) replacement of
    Nothing -> modifyContext (replace elem replacement)
    Just e ->
      getContext >>= \ctx ->
        compilerCrash $
          IllegalContextExtension ctx e $
            "Extending context with a variable that is not reserved by the typechecking environment."
              <> " That means `freshenVar` is allowed to return it as a fresh variable, which would be wrong."

varOf :: Element v loc -> v
varOf (Var tv) = TypeVar.underlying tv
varOf (Solved _ v _) = v
varOf (Ann v _) = v
varOf (Marker v) = v

isReserved :: (Var v) => v -> M v loc Bool
isReserved v = (v `isReservedIn`) <$> get

isReservedIn :: (Var v) => v -> Env v loc -> Bool
isReservedIn v e = freshId e > Var.freshId v

universals :: (Ord v) => Context v loc -> Set v
universals = universalVars . info

existentials :: (Ord v) => Context v loc -> Set v
existentials = existentialVars . info

-- | "Reserves" the given variables in this typechecking environment,
-- i.e. ensures that they won't be returned from `freshenVar` as fresh.
reserveAll :: (Var v, Foldable t) => t v -> M v loc ()
reserveAll vs =
  let maxId = foldr (max . Var.freshId) 0 vs
   in modEnv (\e -> e {freshId = freshId e `max` maxId + 1})

freshenVar :: (Var v) => v -> M v0 loc v
freshenVar v =
  modEnv'
    ( \e ->
        let id = freshId e in (Var.freshenId id v, e {freshId = freshId e + 1})
    )

freshenTypeVar :: (Var v) => TypeVar v loc -> M v loc v
freshenTypeVar v =
  modEnv'
    ( \e ->
        let id = freshId e
         in (Var.freshenId id (TypeVar.underlying v), e {freshId = id + 1})
    )

-- todo: do we want this to return a location for the aspect of the type that was not well formed
-- todo: or maybe a note / list of notes, or an M

-- | Check that the type is well formed wrt the given `Context`, see Figure 7 of paper
wellformedType :: (Var v) => Context v loc -> Type v loc -> Bool
wellformedType c t = case t of
  Type.Var' (TypeVar.Existential _ v) -> Set.member v (existentials c)
  Type.Var' (TypeVar.Universal v) -> Set.member v (universals c)
  Type.Ref' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Effect1' e a -> wellformedType c e && wellformedType c a
  Type.Effects' es -> all (wellformedType c) es
  Type.IntroOuterNamed' _ t -> wellformedType c t
  Type.Forall' t' ->
    let (v, ctx2) = extendUniversal c
     in wellformedType ctx2 (ABT.bind t' (universal' (ABT.annotation t) v))
  _ -> error $ "Match failure in wellformedType: " ++ show t
  where
    -- Extend this `Context` with a single variable, guaranteed fresh
    extendUniversal ctx =
      let v = Var.freshIn (usedVars ctx) (Var.named "var")
          ctx' = fromRight (error "wellformedType: Expected Right") $ extend' (Universal v) ctx
       in (v, ctx')

-- | Return the `Info` associated with the last element of the context, or the zero `Info`.
info :: (Ord v) => Context v loc -> Info v loc
info (Context [] _) = Info mempty mempty mempty mempty mempty
info (Context ((_, i) : _) _) = i

-- | Add an element onto the end of this `Context`. Takes `O(log N)` time,
-- including updates to the accumulated `Info` value.
-- Fail if the new context is not well formed (see Figure 7 of paper).
extend' :: (Var v) => Element v loc -> Context v loc -> Either (CompilerBug v loc) (Context v loc)
extend' e c@(Context ctx _) = context . (: ctx) . (e,) <$> i'
  where
    Info es ses us uas vs = info c
    -- see figure 7
    i' = case e of
      Var v -> case v of
        -- UvarCtx - ensure no duplicates
        TypeVar.Universal v ->
          if Set.notMember v vs
            then pure $ Info es ses (Set.insert v us) uas (Set.insert v vs)
            else crash $ "variable " <> show v <> " already defined in the context"
        -- EvarCtx - ensure no duplicates, and that this existential is not solved earlier in context
        TypeVar.Existential _ v ->
          if Set.notMember v vs
            then pure $ Info (Set.insert v es) ses us uas (Set.insert v vs)
            else crash $ "variable " <> show v <> " already defined in the context"
      -- SolvedEvarCtx - ensure `v` is fresh, and the solution is well-formed wrt the context
      Solved _ v sa@(Type.getPolytype -> t)
        | Set.member v vs -> crash $ "variable " <> show v <> " already defined in the context"
        | not (wellformedType c t) -> crash $ "type " <> show t <> " is not well-formed wrt the context"
        | otherwise ->
            pure $
              Info (Set.insert v es) (Map.insert v sa ses) us uas (Set.insert v vs)
      -- VarCtx - ensure `v` is fresh, and annotation is well-formed wrt the context
      Ann v t
        | Set.member v vs -> crash $ "variable " <> show v <> " already defined in the context"
        | not (wellformedType c t) -> crash $ "type " <> show t <> " is not well-formed wrt the context"
        | otherwise ->
            pure $
              Info
                es
                ses
                us
                (Map.insert v t uas)
                (Set.insert v vs)
      -- MarkerCtx - note that since a Marker is always the first mention of a variable, suffices to
      -- just check that `v` is not previously mentioned
      Marker v ->
        if Set.notMember v vs
          then pure $ Info es ses us uas (Set.insert v vs)
          else crash $ "marker variable " <> show v <> " already defined in the context"
    crash reason = Left $ IllegalContextExtension c e reason

extend :: (Var v) => Element v loc -> Context v loc -> M v loc (Context v loc)
extend e c = either compilerCrash pure $ extend' e c

-- | Add the given elements onto the end of the given `Context`.
-- Fail if the new context is not well-formed.
extendN :: (Var v) => Context v loc -> [Element v loc] -> M v loc (Context v loc)
extendN ctx es = foldM (flip extend) ctx es

-- | doesn't combine notes
orElse :: M v loc a -> M v loc a -> M v loc a
orElse m1 m2 = MT go
  where
    go ppe pmcSwitch datas effects env = runM m1 ppe pmcSwitch datas effects env <|> runM m2 ppe pmcSwitch datas effects env
    s@(Success _ _) <|> _ = s
    TypeError _ _ <|> r = r
    CompilerBug _ _ _ <|> r = r -- swallowing bugs for now: when checking whether a type annotation
    -- is redundant, typechecking without that annotation might result in
    -- a CompilerBug that we want `orElse` to recover from

-- getMaybe :: Result v loc a -> Result v loc (Maybe a)
-- getMaybe = hoistMaybe Just

-- hoistMaybe :: (Maybe a -> Maybe b) -> Result v loc a -> Result v loc b
-- hoistMaybe f (Result es is a) = Result es is (f a)

getPrettyPrintEnv :: M v loc PrettyPrintEnv
getPrettyPrintEnv = MT \ppe _ _ _ env -> pure (ppe, env)

getDataDeclarations :: M v loc (DataDeclarations v loc)
getDataDeclarations = MT \_ _ datas _ env -> pure (datas, env)

getEffectDeclarations :: M v loc (EffectDeclarations v loc)
getEffectDeclarations = MT \_ _ _ effects env -> pure (effects, env)

getPatternMatchCoverageCheckAndKindInferenceSwitch :: M v loc PatternMatchCoverageCheckAndKindInferenceSwitch
getPatternMatchCoverageCheckAndKindInferenceSwitch = MT \_ pmcSwitch _ _ env -> pure (pmcSwitch, env)

compilerCrash :: CompilerBug v loc -> M v loc a
compilerCrash bug = liftResult $ compilerBug bug

failWith :: Cause v loc -> M v loc a
failWith cause = liftResult $ typeError cause

compilerCrashResult :: CompilerBug v loc -> Result v loc a
compilerCrashResult bug = CompilerBug bug mempty mempty

getDataDeclaration :: Reference -> M v loc (DataDeclaration v loc)
getDataDeclaration r = do
  ddecls <- getDataDeclarations
  case Map.lookup r ddecls of
    Nothing ->
      getEffectDeclarations >>= \edecls ->
        case Map.lookup r edecls of
          Nothing -> compilerCrash (UnknownDecl Data r ddecls)
          Just decl ->
            liftResult . typeError $
              DataEffectMismatch Effect r (DD.toDataDecl decl)
    Just decl -> pure decl

getEffectDeclaration :: Reference -> M v loc (EffectDeclaration v loc)
getEffectDeclaration r = do
  edecls <- getEffectDeclarations
  case Map.lookup r edecls of
    Nothing ->
      getDataDeclarations >>= \ddecls ->
        case Map.lookup r ddecls of
          Nothing ->
            compilerCrash $
              UnknownDecl Effect r (DD.toDataDecl <$> edecls)
          Just decl ->
            liftResult . typeError $ DataEffectMismatch Data r decl
    Just decl -> pure decl

getDataConstructorType :: (Var v, Ord loc) => ConstructorReference -> M v loc (Type v loc)
getDataConstructorType = getConstructorType' Data getDataDeclaration

getDataConstructors :: forall v loc. (Var v) => Type v loc -> M v loc (EnumeratedConstructors (TypeVar v loc) v loc)
getDataConstructors typ
  | Type.Ref' r <- typ, r == Type.booleanRef = pure BooleanType
  | Type.Request' effects resultType <- typ =
      let phi effect =
            case theRef effect of
              Just r -> Map.fromList . map (\(v, cr, t) -> (cr, (v, t))) . crFromDecl r . DD.toDataDecl <$> getEffectDeclaration r
              Nothing -> pure Map.empty
          crefs = getAp (foldMap (Ap . phi) effects)
       in AbilityType resultType <$> crefs
  | Type.App' (Type.Ref' r) arg <- typ,
    r == Type.listRef =
      let xs =
            [ (ListPat.Cons, [arg]),
              (ListPat.Nil, [])
            ]
       in pure (SequenceType xs)
  | Just r <- theRef typ = ConstructorType . crFromDecl r <$> getDataDeclaration r
  | otherwise = pure OtherType
  where
    crFromDecl :: Reference -> DataDeclaration v loc -> [(v, ConstructorReference, Type v loc)]
    crFromDecl r decl =
      [(v, ConstructorReference r i, ABT.vmap TypeVar.Universal t) | (i, (v, t)) <- zip [0 ..] (DD.constructors decl)]
    theRef t = case t of
      Type.Apps' (Type.Ref' r@Reference.DerivedId {}) _targs -> Just r
      Type.Ref' r@Reference.DerivedId {} -> Just r
      _ -> Nothing

getEffectConstructorType :: (Var v, Ord loc) => ConstructorReference -> M v loc (Type v loc)
getEffectConstructorType = getConstructorType' Effect go
  where
    go r = DD.toDataDecl <$> getEffectDeclaration r

-- Encountered an unknown constructor in the typechecker; unknown constructors
-- should have been detected earlier though.
getConstructorType' ::
  (Var v) =>
  Unknown ->
  (Reference -> M v loc (DataDeclaration v loc)) ->
  ConstructorReference ->
  M v loc (Type v loc)
getConstructorType' kind get (ConstructorReference r cid) = do
  decl <- get r
  case drop (fromIntegral cid) (DD.constructors decl) of
    [] -> compilerCrash $ UnknownConstructor kind (ConstructorReference r cid) decl
    (_v, typ) : _ -> pure $ ABT.vmap TypeVar.Universal typ

extendUniversal :: (Var v) => v -> M v loc v
extendUniversal v = do
  v' <- freshenVar v
  extendContext (Universal v')
  pure v'

extendExistential :: (Var v) => v -> M v loc v
extendExistential v = do
  v' <- freshenVar v
  extendContext (Var (TypeVar.Existential B.Blank v'))
  pure v'

extendExistentialTV :: (Var v) => v -> M v loc (TypeVar v loc)
extendExistentialTV v =
  TypeVar.Existential B.Blank <$> extendExistential v

notMember :: (Var v, Ord loc) => v -> Set (TypeVar v loc) -> Bool
notMember v s =
  Set.notMember (TypeVar.Universal v) s
    && Set.notMember (TypeVar.Existential B.Blank v) s

-- | Replace any existentials with their solution in the context
apply :: (Var v, Ord loc) => Context v loc -> Type v loc -> Type v loc
apply ctx = apply' (solvedExistentials . info $ ctx)

-- | Replace any existentials with their solution in the context (given as a list of elements)
applyCtx :: (Var v, Ord loc) => Map v (Monotype v loc) -> Type v loc -> Type v loc
applyCtx = apply'

apply' :: (Var v, Ord loc) => Map v (Monotype v loc) -> Type v loc -> Type v loc
apply' _ t | Set.null (Type.freeVars t) = t
apply' solvedExistentials t = go t
  where
    go t = case t of
      Type.Var' (TypeVar.Universal _) -> t
      Type.Ref' _ -> t
      Type.Var' (TypeVar.Existential _ v) ->
        maybe t (\(Type.Monotype t') -> go t') (Map.lookup v solvedExistentials)
      Type.Arrow' i o -> Type.arrow a (go i) (go o)
      Type.App' x y -> Type.app a (go x) (go y)
      Type.Ann' v k -> Type.ann a (go v) k
      Type.Effect1' e t -> Type.effect1 a (go e) (go t)
      Type.Effects' es -> Type.effects a (map go es)
      Type.ForallNamed' v t' -> Type.forAll a v (go t')
      Type.IntroOuterNamed' v t' -> Type.introOuter a v (go t')
      _ -> error $ "Match error in Context.apply': " ++ show t
      where
        a = ABT.annotation t

loc :: ABT.Term f v loc -> loc
loc = ABT.annotation

-- | Post-processes an action that wants abilities by filtering out
-- some handled abilities.
withEffects ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  M v loc (Wanted v loc) ->
  M v loc (Wanted v loc)
withEffects handled act = do
  want <- expandWanted =<< act
  handled <- expandAbilities handled
  pruneWanted [] want handled

synthesizeApps ::
  (Foldable f, Var v, Ord loc) =>
  Term v loc ->
  Type v loc ->
  f (Term v loc) ->
  M v loc (Type v loc, Wanted v loc)
synthesizeApps fun ft args =
  foldM go (ft, []) $ Foldable.toList args `zip` [1 ..]
  where
    go (ft, want) arg = do
      ctx <- getContext
      (t, rwant) <- synthesizeApp fun (apply ctx ft) arg
      (t,) <$> coalesceWanted rwant want

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the context in
-- the process.
-- e.g. in `(f:t) x` -- finds the type of (f x) given t and x.
synthesizeApp ::
  (Var v, Ord loc) =>
  Term v loc ->
  Type v loc ->
  (Term v loc, Int) ->
  M v loc (Type v loc, Wanted v loc)
synthesizeApp _ ft arg
  | debugEnabled && traceShow ("synthesizeApp" :: String, ft, arg) False =
      undefined
synthesizeApp fun (Type.stripIntroOuters -> Type.Effect'' es ft) argp@(arg, argNum) =
  scope (InSynthesizeApp ft arg argNum) $ do
    (t, w) <- go ft
    (t,) <$> coalesceWanted ((Just fun,) <$> es) w
  where
    go (Type.Forall' body) = do
      -- Forall1App
      v <- ABT.freshen body freshenTypeVar
      appendContext [existential v]
      let ft2 = ABT.bindInheritAnnotation body (existential' () B.Blank v)
      synthesizeApp fun ft2 argp
    go (Type.Arrow' i o0) = do
      -- ->App
      let (es, o) = Type.stripEffect o0
      (o,) <$> checkWantedScoped ((Just fun,) <$> es) arg i
    go (Type.Var' (TypeVar.Existential b a)) = do
      -- a^App
      [i, e, o] <- traverse freshenVar [Var.named "i", Var.inferAbility, Var.named "o"]
      let it = existential' (loc ft) B.Blank i
          ot = existential' (loc ft) B.Blank o
          et = existential' (loc ft) B.Blank e
          soln =
            Type.Monotype
              ( Type.arrow
                  (loc ft)
                  it
                  (Type.effect (loc ft) [et] ot)
              )
          ctxMid =
            [ existential o,
              existential e,
              existential i,
              Solved b a soln
            ]
      replaceContext (existential a) ctxMid
      synthesizeApp fun (Type.getPolytype soln) argp
    go _ = getContext >>= \ctx -> failWith $ TypeMismatch ctx
synthesizeApp _ _ _ =
  error "unpossible - Type.Effect'' pattern always succeeds"

-- For arity 3, creates the type `∀ a . a -> a -> a -> Sequence a`
-- For arity 2, creates the type `∀ a . a -> a -> Sequence a`
vectorConstructorOfArity :: (Var v, Ord loc) => loc -> Int -> M v loc (Type v loc)
vectorConstructorOfArity loc arity = do
  let elementVar = Var.named "elem"
      args = replicate arity (loc, Type.var loc elementVar)
      resultType = Type.app loc (Type.list loc) (Type.var loc elementVar)
      vt = Type.forAll loc elementVar (Type.arrows args resultType)
  pure vt

generalizeAndUnTypeVar :: (Var v) => Type v a -> Type.Type v a
generalizeAndUnTypeVar t | debugType "generalizeAndUnTypeVar" t = undefined
generalizeAndUnTypeVar t =
  Type.cleanup . ABT.vmap TypeVar.underlying . Type.generalize (Set.toList $ ABT.freeVars t) $ t

generalizeExistentials' ::
  (Var v) => Type v a -> Type v a
generalizeExistentials' t =
  Type.generalize (filter isExistential . Set.toList $ ABT.freeVars t) t
  where
    isExistential TypeVar.Existential {} = True
    isExistential _ = False

noteTopLevelType ::
  (Ord loc, Var v) =>
  ABT.Subst f v a ->
  Term v loc ->
  Type v loc ->
  M v loc ()
noteTopLevelType e binding typ = case binding of
  Term.Ann' strippedBinding _ -> do
    inferred <- (Just <$> synthesizeTop strippedBinding) `orElse` pure Nothing
    case inferred of
      Nothing ->
        btw $
          topLevelComponent
            [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, False)]
      Just inferred -> do
        redundant <- isRedundant typ inferred
        btw $
          topLevelComponent
            [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, redundant)]
  -- The signature didn't exist, so was definitely redundant
  _ ->
    btw $
      topLevelComponent
        [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, True)]

synthesizeTop ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  M v loc (Type v loc)
synthesizeTop tm = do
  (ty, want) <- synthesize tm
  ctx@(Context es solved) <- getContext
  want <- substAndDefaultWanted want (fst <$> es) solved
  when (not $ null want) . failWith $ do
    AbilityCheckFailure
      []
      (Type.flattenEffects . snd =<< want)
      ctx
  applyM ty

-- | Synthesize the type of the given term, updating the context in
-- the process.  Also collect wanted abilities.
-- | Figure 11 from the paper
synthesize ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  M v loc (Type v loc, Wanted v loc)
synthesize e | debugShow ("synthesize" :: String, e) = undefined
synthesize e = scope (InSynthesize e) $
  case minimize' e of
    Left es -> failWith (DuplicateDefinitions es)
    Right e -> do
      (Type.Effect'' es t, want) <- synthesizeWanted e
      want <- coalesceWanted (fmap (Just e,) es) want
      pure (t, want)

-- | Helper function for turning an ability request's type into the
-- results used by type checking.
wantRequest ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  Type v loc ->
  (Type v loc, Wanted v loc)
wantRequest loc ty =
  let ~(es, t) = Type.unEffect0 ty
   in (t, fmap (Just loc,) es)

-- | This is the main worker for type synthesis. It was factored out
-- of the `synthesize` function. It handles the various actual
-- synthesis cases for terms, while the `synthesize` function wraps
-- this in some common pre/postprocessing.
--
-- The return value is the synthesized type together with a list of
-- wanted abilities.
synthesizeWanted ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  M v loc (Type v loc, Wanted v loc)
synthesizeWanted (Term.Var' v) =
  getContext >>= \ctx ->
    case lookupAnn ctx v of -- Var
      Nothing -> compilerCrash $ UndeclaredTermVariable v ctx
      -- variables accesses are pure
      Just t -> do
        -- Note: we ungeneralize the type for ease of discarding. The
        -- current algorithm isn't sensitive to keeping things
        -- quantified, so it should be valid to not worry about
        -- re-generalizing.
        --
        -- Polymorphic ability variables in covariant positions in an
        -- occurrence's type only add useless degrees of freedom to the
        -- solver. They allow an occurrence to 'want' any row, but the
        -- occurrence might as well be chosen to 'want' the empty row,
        -- since that can be satisfied the most easily. The solver
        -- generally has no way of deciding that these arbitrary degrees
        -- of freedom are unnecessary later, and will get confused about
        -- which variable ot instantiate, so we ought to discard them
        -- early.
        (vs, t) <- ungeneralize' t
        pure (discardCovariant (Set.fromList vs) t, [])
synthesizeWanted (Term.Ref' h) =
  compilerCrash $ UnannotatedReference h
synthesizeWanted (Term.Ann' (Term.Ref' _) t)
  -- innermost Ref annotation assumed to be correctly provided by
  -- `synthesizeClosed`
  --
  -- Top level references don't have their own effects.
  | Set.null s = do
      t <- existentializeArrows t
      -- See note about ungeneralizing above in the Var case.
      t <- ungeneralize t
      pure (discard t, [])
  | otherwise = compilerCrash $ FreeVarsInTypeAnnotation s
  where
    s = ABT.freeVars t
    discard ty = discardCovariant fvs ty
      where
        fvs = foldMap p $ ABT.freeVars ty
        p (TypeVar.Existential _ v) = Set.singleton v
        p _ = mempty
synthesizeWanted (Term.Constructor' r) =
  -- Constructors do not have effects
  (,[]) . Type.purifyArrows <$> getDataConstructorType r
synthesizeWanted tm@(Term.Request' r) =
  fmap (wantRequest tm) . ungeneralize . Type.purifyArrows
    =<< getEffectConstructorType r
synthesizeWanted (Term.Let1Top' top binding e) = do
  (tbinding, wb) <- synthesizeBinding top binding
  v' <- ABT.freshen e freshenVar
  when (Var.isAction (ABT.variable e)) $
    -- enforce that actions in a block have type ()
    subtype tbinding (DDB.unitType (ABT.annotation binding))
  appendContext [Ann v' tbinding]
  (t, w) <- synthesize (ABT.bindInheritAnnotation e (Term.var () v'))
  t <- applyM t
  when top $ noteTopLevelType e binding tbinding
  want <- coalesceWanted w wb
  -- doRetract $ Ann v' tbinding
  pure (t, want)
synthesizeWanted (Term.LetRecNamed' [] body) = synthesizeWanted body
synthesizeWanted (Term.LetRecTop' isTop letrec) = do
  ((t, want), ctx2, solved2) <- markThenRetract (Var.named "let-rec-marker") $ do
    e <- annotateLetRecBindings isTop letrec
    synthesize e
  want <- substAndDefaultWanted want ctx2 solved2
  pure (generalizeExistentials ctx2 solved2 t, want)
synthesizeWanted (Term.Handle' h body) = do
  -- To synthesize a handle block, we first synthesize the handler h,
  -- then push its allowed abilities onto the current ambient set when
  -- checking the body. Assuming that works, we also verify that the
  -- handler only uses abilities in the current ambient set.
  (ht, hwant) <- synthesize h
  ht <- ungeneralize =<< applyM ht
  ctx <- getContext
  case ht of
    -- common case, like `h : Request {Remote} a -> b`, brings
    -- `Remote` into ambient when checking `body`
    Type.Arrow' (Type.Apps' (Type.Ref' ref) [et, i]) o | ref == Type.effectRef -> do
      let es = Type.flattenEffects et
      bwant <- withEffects es $ checkWanted [] body i
      o <- applyM o
      let (oes, o') = Type.stripEffect o
      want <- coalesceWanted (fmap (Just h,) oes ++ bwant) hwant
      pure (o', want)
    -- degenerate case, like `handle x -> 10 in ...`
    -- todo: reviewme - I think just generate a type error in this case
    -- Currently assuming no effects are handled.
    Type.Arrow' (i@(Type.Var' (TypeVar.Existential _ v@(lookupSolved ctx -> Nothing)))) o -> do
      r <- extendExistential v
      let rt = existentialp (loc i) r
          e0 =
            Type.apps
              (Type.ref (loc i) Type.effectRef)
              [(loc i, Type.effects (loc i) []), (loc i, rt)]
      subtype i e0
      o <- applyM o
      let (oes, o') = Type.stripEffect o
      want <- checkWanted (fmap (Just h,) oes) body rt
      pure (o', want)
    _ -> failWith $ HandlerOfUnexpectedType (loc h) ht
synthesizeWanted (Term.Ann' e t) = checkScoped e t
synthesizeWanted tm@(Term.Apps' f args) = do
  -- ->EEEEE
  (ft, fwant) <- synthesize f
  ctx <- getContext
  (vs, ft) <- ungeneralize' ft
  (at, awant) <-
    scope (InFunctionCall vs f ft args) $
      synthesizeApps tm (apply ctx ft) args
  (at,) <$> coalesceWanted awant fwant

-- From here down, the term location is used in the result, so it is
-- more convenient to use pattern guards.
synthesizeWanted e
  -- literals
  | Term.Float' _ <- e = pure (Type.float l, []) -- 1I=>
  | Term.Int' _ <- e = pure (Type.int l, []) -- 1I=>
  | Term.Nat' _ <- e = pure (Type.nat l, []) -- 1I=>
  | Term.Boolean' _ <- e = pure (Type.boolean l, [])
  | Term.Text' _ <- e = pure (Type.text l, [])
  | Term.Char' _ <- e = pure (Type.char l, [])
  | Term.TermLink' _ <- e = pure (Type.termLink l, [])
  | Term.TypeLink' _ <- e = pure (Type.typeLink l, [])
  | Term.Blank' blank <- e = do
      let freshType = case blank of
            B.Recorded (B.MissingResultPlaceholder _) -> Var.missingResult
            _ -> Var.blank
      v <- freshenVar freshType
      appendContext [Var (TypeVar.Existential blank v)]
      pure (existential' l blank v, [])
  | Term.List' v <- e = do
      ft <- vectorConstructorOfArity l (Foldable.length v)
      case Foldable.toList v of
        [] -> pure (ft, [])
        v1 : _ ->
          scope (InVectorApp (ABT.annotation v1)) $
            synthesizeApps e ft v

  -- ->I=> (Full Damas Milner rule)
  | Term.Lam' body <- e = do
      -- arya: are there more meaningful locations we could put into and
      -- pull out of the abschain?)
      [arg, i, e, o] <-
        sequence
          [ ABT.freshen body freshenVar,
            freshenVar (ABT.variable body),
            freshenVar Var.inferAbility,
            freshenVar Var.inferOutput
          ]
      let it = existential' l B.Blank i
          ot = existential' l B.Blank o
          et = existential' l B.Blank e
      appendContext $
        [existential i, existential e, existential o, Ann arg it]
      when (Var.typeOf i == Var.Delay) $ do
        -- '(1 + 1) turns into a lambda with an arg variable of type Var.Delay
        -- here's where the typechecker assumes this must be of type 'thunkArgType'
        subtype it (DDB.thunkArgType l)
      body' <- pure $ ABT.bindInheritAnnotation body (Term.var () arg)
      if Term.isLam body'
        then checkWithAbilities [] body' ot
        else checkWithAbilities [et] body' ot
      ctx <- getContext
      let t = apply ctx $ Type.arrow l it (Type.effect l [et] ot)
      pure (t, [])
  | Term.If' cond t f <- e = do
      cwant <- scope InIfCond $ check cond (Type.boolean l)
      (ty, bwant) <-
        scope (InIfBody $ ABT.annotation t) $
          synthesizeApps e (Type.iff2 l) [t, f]
      (ty,) <$> coalesceWanted bwant cwant
  | Term.And' a b <- e =
      scope InAndApp $ synthesizeApps e (Type.andor' l) [a, b]
  | Term.Or' a b <- e =
      scope InOrApp $ synthesizeApps e (Type.andor' l) [a, b]
  | Term.Match' scrutinee cases <- e = do
      (scrutineeType, swant) <- synthesize scrutinee
      outputTypev <- freshenVar (Var.named "match-output")
      let outputType = existential' l B.Blank outputTypev
      appendContext [existential outputTypev]
      cwant <- checkCases scrutineeType outputType cases
      want <- coalesceWanted cwant swant
      ctx <- getContext
      let matchType = apply ctx outputType
      getPatternMatchCoverageCheckAndKindInferenceSwitch >>= \case
        PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled -> ensurePatternCoverage e matchType scrutinee scrutineeType cases
        PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled -> pure ()
      pure $ (matchType, want)
  where
    l = loc e
synthesizeWanted _e = compilerCrash PatternMatchFailure

-- | Synthesizes a type for a local binding, for use in synthesizing
-- or checking `Let1` expressions. There is a bit of wrapping around
-- the call to `synthesize` to attempt to generalize certain
-- definitions.
--
-- We want to generalize self-contained definitions when possible, so
-- that things like:
--
--   id x = x
--   id ()
--   id "hello"
--
-- will work. However, note that just checking that the definition is
-- self contained is insufficient, because:
--
--   r = IO.ref '(bug "whatever")
--
-- is self-contained (in the free variable sense), but would yield a
-- polymorphic reference. So, I think it is also necessary to check
-- that the binding has no wanted abilities. This automatically covers
-- the local function definitions we want.
--
-- ---
--
-- The current strategy for generalization is a bit sophisticated as
-- well. We want to generalize local definitions when possible.
-- However, when doing type directed name resolution, we _don't_ want
-- to generalize over variables that will help us figure out which
-- selection to make.
--
-- So, when we _do_ generalize, we first partition the discarded
-- context into the portion that is involved in TDNR solutions, and
-- the portion that isn't. We generalize the variables that aren't
-- involved in TDNR, and re-push the variables that are, so that they
-- can be refined later. This is a bit unusual for the algorithm we
-- use, but it seems like it should be safe.
synthesizeBinding ::
  (Var v) =>
  (Ord loc) =>
  Bool ->
  Term v loc ->
  M v loc (Type v loc, Wanted v loc)
synthesizeBinding top binding = do
  markThenCallWithRetract Var.inferOther \retract -> adjustNotes do
    (tb, wb) <- synthesize binding
    if not (null wb)
      then fmap (\t -> ((t, wb), id)) (applyM tb)
      else
        if top
          then do
            (ctx, solved) <- retract
            pure ((generalizeExistentials ctx solved tb, []), substituteSolved solved)
          else do
            (ctx, solved) <- retract
            -- Note: this is conservative about what we avoid
            -- generalizing. Right now only TDNR causes variables to be
            -- retained. It might be possible to make this happen for any
            -- `Recorded` to do more inference for unknown variable errors
            -- (or whatever the other cases are for), at the expense of
            -- less generalization in the process of reporting those.
            let retain (B.Recorded B.Resolve {}) = True
                retain B.Retain = True
                retain _ = False

                erecs = [v | Existential b v <- ctx, retain b]
                srecs =
                  [ v
                    | Solved b _ sa <- ctx,
                      retain b,
                      TypeVar.Existential _ v <-
                        Set.toList . ABT.freeVars . applyCtx solved $ Type.getPolytype sa
                  ]
                keep = Set.fromList (erecs ++ srecs)
                p (Existential _ v)
                  | v `Set.member` keep =
                      Left . Var $ TypeVar.Existential B.Retain v
                p e = Right e
                (repush, discard) = partitionEithers $ fmap p ctx
            appendContext repush
            markRetained keep
            let tf = generalizeExistentials discard (computeSolvedExistentials discard) (applyCtx solved tb)
            pure ((tf, []), substituteSolved solved)

getDataConstructorsAtType :: forall v loc. (Ord loc, Var v) => Type v loc -> M v loc (EnumeratedConstructors (TypeVar v loc) v loc)
getDataConstructorsAtType t0 = do
  dataConstructors <- getDataConstructors t0
  case t0 of
    Type.Request' ets _res ->
      let effectMap :: Map Reference (Type v loc)
          effectMap =
            Map.fromList
              . mapMaybe
                ( \e -> case e of
                    Type.Apps' (Type.Ref' r@Reference.DerivedId {}) _targs -> Just (r, e)
                    Type.Ref' r@Reference.DerivedId {} -> Just (r, e)
                    _ -> Nothing
                )
              $ ets
       in flip traverseConstructorTypes dataConstructors \_ cr t -> do
            case Map.lookup (view reference_ cr) effectMap of
              Nothing -> pure t
              Just t0 -> do
                t <- ungeneralize t
                case t of
                  Type.EffectfulArrows' _ xs
                    | (Just [e], _) <- last xs -> do
                        equate t0 e
                        applyM t
                  _ -> pure t
    _ -> traverseConstructorTypes (\_ _ t -> fixType t) dataConstructors
  where
    fixType t = do
      t <- ungeneralize t
      let lastT = case t of
            Type.Arrows' xs -> last xs
            _ -> t
      equate t0 lastT
      applyM t

data PmcState vt v loc = PmcState
  { variables :: !(Set v),
    constructorCache :: !(Map (Type v loc) (EnumeratedConstructors vt v loc))
  }

instance (Ord loc, Var v) => Pmc (TypeVar v loc) v loc (StateT (PmcState (TypeVar v loc) v loc) (M v loc)) where
  getPrettyPrintEnv = lift getPrettyPrintEnv
  getConstructors typ = do
    st@PmcState {constructorCache} <- get
    let f = \case
          Nothing -> Compose $ (\t -> (t, Just t)) <$> lift (getDataConstructorsAtType typ)
          Just t -> Compose $ pure (t, Just t)
    (result, newCache) <- getCompose (Map.alterF f typ constructorCache)
    put st {constructorCache = newCache}
    pure result
  getConstructorVarTypes t cref@(ConstructorReference _r cid) = do
    Pmc.getConstructors t >>= \case
      AbilityType _ m -> case Map.lookup cref m of
        Nothing -> error $ show cref <> " not found in constructor map: " <> show m
        Just (_, conArgs) -> pure (extractArgs conArgs)
      ConstructorType cs -> case drop (fromIntegral cid) cs of
        [] -> error $ show cref <> " not found in constructor list: " <> show cs
        (_, _, conArgs) : _ -> pure (extractArgs conArgs)
      BooleanType -> pure []
      OtherType -> pure []
      SequenceType {} -> pure []
    where
      extractArgs (Type.Arrows' xs) = init xs
      extractArgs _ = []
  fresh = do
    st@PmcState {variables} <- get
    let v = Var.freshIn variables (Var.typed Var.Pattern)
    put (st {variables = Set.insert v variables})
    pure v

ensurePatternCoverage ::
  forall v loc.
  (Ord loc, Var v) =>
  Term v loc ->
  Type v loc ->
  Term v loc ->
  Type v loc ->
  [Term.MatchCase loc (Term v loc)] ->
  MT v loc (Result v loc) ()
ensurePatternCoverage theMatch _theMatchType _scrutinee scrutineeType cases = do
  let matchLoc = ABT.annotation theMatch
  scrutineeType <- applyM scrutineeType
  let pmcState :: PmcState (TypeVar v loc) v loc =
        PmcState
          { variables = ABT.freeVars theMatch,
            constructorCache = mempty
          }
  (redundant, _inaccessible, uncovered) <- flip evalStateT pmcState do
    checkMatch scrutineeType cases
  let checkUncovered = maybe (pure ()) (failWith . UncoveredPatterns matchLoc) $ Nel.nonEmpty uncovered
      checkRedundant = foldr ((*>) . failWith . RedundantPattern) (pure ()) redundant
  checkUncovered *> checkRedundant

checkCases ::
  (Var v) =>
  (Ord loc) =>
  Type v loc ->
  Type v loc ->
  [Term.MatchCase loc (Term v loc)] ->
  M v loc (Wanted v loc)
checkCases _ _ [] = pure []
checkCases scrutType outType cases@(Term.MatchCase _ _ t : _) =
  scope (InMatch (ABT.annotation t)) $ do
    mes <- requestType (cases <&> \(Term.MatchCase p _ _) -> p)
    for_ mes $ \es ->
      applyM scrutType >>= \sty -> ensureReqEffects sty es
    scrutType' <- applyM =<< ungeneralize scrutType
    coalesceWanteds =<< traverse (checkCase scrutType' outType) cases

-- Checks a scrutinee type against a list of effects from e.g. a list of cases
-- from a handler.
--
-- This opportunistically destructures the scrutinee type if it is of the form
-- `Request es r` in an effort to avoid introducing ability unification
-- variables that will be more difficult to infer. In such cases, we just run an
-- ability check directly against `es`. This works better, for instance, where a
-- signature has been given for the handler, and es = {A,B,g}, for a universal
-- g. In such a situation, we have no good way of solving via the general check
-- for {A,B,e} < {A,B,g} with a fresh `e` existential, but the `e` is actually
-- useless in this scenario.
ensureReqEffects :: (Var v) => (Ord loc) => Type v loc -> [Type v loc] -> M v loc ()
ensureReqEffects (Type.Apps' (Type.Ref' req) [hes, _]) res
  | req == Type.effectRef = expandAbilities [hes] >>= \hes -> abilityCheck' hes res
ensureReqEffects sty res = do
  v <- freshenVar Var.inferPatternPureV
  g <- freshenVar Var.inferAbility
  let lo = loc sty
      vt = existentialp lo v
      gt = existentialp lo g
      es' = gt : res
  appendContext [existential g, existential v]
  subtype (Type.effectV lo (lo, Type.effects lo es') (lo, vt)) sty

getEffect ::
  (Var v) => (Ord loc) => ConstructorReference -> M v loc (Type v loc)
getEffect ref = do
  ect <- getEffectConstructorType ref
  uect <- ungeneralize ect
  let final (Type.Arrow' _ o) = final o
      final t = t
  case final uect of
    Type.Effect'' [et] _ -> pure et
    t@(Type.Effect'' _ _) ->
      compilerCrash $ EffectConstructorHadMultipleEffects t
    _ -> compilerCrash PatternMatchFailure

requestType ::
  (Var v) => (Ord loc) => [Pattern loc] -> M v loc (Maybe [Type v loc])
requestType ps =
  traverse (traverse getEffect . nubBy ((==) `on` view reference_)) $
    Foldable.foldlM (\acc p -> (++ acc) <$> single p) [] ps
  where
    single (Pattern.As _ p) = single p
    single Pattern.EffectPure {} = Just []
    single (Pattern.EffectBind _ ref _ _) = Just [ref]
    single _ = Nothing

checkCase ::
  forall v loc.
  (Var v, Ord loc) =>
  Type v loc ->
  Type v loc ->
  Term.MatchCase loc (Term v loc) ->
  M v loc (Wanted v loc)
checkCase scrutineeType outputType (Term.MatchCase pat guard rhs) = do
  scrutineeType <- applyM scrutineeType
  outputType <- applyM outputType
  markThenRetractWanted Var.inferOther $ do
    let peel t = case t of
          ABT.AbsN' vars bod -> (vars, bod)
        (rhsvs, rhsbod) = peel rhs
        mayGuard = snd . peel <$> guard
    (substs, remains) <- runStateT (checkPattern scrutineeType pat) rhsvs
    unless (null remains) $ compilerCrash (MalformedPattern pat)
    let subst = ABT.substsInheritAnnotation (second (Term.var ()) <$> substs)
        rhs' = subst rhsbod
        guard' = subst <$> mayGuard
    gwant <- for guard' $ \g ->
      scope InMatchGuard $
        checkWantedScoped [] g (Type.boolean (loc g))
    outputType <- applyM outputType
    scope InMatchBody $
      checkWantedScoped (fromMaybe [] gwant) rhs' outputType

-- For example:
--   match scrute with
--     (x, [42,y,Foo z]) -> blah x y z
--
-- scrutineeType will just be the type of `scrute`
-- The starting state will be the variables [x,y,z] (extracted from the Abs-chain on the RHS of the ->)
-- The output (assuming no type errors) is [(x,x'), (y,y'), (z,z')]
-- where x', y', z' are freshened versions of x, y, z. These will be substituted
-- into `blah x y z` to produce `blah x' y' z'` before typechecking it.
checkPattern ::
  (Var v, Ord loc) =>
  Type v loc ->
  Pattern loc ->
  StateT [v] (M v loc) [(v, v)]
checkPattern tx ty | (debugEnabled || debugPatternsEnabled) && traceShow ("checkPattern" :: String, tx, ty) False = undefined
checkPattern scrutineeType p =
  case p of
    Pattern.Unbound _ -> pure []
    Pattern.Var _loc -> do
      v <- getAdvance p
      v' <- lift $ freshenVar v
      lift . appendContext $ [Ann v' scrutineeType]
      pure [(v, v')]
    -- Ex: [42, y, Foo z]
    Pattern.SequenceLiteral loc ps -> do
      vt <- lift $ do
        v <- freshenVar Var.inferOther
        let vt = existentialp loc v
        appendContext [existential v]
        -- ['a] <: scrutineeType, where 'a is fresh existential
        subtype (Type.app loc (Type.list loc) vt) scrutineeType
        applyM vt
      join <$> traverse (checkPattern vt) ps
    Pattern.SequenceOp loc l op r -> do
      let (locL, locR) = (Pattern.loc l, Pattern.loc r)
      vt <- lift $ do
        v <- freshenVar Var.inferOther
        let vt = existentialp loc v
        appendContext [existential v]
        -- todo: `Type.list loc` is super-probably wrong;
        -- I'm thinking it should be Ann.Intrinsic, but we don't
        -- have access to that here.
        subtype (Type.app loc (Type.list loc) vt) scrutineeType
        applyM vt
      case op of
        Pattern.Cons -> do
          lvs <- checkPattern vt l
          -- todo: same `Type.list loc` thing
          rvs <- checkPattern (Type.app locR (Type.list locR) vt) r
          pure $ lvs ++ rvs
        Pattern.Snoc -> do
          -- todo: same `Type.list loc` thing
          lvs <- checkPattern (Type.app locL (Type.list locL) vt) l
          rvs <- checkPattern vt r
          pure $ lvs ++ rvs
        Pattern.Concat ->
          case (l, r) of
            (p, _) | isConstLen p -> f
            (_, p) | isConstLen p -> f
            (_, _) ->
              lift . failWith $
                ConcatPatternWithoutConstantLength loc (Type.app loc (Type.list loc) vt)
          where
            f = liftA2 (++) (g locL l) (g locR r)
            -- todo: same `Type.list loc` thing
            g l p = checkPattern (Type.app l (Type.list l) vt) p

            -- Only pertains to sequences, returns False if not a sequence
            isConstLen :: Pattern loc -> Bool
            isConstLen p = case p of
              Pattern.SequenceLiteral _ _ -> True
              Pattern.SequenceOp _ l op r -> case op of
                Pattern.Snoc -> isConstLen l
                Pattern.Cons -> isConstLen r
                Pattern.Concat -> isConstLen l && isConstLen r
              Pattern.As _ p -> isConstLen p
              _ -> False
    -- TODO: provide a scope here for giving a good error message
    Pattern.Boolean loc _ ->
      lift $ subtype (Type.boolean loc) scrutineeType $> mempty
    Pattern.Int loc _ ->
      lift $ subtype (Type.int loc) scrutineeType $> mempty
    Pattern.Nat loc _ ->
      lift $ subtype (Type.nat loc) scrutineeType $> mempty
    Pattern.Float loc _ ->
      lift $ subtype (Type.float loc) scrutineeType $> mempty
    Pattern.Text loc _ ->
      lift $ subtype (Type.text loc) scrutineeType $> mempty
    Pattern.Char loc _ ->
      lift $ subtype (Type.char loc) scrutineeType $> mempty
    Pattern.Constructor loc ref args -> do
      dct <- lift $ getDataConstructorType ref
      udct <- lift $ skolemize forcedData dct
      unless (Type.arity udct == length args)
        . lift
        . failWith
        $ PatternArityMismatch loc dct (length args)
      let step (Type.Arrow' i o, vso) pat =
            (\vso' -> (o, vso ++ vso')) <$> checkPattern i pat
          step _ _ =
            lift . failWith $ PatternArityMismatch loc dct (length args)
      (overall, vs) <- foldM step (udct, []) args
      st <- lift $ applyM scrutineeType
      lift $ subtype overall st
      pure vs
    Pattern.As _loc p' -> do
      v <- getAdvance p
      v' <- lift $ freshenVar v
      lift . appendContext $ [Ann v' scrutineeType]
      ((v, v') :) <$> checkPattern scrutineeType p'
    -- ex: { a } -> a
    -- ex: { (x, 42) } -> a
    Pattern.EffectPure loc p
      -- Avoid creating extra effect variables when the scrutinee is already
      -- known to be a Request.
      --
      -- TODO: this should actually _always_ be the case, because we do a pass
      -- across the entire case statement refining the scrutinee type. The
      -- 'otherwise' still needs to be covered for exhaustivity, however.
      | Type.Apps' (Type.Ref' req) [_, r] <- scrutineeType,
        req == Type.effectRef ->
          checkPattern r p
      | otherwise -> do
          vt <- lift $ do
            v <- freshenVar Var.inferPatternPureV
            e <- freshenVar Var.inferPatternPureE
            let vt = existentialp loc v
            let et = existentialp loc e
            appendContext [existential v, existential e]
            subtype (Type.effectV loc (loc, et) (loc, vt)) scrutineeType
            applyM vt
          checkPattern vt p
    -- ex: { Stream.emit x -> k } -> ...
    Pattern.EffectBind loc ref args k -> do
      -- scrutineeType should be a supertype of `Effect e vt`
      -- for fresh existentials `e` and `vt`
      ect <- lift $ getEffectConstructorType ref
      uect <- lift $ skolemize forcedEffect ect
      unless (Type.arity uect == length args)
        . lift
        . failWith
        . PatternArityMismatch loc ect
        $ length args
      let step (Type.Arrow' i o, vso) pat =
            (\vso' -> (o, vso ++ vso')) <$> checkPattern i pat
          step _ _ =
            lift . failWith $ PatternArityMismatch loc ect (length args)
      (ctorOutputType, vs) <- foldM step (uect, []) args
      st <- lift $ applyM scrutineeType
      case ctorOutputType of
        -- an effect ctor should have exactly 1 effect!
        Type.Effect'' [et] it
          -- expecting scrutineeType to be `Effect et vt`
          | Type.Apps' _ [eff, vt] <- st -> do
              -- ensure that the variables in `et` unify with those from
              -- the scrutinee.
              lift $ abilityCheck' [eff] [et]
              let kt =
                    Type.arrow
                      (Pattern.loc k)
                      it
                      (Type.effect (Pattern.loc k) [eff] vt)
              (vs ++) <$> checkPattern kt k
          | otherwise -> lift . compilerCrash $ PatternMatchFailure
        _ ->
          lift . compilerCrash $
            EffectConstructorHadMultipleEffects
              ctorOutputType
  where
    getAdvance :: Pattern loc -> StateT [v] (M v loc) v
    getAdvance p = do
      vs <- get
      case vs of
        [] -> lift $ compilerCrash (MalformedPattern p)
        (v : vs) -> do
          put vs
          pure v

applyM :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
applyM t = (`apply` t) <$> getContext

lookupAnn :: (Ord v) => Context v loc -> v -> Maybe (Type v loc)
lookupAnn ctx v = Map.lookup v (termVarAnnotations . info $ ctx)

lookupSolved :: (Ord v) => Context v loc -> v -> Maybe (Monotype v loc)
lookupSolved ctx v = Map.lookup v (solvedExistentials . info $ ctx)

resetContextAfter :: a -> M v loc a -> M v loc a
resetContextAfter x a = do
  ctx <- getContext
  a <- a `orElse` pure x
  setContext ctx
  pure a

-- | Synthesize and generalize the type of each binding in a let rec.
-- Updates the context so that all bindings are annotated with
-- their type. Also returns the freshened version of `body`.
-- See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings ::
  (Var v, Ord loc) =>
  Term.IsTop ->
  ((v -> M v loc v) -> M v loc ([(v, Term v loc)], Term v loc)) ->
  M v loc (Term v loc)
annotateLetRecBindings isTop letrec =
  -- If this is a top-level letrec, then emit a TopLevelComponent note,
  -- which asks if the user-provided type annotations were needed.
  if isTop
    then do
      -- First, typecheck (using annotateLetRecBindings') the bindings with any
      -- user-provided annotations.
      (body, vts) <- annotateLetRecBindings' True
      -- Then, try typechecking again, but ignoring any user-provided annotations.
      -- This will infer whatever type.  If it altogether fails to typecheck here
      -- then, ...(1)
      withoutAnnotations <-
        resetContextAfter Nothing $ Just <$> annotateLetRecBindings' False
      -- convert from typechecker TypeVar back to regular `v` vars
      let unTypeVar (v, t) = (v, generalizeAndUnTypeVar t)
      case withoutAnnotations of
        Just (_, vts') -> do
          r <- and <$> zipWithM isRedundant (fmap snd vts) (fmap snd vts')
          btw $ topLevelComponent ((\(v, b) -> (Var.reset v, b, r)) . unTypeVar <$> vts)
        -- ...(1) we'll assume all the user-provided annotations were needed
        Nothing ->
          btw $
            topLevelComponent ((\(v, b) -> (Var.reset v, b, False)) . unTypeVar <$> vts)
      pure body
    else -- If this isn't a top-level letrec, then we don't have to do anything special
      fst <$> annotateLetRecBindings' True
  where
    annotateLetRecBindings' useUserAnnotations = do
      (bindings, body) <- letrec freshenVar
      let vs = map fst bindings
      ((bindings, bindingTypes), ctx2, solved2) <- markThenRetract Var.inferOther $ do
        let f (v, binding) = case binding of
              -- If user has provided an annotation, we use that
              Term.Ann' e t | useUserAnnotations -> do
                -- Arrows in `t` with no ability lists get an attached fresh
                -- existential to allow inference of required abilities
                t2 <- existentializeArrows =<< applyM t
                pure (Term.ann (loc binding) e t2, t2)
              -- If we're not using an annotation, we make one up. There's 2 cases:

              lam@(Term.Lam' _) ->
                -- If `e` is a lambda of arity K, we immediately refine the
                -- existential to `a1 ->{e1} a2 ... ->{eK} r`. This gives better
                -- inference of the lambda's ability variables in conjunction with
                -- handling of lambdas in `check` judgement.
                (lam,) <$> existentialFunctionTypeFor lam
              e -> do
                -- Anything else, just make up a fresh existential
                -- which will be refined during typechecking of the binding
                vt <- extendExistential v
                pure $ (e, existential' (loc binding) B.Blank vt)
        (bindings, bindingTypes) <- unzip <$> traverse f bindings
        appendContext (zipWith Ann vs bindingTypes)
        -- check each `bi` against its type
        Foldable.for_ (zip3 vs bindings bindingTypes) $ \(v, b, t) -> do
          -- note: elements of a cycle have to be pure, otherwise order of effects
          -- is unclear and chaos ensues
          -- ensure actions in blocks have type ()
          when (Var.isAction v) $ subtype t (DDB.unitType (ABT.annotation b))
          checkScopedWith b t []
        ensureGuardedCycle (vs `zip` bindings)
        pure (bindings, bindingTypes)
      -- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
      -- add annotations `v1 : gt1, v2 : gt2 ...` to the context
      let bindingArities = Term.arity <$> bindings
          gen bindingType _arity = generalizeExistentials ctx2 solved2 bindingType
          bindingTypesGeneralized = zipWith gen bindingTypes bindingArities
          annotations = zipWith Ann vs bindingTypesGeneralized
      appendContext annotations
      pure (body, vs `zip` bindingTypesGeneralized)

ensureGuardedCycle :: (Var v) => [(v, Term v loc)] -> M v loc ()
ensureGuardedCycle bindings =
  let -- We make sure that nonLambdas can depend only on lambdas, not on each other
      nonLambdas = Set.fromList [v | (v, b) <- bindings, Term.arity b == 0]
      (notok, ok) = partition f bindings
      f (v, b) =
        if Set.member v nonLambdas
          then not $ Set.null (ABT.freeVars b `Set.intersection` nonLambdas)
          else False
   in if length ok == length bindings
        then pure ()
        else failWith $ UnguardedLetRecCycle (fst <$> notok) bindings

existentialFunctionTypeFor :: (Ord loc, Var v) => Term v loc -> M v loc (Type v loc)
existentialFunctionTypeFor lam@(Term.LamNamed' v body) = do
  v <- extendExistential v
  e <- extendExistential Var.inferAbility
  o <- existentialFunctionTypeFor body
  pure $
    Type.arrow
      (loc lam)
      (existentialp (loc lam) v)
      (Type.effect (loc lam) [existentialp (loc lam) e] o)
existentialFunctionTypeFor e = do
  v <- extendExistential Var.inferOutput
  pure $ existentialp (loc e) v

existentializeArrows :: (Var v) => Type v loc -> M v loc (Type v loc)
existentializeArrows t = do
  let newVar = extendExistentialTV Var.inferAbility
  t <- Type.existentializeArrows newVar t
  pure t

ungeneralize :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
ungeneralize t = snd <$> ungeneralize' t

ungeneralize' :: (Var v, Ord loc) => Type v loc -> M v loc ([v], Type v loc)
ungeneralize' (Type.ForallNamed' v t) = do
  (vs, t) <- tweakEffects v t
  first (vs ++) <$> ungeneralize' t
ungeneralize' t = pure ([], t)

-- Tries to massage types like:
--
--     (a ->{e} b ->{e} c) ->{e} d
--
-- by rewriting them into:
--
--     (a ->{e1} b ->{e2} c) ->{e1,e2} d
--
-- The strategy is to find all negative occurrences of `e` and
-- introduce a new variable for each, and then replace any
-- non-negative occurrences with the row of all negative
-- variables. The reason this is valid is that `e` can be
-- instantiated with the entire row, and then all the negative
-- rows can be pared down to the single variable via subtyping.
--
-- This is meant to occur when a polymorphic type is
-- de-generalized, and replaces simple freshening of the
-- polymorphic variable.
tweakEffects ::
  (Var v) =>
  (Ord loc) =>
  TypeVar v loc ->
  Type v loc ->
  M v loc ([v], Type v loc)
tweakEffects v0 t0
  | isEffectVar v0 t0 && isVariant v0 t0 =
      rewrite (Just False) t0 >>= \case
        ([], ty) ->
          freshenTypeVar v0 >>= \out -> finish [out] ty
        (vs, ty) -> finish vs ty
  | otherwise =
      freshenTypeVar v0 >>= \out -> finish [out] t0
  where
    negative = fromMaybe False

    typ [v] = existential' () B.Blank v
    typ vs = Type.effects () $ existential' () B.Blank <$> vs

    finish vs ty = do
      appendContext (existential <$> vs)
      pure (vs, ABT.substInheritAnnotation v0 (typ vs) ty)

    rewrite p ty
      | Type.ForallNamed' v t <- ty,
        v0 /= v =
          second (Type.forAll a v) <$> rewrite p t
      | Type.Arrow' i o <- ty = do
          (vis, i) <- rewrite (not <$> p) i
          (vos, o) <- rewrite p o
          pure (vis ++ vos, Type.arrow a i o)
      | Type.Effect1' e t <- ty = do
          (ves, e) <- rewrite p e
          (vts, t) <- rewrite p t
          pure (ves ++ vts, Type.effect1 a e t)
      | Type.Effects' es <- ty = do
          ess <- traverse (rewrite p) es
          let es = snd <$> ess; ves = fst =<< ess
          pure (ves, Type.effects a es)
      | Type.Var' v <- ty,
        v0 == v && negative p = do
          u <- freshenTypeVar v0
          pure ([u], existential' (loc ty) B.Blank u)
      | Type.App' f x <- ty = do
          (vfs, f) <- rewrite p f
          (vxs, x) <- rewrite Nothing x
          pure (vfs ++ vxs, Type.app (loc ty) f x)
      | otherwise = pure ([], ty)
      where
        a = loc ty

isEffectVar :: (Var v) => TypeVar v loc -> Type v loc -> Bool
isEffectVar u (Type.ForallNamed' v t)
  | u == v = False
  | otherwise = isEffectVar u t
isEffectVar u (Type.Arrow'' i es o) =
  any p es || isEffectVar u i || isEffectVar u o
  where
    p (Type.Var' v) = v == u
    p _ = False
isEffectVar _ _ = False

-- Checks that a variable only occurs in variant positions. This may mean that
-- it occurs in both covariant and contravariant positions, so long as it
-- doesn't occur in a single position that is invariant, like the `x` in `F x`.
isVariant :: (Var v) => TypeVar v loc -> Type v loc -> Bool
isVariant u = walk True
  where
    walk var (Type.ForallNamed' v t)
      | u == v = True
      | otherwise = walk var t
    walk var (Type.Arrow'' i es o) =
      walk var i && walk var o && all (walk var) es
    walk var (Type.App' f x) = walk var f && walk False x
    walk var (Type.Var' v) = u /= v || var
    walk _ _ = True

skolemize ::
  (Var v) =>
  (Ord loc) =>
  (Type v loc -> Set (TypeVar v loc)) ->
  Type v loc ->
  M v loc (Type v loc)
skolemize forced (Type.ForallsNamed' vs ty) = do
  urn <- for uvs $ \u -> (,) u <$> freshenTypeVar u
  srn <- for svs $ \u -> (,) u <$> freshenTypeVar u
  let uctx = existential . snd <$> urn
      sctx = Universal . snd <$> srn
      rn =
        (fmap (existential' () B.Blank) <$> urn)
          ++ (fmap (universal' ()) <$> srn)
  appendContext $ uctx ++ sctx
  pure $ foldl (flip $ uncurry ABT.substInheritAnnotation) ty rn
  where
    fovs = forced ty
    (uvs, svs) = partition (`Set.member` fovs) vs
skolemize _ ty = pure ty

forcedEffect :: Type v loc -> Set (TypeVar v loc)
forcedEffect (Type.Arrow' _ o) = forcedEffect o
forcedEffect (Type.Effect1' es _) = Type.freeVars es
forcedEffect _ = Set.empty

forcedData :: Type v loc -> Set (TypeVar v loc)
forcedData (Type.Arrow' _ o) = forcedData o
forcedData ty = Type.freeVars ty

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: (Var v, Ord loc) => [Element v loc] -> Map v (Monotype v loc) -> Type v loc -> Type v loc
generalizeExistentials ctx solved ty0 = generalizeP pred ctx solved ty
  where
    gens = Set.fromList $ mapMaybe (fmap snd . existentialP) ctx

    ty = discardCovariant gens $ applyCtx solved ty0
    fvs = Type.freeVars ty

    pred e
      | pe@(Just (tv, _)) <- existentialP e,
        tv `Set.member` fvs =
          pe
      | otherwise = Nothing

generalizeP ::
  (Var v) =>
  (Ord loc) =>
  (Element v loc -> Maybe (TypeVar v loc, v)) ->
  [Element v loc] ->
    Map v (Monotype v loc) ->
  Type v loc ->
  Type v loc
generalizeP p ctx0 solved ty = foldr gen (applyCtx solved ty) ctx
  where
    ctx = mapMaybe p ctx0

    gen (tv, v) t
      | tv `ABT.isFreeIn` t =
          -- location of the forall is just the location of the input type
          -- and the location of each quantified variable is just inherited
          -- from its source location
          Type.forAll
            (loc t)
            (TypeVar.Universal v)
            (ABT.substInheritAnnotation tv (universal' () v) t)
      -- don't bother introducing a forall if type variable is unused
      | otherwise = t

existentialP :: Element v loc -> Maybe (TypeVar v loc, v)
existentialP (Var (TypeVar.Existential _ v)) =
  Just (TypeVar.Existential B.Blank v, v)
existentialP _ = Nothing

variableP :: Element v loc -> Maybe (TypeVar v loc, v)
variableP (Var (TypeVar.Existential _ v)) =
  Just (TypeVar.Existential B.Blank v, v)
variableP (Var tv@(TypeVar.Universal v)) = Just (tv, v)
variableP _ = Nothing

-- This checks `e` against the type `t`, but if `t` is a `∀`, any ∀-quantified
-- variables are freshened and substituted into `e`. This should be called whenever
-- a term is being checked against a type due to a user-provided signature on `e`.
-- See its usage in `synthesize` and `annotateLetRecBindings`.
checkScoped ::
  forall v loc.
  (Var v, Ord loc) =>
  Term v loc ->
  Type v loc ->
  M v loc (Type v loc, Wanted v loc)
checkScoped e (Type.Forall' body) = do
  v <- ABT.freshen body freshenTypeVar
  ((ty, want), pop, popSolved) <- markThenRetract v $ do
    x <- extendUniversal v
    let e' = Term.substTypeVar (ABT.variable body) (universal' () x) e
    checkScoped e' (ABT.bindInheritAnnotation body (universal' () x))
  want <- substAndDefaultWanted want pop popSolved
  pure (generalizeP variableP pop popSolved ty, want)
checkScoped e t = do
  t <- existentializeArrows t
  (t,) <$> check e t

checkScopedWith ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  Type v loc ->
  [Type v loc] ->
  M v loc ()
checkScopedWith tm ty ab = do
  (_, want) <- checkScoped tm ty
  subAbilities want ab

markThenRetractWanted ::
  (Var v) =>
  (Ord loc) =>
  v ->
  M v loc (Wanted v loc) ->
  M v loc (Wanted v loc)
markThenRetractWanted v m = do
  (a, es, solved) <- markThenRetract v m
  substAndDefaultWanted a es solved

-- This function handles merging two sets of wanted abilities, along
-- with some pruning of the set. This means that coalescing a list
-- with the empty list may result in a distinct list, but coalescing
-- again afterwards should not further change things.
--
-- With respect to the above, it is presumed that the second argument
-- (`old`) has already been coalesced in this manner. So only the
-- contents of `new` may be reduced, and coalescing with the empty
-- list as `new` will just yield the `old` list.
--
-- There are two main operations performed while merging. First, an
-- ability (currently) may only occur once in a row, so if it occurs
-- twice in a list, those two occurrences are unified. Second, some
-- references to ability polymorphic functions can lead to arbitrary
-- variables being 'wanted'. However, if these variables do not occur
-- in the context, and we are thus not trying to infer them, it is
-- pointless to add them to the wanted abilities, just making types
-- more complicated, and inference harder. So in that scenario, we
-- default the variable to {} and omit it.
coalesceWanted' ::
  (Var v) =>
  (Ord loc) =>
  (TypeVar v loc -> Bool) ->
  Wanted v loc ->
  Wanted v loc ->
  M v loc (Wanted v loc)
coalesceWanted' _ [] old = pure old
coalesceWanted' keep ((loc, n) : new) old
  | Just (_, o) <- find (headMatch n . snd) old = do
      equate n o
      coalesceWanted new old
  | Type.Var' u <- n = do
      (new, old) <-
        -- Only add existential variables to the wanted list if they
        -- occur in a type we're trying to infer in the context. If
        -- they don't, they were added as instantiations of polymorphic
        -- types that might as well just be instantiated to {}.
        if keep u
          then pure (new, (loc, n) : old)
          else do
            _ <- defaultAbility n
            pure (new, old)
      coalesceWanted new old
  | otherwise = coalesceWanted' keep new ((loc, n) : old)

-- Wrapper for coalesceWanted' that ensures both lists are fully
-- expanded and calculates some necessary information for the main
-- procedure.
coalesceWanted ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  Wanted v loc ->
  M v loc (Wanted v loc)
coalesceWanted new old = do
  new <- expandWanted new
  old <- expandWanted old
  ctx <- getContext
  let invars (Type.Var' _) = mempty
      invars t = Type.freeVars t
      ivs = (foldMap . foldMap) invars (new ++ old)
      keep v@TypeVar.Existential {} = Set.member v ivs || occursAnn v ctx
      keep _ = True
  coalesceWanted' keep new old

coalesceWanteds ::
  (Var v) => (Ord loc) => [Wanted v loc] -> M v loc (Wanted v loc)
coalesceWanteds = foldM (flip coalesceWanted) []

-- | This implements the subtraction of handled effects from the
-- wanted effects of its body. Similar to merging, it presumes that
-- only one occurrence of each concrete ability is in play in the
-- scope, and will impose subtyping relations between the wanted and
-- handled abilities.
pruneWanted ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  Wanted v loc ->
  [Type v loc] ->
  M v loc (Wanted v loc)
pruneWanted acc [] _ = pure acc
pruneWanted acc ((loc, w) : want) handled
  | Just h <- find (headMatch w) handled = do
      subtype w h
      want <- expandWanted want
      handled <- expandAbilities handled
      pruneWanted acc want handled
  | otherwise = pruneWanted ((loc, w) : acc) want handled

-- | Processes wanted effects with respect to a portion of context
-- that is being discarded. This has the following consequences:
--   - All solved variables are substituted into the wanted abilities
--   - Effects that would involve escaping universal variables cause
--     an error, because they cannot possibly be satisfied.
--   - Unsolved existential abilities are defaulted to the empty row
--   - Abilities containing unsolved existentials that are going out
--     of scope cause an error, because it is unclear what they ought
--     to be solved to.
substAndDefaultWanted ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  [Element v loc] ->
  Map v (Monotype v loc) ->
  M v loc (Wanted v loc)
substAndDefaultWanted want ctx solved
  | want <- (fmap . fmap) (applyCtx solved) want,
    want <- filter q want,
    repush <- filter keep ctx =
      appendContext repush *> coalesceWanted want []
  where
    isExistential TypeVar.Existential {} = True
    isExistential _ = False

    -- get the free variables of things that aren't just variables
    necessary (Type.Var' _) = mempty
    necessary t = Set.filter isExistential $ Type.freeVars t

    keeps = foldMap (necessary . snd) want
    keep (Var v) = v `Set.member` keeps
    keep _ = False

    p (Var v) | isExistential v = Just v
    p _ = Nothing

    outScope = Set.fromList $ mapMaybe p ctx

    q (_, Type.Var' u) = u `Set.notMember` outScope
    q _ = True

-- Defaults unsolved ability variables to the empty row
defaultAbility :: (Var v) => (Ord loc) => Type v loc -> M v loc Bool
defaultAbility e@(Type.Var' (TypeVar.Existential b v)) =
  (True <$ instantiateL b v eff0) `orElse` pure False
  where
    eff0 = Type.effects (loc e) []
defaultAbility _ = pure False

-- Discards existential ability variables that only occur in covariant
-- position in the type. This is a useful step before generalization,
-- because it eliminates unnecessary variable parameterization.
--
-- Expects a fully substituted type, so that it is unnecessary to
-- check if an existential in the type has been solved.
discardCovariant :: (Var v) => Set v -> Type v loc -> Type v loc
discardCovariant _ ty | debugType "discardCovariant" ty = undefined
discardCovariant gens ty =
  ABT.rewriteDown (strip $ keepVarsT True ty) ty
  where
    keepVarsT pos (Type.Arrow' i o) =
      keepVarsT (not pos) i <> keepVarsT pos o
    keepVarsT pos (Type.Effect1' e o) =
      keepVarsT pos e <> keepVarsT pos o
    keepVarsT pos (Type.Effects' es) = foldMap (keepVarsE pos) es
    keepVarsT pos (Type.ForallNamed' _ t) = keepVarsT pos t
    keepVarsT pos (Type.IntroOuterNamed' _ t) = keepVarsT pos t
    keepVarsT _ t = foldMap exi $ Type.freeVars t

    exi (TypeVar.Existential _ v) = Set.singleton v
    exi _ = mempty

    keepVarsE pos (Type.Var' (TypeVar.Existential _ v))
      | pos, v `Set.member` gens = mempty
      | otherwise = Set.singleton v
    keepVarsE pos e = keepVarsT pos e

    strip keep t@(Type.Effect1' es0 o) =
      Type.effect (loc t) (discard keep $ Type.flattenEffects es0) o
    strip _ t = t

    discard keep es = filter p es
      where
        p (Type.Var' (TypeVar.Existential _ v)) = v `Set.member` keep
        p _ = True

-- Ability inference prefers minimal sets of abilities when
-- possible. However, such inference may disqualify certain TDNR
-- candicates due to a subtyping check with an overly minimal type.
-- It may be that the candidate's type would work fine, because the
-- inference was overly conservative about guessing which abilities
-- are in play.
--
-- `relax` adds an existential variable to the final inferred
-- abilities for such a function type if there isn't already one,
-- changing:
--
--   T ->{..} U ->{..} V
--
-- into:
--
--   T ->{..} U ->{e, ..} V
--
-- (where the `..` are presumed to be concrete) so that it can
-- behave better in the check.
--
-- It's possible this would allow an ability set that doesn't work,
-- but this is only used for type directed name resolution. A
-- separate type check must pass if the candidate is allowed, which
-- will ensure that the location has the right abilities.
relax :: (Var v) => (Ord loc) => Type v loc -> Type v loc
relax t = relax' True v t
  where
    fvs = foldMap f $ Type.freeVars t
    f (TypeVar.Existential _ v) = Set.singleton v
    f _ = mempty
    v = ABT.freshIn fvs $ Var.inferAbility

-- The worker for `relax`.
--
-- The boolean argument controls whether a non-arrow type is relaxed.
-- For example, the type:
--
--   Nat
--
-- is relaxed to:
--
--   {e} Nat
--
-- if True. This is desirable when doing TDNR, because a potential
-- effect reference may have type `{A} T` while the inferred necessary
-- type is just `T`. However, it is undesirable to add these variables
-- when relax' is used during variable instantiation, because it just
-- adds ability inference ambiguity.
relax' :: (Var v) => (Ord loc) => Bool -> v -> Type v loc -> Type v loc
relax' nonArrow v t
  | Type.Arrow' i o <- t =
      Type.arrow (ABT.annotation t) i $ relax' nonArrow v o
  | Type.ForallsNamed' vs b <- t =
      Type.foralls loc vs $ relax' nonArrow v b
  | Type.Effect' es r <- t,
    Type.Arrow' i o <- r =
      Type.effect loc es . Type.arrow (ABT.annotation t) i $ relax' nonArrow v o
  | Type.Effect' es r <- t =
      if any open es then t else Type.effect loc (tv : es) r
  | nonArrow = Type.effect loc [tv] t
  | otherwise = t
  where
    open (Type.Var' (TypeVar.Existential {})) = True
    open _ = False
    loc = ABT.annotation t
    tv = Type.var loc (TypeVar.Existential B.Blank v)

checkWantedScoped ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  Term v loc ->
  Type v loc ->
  M v loc (Wanted v loc)
checkWantedScoped want m ty =
  scope (InCheck m ty) $ checkWanted want m ty

checkWanted ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  Term v loc ->
  Type v loc ->
  M v loc (Wanted v loc)
-- ForallI
checkWanted want m (Type.Forall' body) = do
  v <- ABT.freshen body freshenTypeVar
  markThenRetractWanted v $ do
    x <- extendUniversal v
    checkWanted want m $
      ABT.bindInheritAnnotation body (universal' () x)
-- =>I
-- Lambdas are pure, so they add nothing to the wanted set
checkWanted want (Term.Lam' body) (Type.Arrow'' i es o) = do
  x <- ABT.freshen body freshenVar
  markThenRetract0 x $ do
    extendContext (Ann x i)
    body <- pure $ ABT.bindInheritAnnotation body (Term.var () x)
    checkWithAbilities es body o
  pure want
checkWanted want (Term.Let1Top' top binding m) t = do
  (tbinding, wbinding) <- synthesizeBinding top binding
  want <- coalesceWanted wbinding want
  v <- ABT.freshen m freshenVar
  markThenRetractWanted v $ do
    when (Var.isAction (ABT.variable m)) $
      -- enforce that actions in a block have type ()
      subtype tbinding (DDB.unitType (ABT.annotation binding))
    extendContext (Ann v tbinding)
    checkWanted want (ABT.bindInheritAnnotation m (Term.var () v)) t
checkWanted want (Term.LetRecNamed' [] m) t =
  checkWanted want m t
-- letrec can't have effects, so it doesn't extend the wanted set
checkWanted want (Term.LetRecTop' isTop lr) t =
  markThenRetractWanted (Var.named "let-rec-marker") $ do
    e <- annotateLetRecBindings isTop lr
    checkWanted want e t
checkWanted want e@(Term.Match' scrut cases) t = do
  (scrutType, swant) <- synthesize scrut
  want <- coalesceWanted swant want
  cwant <- checkCases scrutType t cases
  want <- coalesceWanted cwant want
  ctx <- getContext
  let matchType = apply ctx t
  getPatternMatchCoverageCheckAndKindInferenceSwitch >>= \case
    PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled ->
      ensurePatternCoverage e matchType scrut scrutType cases
    PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled ->
      pure ()
  pure want
checkWanted want e t = do
  (u, wnew) <- synthesize e
  ctx <- getContext
  subtype (apply ctx u) (apply ctx t)
  coalesceWanted wnew want

-- | Check that under the current context:
--     `m` has type `t` with abilities `es`,
-- updating the context in the process.
checkWithAbilities ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  Term v loc ->
  Type v loc ->
  M v loc ()
checkWithAbilities es m t = do
  want <- check m t
  subAbilities want es

-- traverse_ defaultAbility es

-- | Check that under the given context:
--     `m` has type `t`
-- updating the context in the process.
check ::
  (Var v) =>
  (Ord loc) =>
  Term v loc ->
  Type v loc ->
  M v loc (Wanted v loc)
check m t | debugShow ("check" :: String, m, t) = undefined
check m0 t0 = scope (InCheck m0 t0) $ do
  ctx <- getContext
  case minimize' m0 of
    Left m -> failWith $ DuplicateDefinitions m
    Right m
      | not (wellformedType ctx t0) ->
          failWith $ IllFormedType ctx
      | Type.Var' TypeVar.Existential {} <- t0 ->
          applyM t0 >>= checkWanted [] m
      | otherwise ->
          checkWanted [] m (Type.stripIntroOuters t0)

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: forall v loc. (Var v, Ord loc) => Type v loc -> Type v loc -> M v loc ()
subtype tx ty | debugTypes "subtype" tx ty = undefined
subtype tx ty = scope (InSubtype tx ty) $ do
  ctx <- getContext
  go (ctx :: Context v loc) (Type.stripIntroOuters tx) (Type.stripIntroOuters ty)
  where
    -- Rules from figure 9
    go :: Context v loc -> Type v loc -> Type v loc -> M v loc ()
    go _ (Type.Ref' r) (Type.Ref' r2) | r == r2 = pure () -- `Unit`
    go ctx t1@(Type.Var' (TypeVar.Universal v1)) t2@(Type.Var' (TypeVar.Universal v2)) -- `Var`
      | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2 =
          pure ()
    go ctx t1@(Type.Var' (TypeVar.Existential _ v1)) t2@(Type.Var' (TypeVar.Existential _ v2)) -- `Exvar`
      | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2 =
          pure ()
    go _ (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do
      -- `-->`
      subtype i2 i1
      ctx' <- getContext
      subtype (apply ctx' o1) (apply ctx' o2)
    go _ (Type.App' x1 y1) (Type.App' x2 y2) = do
      -- analogue of `-->`
      subtype x1 x2
      -- We don't know the variance of the type argument, so we assume
      -- (conservatively) that it's invariant, see
      -- discussion https://github.com/unisonweb/unison/issues/512
      y1 <- applyM y1
      y2 <- applyM y2
      equate y1 y2
    go _ t (Type.Forall' t2) = do
      v <- ABT.freshen t2 freshenTypeVar
      markThenRetract0 v $ do
        v' <- extendUniversal v
        t2 <- pure $ ABT.bindInheritAnnotation t2 (universal' () v')
        subtype t t2
    go _ (Type.Forall' t) t2 = do
      v0 <- ABT.freshen t freshenTypeVar
      markThenRetract0 v0 $ do
        v <- extendExistential v0
        t <- pure $ ABT.bindInheritAnnotation t (existential' () B.Blank v)
        t1 <- applyM t
        subtype t1 t2
    go _ (Type.Effect1' e1 a1) (Type.Effect1' e2 a2) = do
      subtype e1 e2
      ctx <- getContext
      subtype (apply ctx a1) (apply ctx a2)
    go _ a (Type.Effect1' _e2 a2) = subtype a a2
    go _ (Type.Effect1' es a) a2 = do
      subtype es (Type.effects (loc es) [])
      subtype a a2
    go ctx (Type.Var' (TypeVar.Existential b v)) t -- `InstantiateL`
      | Set.member v (existentials ctx)
          && notMember v (Type.freeVars t) = do
          instantiateL b v t
    go ctx t (Type.Var' (TypeVar.Existential b v)) -- `InstantiateR`
      | Set.member v (existentials ctx)
          && notMember v (Type.freeVars t) = do
          e <- extendExistential Var.inferAbility
          instantiateR (relax' False e t) b v
    go _ (Type.Effects' es1) (Type.Effects' es2) =
      subAbilities ((,) Nothing <$> es1) es2
    go _ t t2@(Type.Effects' _) | expand t = subtype (Type.effects (loc t) [t]) t2
    go _ t@(Type.Effects' _) t2 | expand t2 = subtype t (Type.effects (loc t2) [t2])
    go ctx _ _ = failWith $ TypeMismatch ctx

    expand :: Type v loc -> Bool
    expand t = case t of
      Type.Var' _ -> True
      Type.App' _ _ -> True
      Type.Ref' _ -> True
      _ -> False

equate :: (Var v) => (Ord loc) => Type v loc -> Type v loc -> M v loc ()
equate t1 t2 = scope (InEquate t1 t2) $ do
  ctx <- getContext
  guardWF ctx t1
  guardWF ctx t2
  equate0 t1 t2
  where
    guardWF ctx t@(Type.Var' _)
      | not $ wellformedType ctx t =
          failWith (TypeMismatch ctx)
    guardWF _ _ = pure ()

equate0 ::
  (Var v) =>
  (Ord loc) =>
  Type v loc ->
  Type v loc ->
  M v loc ()
equate0 (Type.Ref' r1) (Type.Ref' r2) | r1 == r2 = pure ()
equate0 t1@(Type.Var' tv1) t2@(Type.Var' tv2)
  | TypeVar.Universal v1 <- tv1,
    TypeVar.Universal v2 <- tv2,
    v1 == v2 =
      pure ()
  | TypeVar.Existential b1 v1 <- tv1,
    TypeVar.Existential b2 v2 <- tv2 =
      if v1 == v2
        then pure ()
        else do
          ctx <- getContext
          if ordered ctx v1 v2
            then instantiateL b2 v2 t1
            else instantiateL b1 v1 t2
  | TypeVar.Existential b v1 <- tv1 =
      instantiateL b v1 t2
  | TypeVar.Existential b v2 <- tv2 =
      instantiateL b v2 t1
equate0 (Type.Forall' t01) (Type.Forall' t02) = do
  v <- ABT.freshen t02 freshenTypeVar
  markThenRetract0 v $ do
    v <- universal' () <$> extendUniversal v
    let t1 = ABT.bindInheritAnnotation t01 v
        t2 = ABT.bindInheritAnnotation t02 v
    equate t1 t2
equate0 (Type.App' x1 y1) (Type.App' x2 y2) = do
  equate x1 x2
  y1 <- applyM y1
  y2 <- applyM y2
  equate y1 y2
equate0 (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do
  equate i1 i2
  o1 <- applyM o1
  o2 <- applyM o2
  equate o1 o2
equate0 (Type.Effect1' e1 a1) (Type.Effect1' e2 a2) = do
  equate e1 e2
  a1 <- applyM a1
  a2 <- applyM a2
  equate a1 a2
equate0 (Type.Var' (TypeVar.Existential b v)) t
  | notMember v (Type.freeVars t) =
      -- subtyping relaxes here, should equality
      instantiateL b v t
equate0 t (Type.Var' (TypeVar.Existential b v))
  | notMember v (Type.freeVars t) =
      -- subtyping relaxes here, should equality
      instantiateL b v t
equate0 (Type.Effects' es1) (Type.Effects' es2) =
  equateAbilities es1 es2
equate0 y1 y2 = do
  subtype y1 y2
  y1 <- applyM y1
  y2 <- applyM y2
  -- performing the subtype check in both directions means
  -- the types must be equal
  subtype y2 y1

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: (Var v, Ord loc) => B.Blank loc -> v -> Type v loc -> M v loc ()
instantiateL _ v t | debugEnabled && traceShow ("instantiateL" :: String, v, t) False = undefined
instantiateL blank v (Type.stripIntroOuters -> t) =
  scope (InInstantiateL v t) $
    getContext >>= \ctx -> case Type.monotype t of
      Just t ->
        solve ctx v t >>= \case
          Just ctx -> setContext ctx -- InstLSolve
          Nothing -> go ctx
      Nothing -> go ctx
  where
    go ctx = case t of
      Type.Var' (TypeVar.Existential _ v2)
        | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
            solve ctx v2 (Type.Monotype (existentialp (loc t) v))
              >>= maybe (failWith $ TypeMismatch ctx) setContext
      Type.Arrow' i o -> do
        -- InstLArr
        [i', o'] <- traverse freshenVar [nameFrom Var.inferInput i, nameFrom Var.inferOutput o]
        let s =
              Solved
                blank
                v
                ( Type.Monotype
                    ( Type.arrow
                        (loc t)
                        (existentialp (loc i) i')
                        (existentialp (loc o) o')
                    )
                )
        replaceContext
          (existential v)
          [existential o', existential i', s]
        instantiateR i B.Blank i' -- todo: not sure about this, could also be `blank`
        applyM o >>= instantiateL B.Blank o'
      Type.App' x y -> do
        -- analogue of InstLArr
        [x', y'] <- traverse freshenVar [nameFrom Var.inferTypeConstructor x, nameFrom Var.inferTypeConstructorArg y]
        let s =
              Solved
                blank
                v
                ( Type.Monotype
                    ( Type.app
                        (loc t)
                        (existentialp (loc x) x')
                        (existentialp (loc y) y')
                    )
                )
        replaceContext
          (existential v)
          [existential y', existential x', s]
        applyM x >>= instantiateL B.Blank x'
        applyM y >>= instantiateL B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshenVar Var.inferAbility
        vt' <- freshenVar Var.inferOther
        let t' =
              Type.effect1
                (loc t)
                (existentialp (loc es) es')
                (existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        replaceContext
          (existential v)
          [existential es', existential vt', s]
        applyM es >>= instantiateL B.Blank es'
        applyM vt >>= instantiateL B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom Var.inferAbility e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
        replaceContext
          (existential v)
          ((existential <$> es') ++ [s])
        Foldable.for_ (es' `zip` es) $ \(e', e) ->
          applyM e >>= instantiateL B.Blank e'
      Type.Forall' body -> do
        -- InstLIIL
        v0 <- ABT.freshen body freshenTypeVar
        markThenRetract0 v0 $ do
          v1 <- extendUniversal v0
          instantiateL B.Blank v (ABT.bindInheritAnnotation body (universal' () v1))
      _ -> failWith $ TypeMismatch ctx

nameFrom :: (Var v) => v -> Type v loc -> v
nameFrom _ (Type.Var' v) = TypeVar.underlying (Var.reset v)
nameFrom ifNotVar _ = ifNotVar

refineEffectVar ::
  (Var v) =>
  (Ord loc) =>
  loc ->
  [Type v loc] ->
  B.Blank loc ->
  v ->
  Type v loc ->
  M v loc ()
refineEffectVar _ es _ v _
  | debugShow ("refineEffectVar" :: Text, es, v) = undefined
refineEffectVar _ [] _ _ _ = pure ()
refineEffectVar l es blank v tv
  | ev <- TypeVar.Existential blank v,
    any (\e -> ev `Set.member` Type.freeVars e) es =
      getContext >>= failWith . AbilityCheckFailure [tv] es
  | otherwise = do
      slack <- freshenVar Var.inferAbility
      evs <- traverse (\e -> freshenVar (nameFrom Var.inferAbility e)) es
      let locs = loc <$> es
          es' = zipWith existentialp locs evs
          t' = Type.effects l (existentialp l slack : es')
          s = Solved blank v (Type.Monotype t')
          vs = existential slack : fmap existential evs
      replaceContext (existential v) (vs ++ [s])
      Foldable.for_ (es `zip` evs) $ \(e, ev) ->
        getContext >>= \ctx -> instantiateR (apply ctx e) B.Blank ev

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: (Var v, Ord loc) => Type v loc -> B.Blank loc -> v -> M v loc ()
instantiateR t _ v | debugEnabled && traceShow ("instantiateR" :: String, t, v) False = undefined
instantiateR (Type.stripIntroOuters -> t) blank v =
  scope (InInstantiateR t v) $
    getContext >>= \ctx -> case Type.monotype t of
      Just t ->
        solve ctx v t >>= \case
          Just ctx -> setContext ctx -- InstRSolve
          Nothing -> go ctx
      Nothing -> go ctx
  where
    go ctx = case t of
      Type.Var' (TypeVar.Existential _ v2)
        | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
            solve ctx v2 (Type.Monotype (existentialp (loc t) v))
              >>= maybe (failWith $ TypeMismatch ctx) setContext
      Type.Arrow' i o -> do
        -- InstRArrow
        [i', o'] <- traverse freshenVar [nameFrom Var.inferInput i, nameFrom Var.inferOutput o]
        let s =
              Solved
                blank
                v
                ( Type.Monotype
                    ( Type.arrow
                        (loc t)
                        (existentialp (loc i) i')
                        (existentialp (loc o) o')
                    )
                )
        replaceContext
          (existential v)
          [existential o', existential i', s]
        ctx <- instantiateL B.Blank i' i >> getContext
        instantiateR (apply ctx o) B.Blank o'
      Type.App' x y -> do
        -- analogue of InstRArr
        -- example foo a <: v' will
        -- 1. create foo', a', add these to the context
        -- 2. add v' = foo' a' to the context
        -- 3. recurse to refine the types of foo' and a'
        [x', y'] <- traverse freshenVar [nameFrom Var.inferTypeConstructor x, nameFrom Var.inferTypeConstructorArg y]
        let s = Solved blank v (Type.Monotype (Type.app (loc t) (existentialp (loc x) x') (existentialp (loc y) y')))
        replaceContext (existential v) [existential y', existential x', s]
        applyM x >>= \x -> instantiateR x B.Blank x'
        applyM y >>= \y -> instantiateR y B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshenVar (nameFrom Var.inferAbility es)
        vt' <- freshenVar (nameFrom Var.inferTypeConstructorArg vt)
        let t' =
              Type.effect1
                (loc t)
                (existentialp (loc es) es')
                (existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        replaceContext
          (existential v)
          [existential es', existential vt', s]
        applyM es >>= \es -> instantiateR es B.Blank es'
        applyM vt >>= \vt -> instantiateR vt B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom Var.inferAbility e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
        replaceContext
          (existential v)
          ((existential <$> es') ++ [s])
        Foldable.for_ (es `zip` es') $ \(e, e') -> do
          ctx <- getContext
          instantiateR (apply ctx e) B.Blank e'
      Type.Forall' body -> do
        -- InstRAIIL
        x' <- ABT.freshen body freshenTypeVar
        markThenRetract0 x' $ do
          appendContext [existential x']
          instantiateR (ABT.bindInheritAnnotation body (existential' () B.Blank x')) B.Blank v
      _ -> failWith $ TypeMismatch ctx

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α^ = τ,ΓR)
-- Solve the given existential variable to the given monotype.
-- If the given monotype is not well-formed at the context location
-- where the existential variable is introduced, return `Nothing`.
-- Fail with type mismatch if the existential is already solved to something else.
-- Fail with a compiler bug if the existential does not appear in the context at all.
solve :: (Var v, Ord loc) => Context v loc -> v -> Monotype v loc -> M v loc (Maybe (Context v loc))
solve ctx v t = case lookupSolved ctx v of
  Just t2 ->
    -- okay to solve something again if it's to an identical type
    if same t t2
      then pure (Just ctx)
      else failWith $ TypeMismatch ctx
    where
      same t1 t2 = apply ctx (Type.getPolytype t1) == apply ctx (Type.getPolytype t2)
  Nothing -> case breakAt (existential v) ctx of
    Just (ctxL, Existential blank v, ctxR) ->
      if wellformedType ctxL (Type.getPolytype t)
        then Just <$> ctxL `extendN` ((Solved blank v t) : ctxR)
        else pure Nothing
    _ -> compilerCrash $ UnknownExistentialVariable v ctx

expandAbilities ::
  (Var v) => (Ord loc) => [Type v loc] -> M v loc [Type v loc]
expandAbilities =
  fmap (concatMap Type.flattenEffects) . traverse applyM

expandWanted ::
  (Var v) => (Ord loc) => Wanted v loc -> M v loc (Wanted v loc)
expandWanted =
  (fmap . concatMap) (\(l, es) -> (,) l <$> Type.flattenEffects es)
    . (traverse . traverse) applyM

matchConcrete ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  [Type v loc] ->
  [Type v loc] ->
  [Type v loc] ->
  M v loc ([Type v loc], [Type v loc])
matchConcrete common acc [] _ = pure (reverse acc, common)
matchConcrete common acc (l : ls) rs
  | Just v <- find (headMatch l) common = do
      equate v l
      ls <- expandAbilities ls
      rs <- expandAbilities rs
      matchConcrete common acc ls rs
  | Just v <- find (headMatch l) rs = do
      equate v l
      ls <- expandAbilities ls
      rs <- expandAbilities rs
      matchConcrete (l : common) acc ls rs
  | otherwise = matchConcrete common (l : acc) ls rs

pruneConcrete ::
  (Var v) =>
  (Ord loc) =>
  (Maybe (Term v loc) -> Type v loc -> M v loc ()) ->
  Wanted v loc ->
  Wanted v loc ->
  [Type v loc] ->
  M v loc (Wanted v loc)
pruneConcrete _ acc [] _ = pure (reverse acc)
pruneConcrete missing acc ((loc, w) : ws) have
  | Just v <- find (headMatch w) have = do
      subtype v w `orElse` missing loc w
      ws <- expandWanted ws
      have <- expandAbilities have
      pruneConcrete missing acc ws have
  | otherwise = pruneConcrete missing ((loc, w) : acc) ws have

matchVariables ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  [Type v loc] ->
  [Type v loc] ->
  [Type v loc] ->
  ([Type v loc], [Type v loc], [Type v loc])
matchVariables com acc (l : ls) rs
  | isExistential l && any (== l) rs =
      matchVariables (l : com) acc (filter (/= l) ls) (filter (/= l) rs)
  | otherwise = matchVariables com (l : acc) ls rs
  where
    isExistential (Type.Var' TypeVar.Existential {}) = True
    isExistential _ = False
matchVariables com acc [] rs = (com, reverse acc, rs)

pruneVariables ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  Wanted v loc ->
  M v loc (Wanted v loc)
pruneVariables acc [] = pure $ reverse acc
pruneVariables acc ((loc, v) : vs) = do
  discard <- defaultAbility v
  vs <- expandWanted vs
  if discard
    then pruneVariables acc vs
    else pruneVariables ((loc, v) : acc) vs

pruneAbilities ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  [Type v loc] ->
  M v loc (Wanted v loc)
pruneAbilities want0 have0
  | debugShow ("pruneAbilities" :: Text, want0, have0) = undefined
pruneAbilities want0 have0 = do
  pwant <- pruneConcrete missing [] want0 have0
  if pwant /= want0
    then do
      want <- expandWanted pwant
      have <- expandAbilities have0
      pruneAbilities want have
    else -- fixed point

      if dflt
        then expandWanted =<< pruneVariables [] pwant
        else pure pwant
  where
    missing loc w = maybe id (scope . InSynthesize) loc $ do
      ctx <- getContext
      failWith $
        AbilityCheckFailure
          (Type.flattenEffects . apply ctx =<< have0)
          [w]
          ctx

    dflt = not $ any isExistential have0

isExistential :: Type v loc -> Bool
isExistential (Type.Var' TypeVar.Existential {}) = True
isExistential _ = False

matchAbilities ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  [Type v loc] ->
  M v loc ([Type v loc], [Type v loc], [Type v loc])
matchAbilities ls0 rs0 = do
  (ls, com) <- matchConcrete [] [] ls0 rs0
  rs <- expandAbilities rs0
  (rs, _) <- matchConcrete com [] rs ls
  ls <- expandAbilities ls
  if ls /= ls0 || rs /= rs0
    then matchAbilities ls rs
    else pure $ matchVariables [] [] ls rs

equateAbilities ::
  (Var v) =>
  (Ord loc) =>
  [Type v loc] ->
  [Type v loc] ->
  M v loc ()
equateAbilities abs1 abs2
  | debugShow ("equateAbilities" :: Text, abs1, abs2) = undefined
equateAbilities ls rs =
  matchAbilities ls rs >>= \(com, ls, rs) ->
    let (vls, cls) = partition isExistential ls
        (vrs, crs) = partition isExistential rs
        mlSlack
          | [t@(Type.Var' (TypeVar.Existential b v))] <- vls =
              Just (loc t, b, v)
          | otherwise = Nothing
        mrSlack
          | [t@(Type.Var' (TypeVar.Existential b v))] <- vrs =
              Just (loc t, b, v)
          | otherwise = Nothing
     in case liftA2 (,) mlSlack mrSlack of
          Just ((ll, bl, lSlack), (lr, br, rSlack)) ->
            refine True [(ll, bl, lSlack), (lr, br, rSlack)] [crs, cls]
          _
            | [t@(Type.Var' (TypeVar.Existential bc cv))] <- com,
              null vls,
              null vrs ->
                refine True [(loc t, bc, cv)] [cls ++ crs]
            | [] <- com, null rs, null cls -> for_ vls defaultAbility
            | [] <- com, null ls, null crs -> for_ vrs defaultAbility
            | [] <- com, Just pl <- mlSlack, null cls -> refine False [pl] [rs]
            | [] <- com, Just pr <- mrSlack, null crs -> refine False [pr] [ls]
            | otherwise -> getContext >>= failWith . AbilityEqFailure ls rs
  where
    refine common lbvs ess = do
      cv <- traverse freshenVar cn
      ctx <- getContext
      let (_, _, early) = minimumBy (cmp ctx) lbvs
          frsh e = freshenVar (nameFrom Var.inferAbility e)
      vss <- (traverse . traverse) frsh ess
      let evss = zipWith zip ess vss
          p ((_, _, u), _) = u == early
          bigList = case break p $ zip lbvs vss of
            (l, x : r) -> x : l ++ r
            _ -> error "impossible"
      for_ bigList $ \((l, b, v), us) ->
        let pre
              | v == early = fmap existential $ cv ++ join vss
              | otherwise = []
            t = Type.effects l (fmap (existentialp l) $ cv ++ us)
            s = Solved b v (Type.Monotype t)
         in replaceContext (existential v) (pre ++ [s])
      Foldable.for_ evss $ \evs -> Foldable.for_ evs $ \(e, v) ->
        getContext >>= \ctx -> instantiateR (apply ctx e) B.Blank v
      where
        cmp ctx (_, _, u) (_, _, v)
          | u == v = EQ
          | ordered ctx u v = LT
          | otherwise = GT
        cn | common = [Var.inferAbility] | otherwise = []

subAbilities ::
  (Var v) =>
  (Ord loc) =>
  Wanted v loc ->
  [Type v loc] ->
  M v loc ()
subAbilities want have
  | debugShow ("subAbilities" :: Text, want, have) = undefined
subAbilities want have = do
  want <- expandWanted want
  have <- expandAbilities have
  want <- expandWanted =<< pruneAbilities want have
  have <- expandAbilities have
  case (want, mapMaybe ex have) of
    ([], _) -> pure ()
    (want@((_, w) : _), [(b, ve, tv)]) ->
      refineEffectVar (loc w) (snd <$> want) b ve tv -- `orElse` die src w
    ((src, w) : _, _) -> die src w
  where
    ex t@(Type.Var' (TypeVar.Existential b v)) = Just (b, v, t)
    ex _ = Nothing
    die src w = maybe id (scope . InSynthesize) src do
      ctx <- getContext
      failWith $
        AbilityCheckFailure
          (Type.flattenEffects . apply ctx =<< have)
          [w]
          ctx

-- This function deals with collecting up a list of used abilities
-- during inference. Example: when inferring `x -> Stream.emit 42`, an
-- ambient existential `e` ability is created for the lambda.  In the
-- body of the lambda, requests are made for various abilities and
-- this branch finds the first unsolved ambient ability, `e`, and
-- solves that to `{r, e'}` where `e'` is another fresh existential.
-- In this way, a lambda whose body uses multiple effects can be
-- inferred properly.
subAmbient ::
  (Var v) =>
  (Ord loc) =>
  M v loc () ->
  [Type v loc] ->
  Type v loc ->
  M v loc ()
subAmbient die ambient r
  -- find unsolved existential, 'e, that appears in ambient
  | (b, e') : _ <- unsolveds = do
      -- introduce fresh existential 'e2 to context
      e2' <- extendExistential e'
      let et2 = Type.effects (loc r) [r, existentialp (loc r) e2']
      instantiateR et2 b e' `orElse` die
  | otherwise = die
  where
    unsolveds = (ambient >>= Type.flattenEffects >>= vars)
    vars (Type.Var' (TypeVar.Existential b v)) = [(b, v)]
    vars _ = []

abilityCheckSingle ::
  (Var v) =>
  (Ord loc) =>
  M v loc () ->
  [Type v loc] ->
  Type v loc ->
  M v loc ()
abilityCheckSingle die ambient r
  -- Look in ambient for exact match of head of `r`
  --   Ex: given `State Nat`, `State` is the head
  --   Ex: given `IO`,        `IO` is the head
  --   Ex: given `a`, where there's an exact variable
  -- If yes for `a` in ambient, do `subtype a r` and done.
  | Just a <- find (headMatch r) ambient =
      subtype a r `orElse` die
  -- It's an unsolved existential, instantiate it to all of ambient
  | Type.Var' tv@(TypeVar.Existential b v) <- r =
      let et2 = Type.effects (loc r) ambient
          acyclic
            | Set.member tv (Type.freeVars et2) =
                -- just need to trigger `orElse` in this case
                getContext >>= failWith . TypeMismatch
            | otherwise = instantiateR et2 b v
       in -- instantiate it to `{}` if can't cover all of ambient
          acyclic
            `orElse` instantiateR (Type.effects (loc r) []) b v
            `orElse` die
  | otherwise = subAmbient die ambient r

headMatch :: (Var v) => (Ord loc) => Type v loc -> Type v loc -> Bool
headMatch (Type.App' f _) (Type.App' f2 _) = headMatch f f2
headMatch r r2 = r == r2

abilityCheck' ::
  (Var v) => (Ord loc) => [Type v loc] -> [Type v loc] -> M v loc ()
abilityCheck' ambient0 requested0 = go ambient0 requested0
  where
    go _ [] = pure ()
    go ambient0 (r0 : rs) =
      applyM r0 >>= \case
        Type.Effects' es -> go ambient0 (es ++ rs)
        r -> do
          ambient <-
            concatMap Type.flattenEffects
              <$> traverse applyM ambient0
          abilityCheckSingle die ambient r
          go ambient rs

    die = do
      ctx <- getContext
      failWith $
        AbilityCheckFailure
          (apply ctx <$> ambient0)
          (apply ctx <$> requested0)
          ctx

verifyDataDeclarations :: (Var v, Ord loc) => DataDeclarations v loc -> Result v loc ()
verifyDataDeclarations decls = forM_ (Map.toList decls) $ \(_ref, decl) -> do
  let ctors = DD.constructors decl
  forM_ ctors $ \(_ctorName, typ) -> verifyClosed typ id

-- | public interface to the typechecker
synthesizeClosed ::
  (BuiltinAnnotation loc, Var v, Ord loc, Show loc) =>
  PrettyPrintEnv ->
  PatternMatchCoverageCheckAndKindInferenceSwitch ->
  [Type v loc] ->
  TL.TypeLookup v loc ->
  Term v loc ->
  Result v loc (Type v loc)
synthesizeClosed ppe pmcSwitch abilities lookupType term0 =
  let datas = TL.dataDecls lookupType
      effects = TL.effectDecls lookupType
      term = annotateRefs (TL.typeOfTerm' lookupType) term0
   in case term of
        Left missingRef ->
          compilerCrashResult (UnknownTermReference missingRef)
        Right term -> run ppe pmcSwitch datas effects $ do
          liftResult $
            verifyDataDeclarations datas
              *> verifyDataDeclarations (DD.toDataDecl <$> effects)
              *> verifyClosedTerm term
          doKindInference ppe datas effects term
          synthesizeClosed' abilities term

doKindInference ::
  ( Var v,
    Ord loc,
    BuiltinAnnotation loc,
    Show loc
  ) =>
  PrettyPrintEnv ->
  DataDeclarations v loc ->
  Map Reference (EffectDeclaration v loc) ->
  Term v loc ->
  MT v loc (Result v loc) ()
doKindInference ppe datas effects term = do
  getPatternMatchCoverageCheckAndKindInferenceSwitch >>= \case
    PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled -> pure ()
    PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled -> do
      let kindInferRes = do
            let decls = (Left <$> effects) <> (Right <$> datas)
            st <- KindInference.inferDecls ppe decls
            KindInference.kindCheckAnnotations ppe st (TypeVar.lowerTerm term)
      case kindInferRes of
        Left (ke Nel.:| _kes) -> failWith (KindInferenceFailure ke)
        Right () -> pure ()

verifyClosedTerm :: forall v loc. (Ord v) => Term v loc -> Result v loc ()
verifyClosedTerm t = do
  ok1 <- verifyClosed t id
  let freeTypeVars = Map.toList $ Term.freeTypeVarAnnotations t
      reportError (v, locs) = for_ locs $ \loc ->
        typeError (UnknownSymbol loc (TypeVar.underlying v))
  for_ freeTypeVars reportError
  when (not ok1 || (not . null) freeTypeVars) $ compilerBug (OtherBug "impossible")

verifyClosed :: (Traversable f, Ord v) => ABT.Term f v a -> (v -> v2) -> Result v2 a Bool
verifyClosed t toV2 =
  let isBoundIn v t = Set.member v (snd (ABT.annotation t))
      loc t = fst (ABT.annotation t)
      go t@(ABT.Var' v) | not (isBoundIn v t) = typeError (UnknownSymbol (loc t) $ toV2 v)
      go _ = pure True
   in all id <$> ABT.foreachSubterm go (ABT.annotateBound t)

annotateRefs ::
  (Applicative f, Var v) =>
  (Reference -> f (Type.Type v loc)) ->
  Term v loc ->
  f (Term v loc)
annotateRefs synth = ABT.visit f
  where
    f r@(Term.Ref' h) = Just (Term.ann ra (Term.ref ra h) <$> (ge <$> synth h))
      where
        ra = ABT.annotation r
        ge t = ABT.vmap TypeVar.Universal $ t
    f _ = Nothing

run ::
  (Var v, Ord loc, Functor f) =>
  PrettyPrintEnv ->
  PatternMatchCoverageCheckAndKindInferenceSwitch ->
  DataDeclarations v loc ->
  EffectDeclarations v loc ->
  MT v loc f a ->
  f a
run ppe pmcSwitch datas effects m =
  fmap fst
    . runM m ppe pmcSwitch datas effects
    $ Env 1 context0

synthesizeClosed' ::
  (Var v, Ord loc) =>
  [Type v loc] ->
  Term v loc ->
  M v loc (Type v loc)
synthesizeClosed' abilities term = do
  -- save current context, for restoration when done
  ctx0 <- getContext
  setContext context0
  (t, ctx, solved) <- markThenRetract (Var.named "start") $ do
    -- retract will cause notes to be written out for
    -- any `Blank`-tagged existentials passing out of scope
    (t, want) <- synthesize term
    scope (InSynthesize term) $
      t <$ subAbilities want abilities
  setContext ctx0 -- restore the initial context
  pure $ generalizeExistentials ctx solved t

-- Check if the given typechecking action succeeds.
succeeds :: M v loc a -> TotalM v loc Bool
succeeds m =
  MT \ppe pmccSwitch datas effects env ->
    case runM m ppe pmccSwitch datas effects env of
      Success _ _ -> Right (True, env)
      TypeError _ _ -> Right (False, env)
      CompilerBug bug _ _ -> Left bug

-- Check if `t1` is a subtype of `t2`. Doesn't update the typechecking context.
isSubtype' :: (Var v, Ord loc) => Type v loc -> Type v loc -> TotalM v loc Bool
isSubtype' type1 type2 = succeeds $ do
  let vars = Set.toList $ Set.union (ABT.freeVars type1) (ABT.freeVars type2)
  reserveAll (TypeVar.underlying <$> vars)
  appendContext (Var <$> vars)
  subtype type1 type2

-- See documentation at 'Unison.Typechecker.fitsScheme'
fitsScheme :: (Var v, Ord loc) => Type v loc -> Type v loc -> Either (CompilerBug v loc) Bool
fitsScheme type1 type2 = run PPE.empty PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled Map.empty Map.empty $
  succeeds $ do
    let vars = Set.toList $ Set.union (ABT.freeVars type1) (ABT.freeVars type2)
    reserveAll (TypeVar.underlying <$> vars)
    appendContext (Var <$> vars)
    type2 <- ungeneralize type2
    subtype type1 type2

-- `isRedundant userType inferredType` returns `True` if the `userType`
-- is equal "up to inferred abilities" to `inferredType`.
--
-- Example: `userType` is `Nat -> Nat`, `inferredType` is `∀ a . a ->{IO} a`.
--           In this case, the signature isn't redundant, and we return
--           `False`.
-- Example: `userType` is (`∀ a . a -> a`) and inferred is `∀ z e . z ->{e} z`.
--           In this case, the signature IS redundant, and we return `True`.
isRedundant ::
  (Var v, Ord loc) =>
  Type v loc ->
  Type v loc ->
  M v loc Bool
isRedundant userType0 inferredType0 = do
  ctx0 <- getContext
  -- the inferred type may have some unsolved existentials, which we generalize over
  -- before doing the comparison, otherwise it will just test equal to any
  -- concrete instantiation of those existentials. For instance, the
  -- inferred type `a -> a` for a existential `a` should get generalized
  -- to `∀ a . a -> a` before comparison to `Nat -> Nat`, otherwise the
  -- typechecker will solve `a = Nat` and call the types equal!
  userType <- existentializeArrows userType0
  inferredType <- generalizeExistentials' <$> applyM inferredType0
  -- We already know `inferred <: userType`, otherwise the user's given
  -- type would have caused the program not to typecheck! Ex: if user writes
  -- `: Nat -> Nat` when it has an inferred type of `a -> a`. So we only
  -- need to check the other direction to determine redundancy.
  (liftTotalM $ isSubtype' userType inferredType) <* setContext ctx0

-- Public interface to `isSubtype`
isSubtype ::
  (Var v, Ord loc) => Type v loc -> Type v loc -> Either (CompilerBug v loc) Bool
isSubtype t1 t2 =
  run PPE.empty PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled Map.empty Map.empty (isSubtype' t1 t2)

isEqual ::
  (Var v, Ord loc) => Type v loc -> Type v loc -> Either (CompilerBug v loc) Bool
isEqual t1 t2 =
  (&&) <$> isSubtype t1 t2 <*> isSubtype t2 t1

instance (Var v) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    e -> show e
  show (Solved _ v t) = "'" ++ Text.unpack (Var.name v) ++ " = " ++ TP.prettyStr Nothing PPE.empty (Type.getPolytype t)
  show (Ann v t) =
    Text.unpack (Var.name v)
      ++ " : "
      ++ TP.prettyStr Nothing PPE.empty t
  show (Marker v) = "|" ++ Text.unpack (Var.name v) ++ "|"

instance (Ord loc, Var v) => Show (Context v loc) where
  show ctx@(Context es _) = "Γ\n  " ++ (intercalate "\n  " . map (showElem ctx . fst)) (reverse es)
    where
      showElem _ctx (Var v) = case v of
        TypeVar.Universal x -> "@" <> show x
        e -> show e
      showElem ctx (Solved _ v (Type.Monotype t)) = "'" ++ Text.unpack (Var.name v) ++ " = " ++ TP.prettyStr Nothing PPE.empty (apply ctx t)
      showElem ctx (Ann v t) = Text.unpack (Var.name v) ++ " : " ++ TP.prettyStr Nothing PPE.empty (apply ctx t)
      showElem _ (Marker v) = "|" ++ Text.unpack (Var.name v) ++ "|"

instance (Monad f) => Monad (MT v loc f) where
  return = pure
  m >>= f = MT \ppe pmccSwitch datas effects env0 -> do
    (a, env1) <- runM m ppe pmccSwitch datas effects env0
    runM (f a) ppe pmccSwitch datas effects $! env1

instance (Monad f) => MonadFail.MonadFail (MT v loc f) where
  fail = error

instance (Monad f) => Applicative (MT v loc f) where
  pure a = MT (\_ _ _ _ env -> pure (a, env))
  (<*>) = ap

instance (Monad f) => MonadState (Env v loc) (MT v loc f) where
  get = MT \_ _ _ _ env -> pure (env, env)
  put env = MT \_ _ _ _ _ -> pure ((), env)

instance (MonadFix f) => MonadFix (MT v loc f) where
  mfix f = MT \ppe pmccSwitch a b c ->
    let res = mfix (\ ~(wubble, _finalenv) -> runM (f wubble) ppe pmccSwitch a b c)
     in res
