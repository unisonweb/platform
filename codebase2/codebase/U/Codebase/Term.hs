module U.Codebase.Term
  ( Term,
    ClosedTerm,
    HashableTerm,
    HashableTermRef,
    Type,
    HashableTermLink,
    TermLink,
    TypeLink,
    F,
    ClosedF,
    F' (..),
    MatchCase (..),
    Pattern (..),
    SeqOp (..),
    extraMap,
    rmapPattern,
    matchCasePattern,
    unhashComponent,
  )
where

import Control.Lens hiding (List)
import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (Reference, Reference', TermRReference, TermReference, TypeReference)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent, Referent', ReferentH)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Type (TypeR)
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import U.Core.ABT.Var qualified as ABT
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import Unison.Prelude

type Term v = ABT.Term (F v) v ()

type Type v = TypeR TypeReference v

-- | A version of 'Term' but where TermRefs never have a 'Nothing' Hash, but instead self references
-- are filled with User Variable references
-- to the relevant piece of the component in a component map.
type HashableTerm v = ABT.Term (F' Text HashableTermRef TypeReference HashableTermLink TypeLink v) v ()

type HashableTermRef = Reference' Text Hash

type TermLink = ReferentH

type HashableTermLink = Referent' (Reference' Text Hash) (Reference' Text Hash)

type TypeLink = Reference

-- | A closed term has no free variables, nor holes that represent self-references.
type ClosedTerm v = ABT.Term (ClosedF v) v ()

-- | Base functor for terms in the Unison codebase
type F vt =
  F'
    Text
    TermRReference
    TypeReference
    TermLink
    TypeLink
    vt

type ClosedF vt =
  F'
    Text
    TermReference
    TypeReference
    Referent
    TypeReference
    vt

-- | Generalized version.  We could generalize further to allow sharing within
--  terms.
data F' text termRef typeRef termLink typeLink vt a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text text
  | Char Char
  | Ref termRef
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    Constructor typeRef ConstructorId
  | Request typeRef ConstructorId
  | Handle a a
  | App a a
  | Ann a (TypeR typeRef vt)
  | List (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    LetRec [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let a a
  | -- Pattern matching / eliminating data types, example:
    --  case x of
    --    Just n -> rhs1
    --    Nothing -> rhs2
    --
    -- translates to
    --
    --   Match x
    --     [ (Constructor 0 [Var], ABT.abs n rhs1)
    --     , (Constructor 1 [], rhs2) ]
    Match a [MatchCase text typeRef a]
  | TermLink termLink
  | TypeLink typeLink
  deriving (Foldable, Functor, Traversable, Show)

data MatchCase t r a = MatchCase (Pattern t r) (Maybe a) a
  deriving (Foldable, Functor, Generic, Generic1, Traversable, Show)

matchCasePattern :: Lens (MatchCase t r a) (MatchCase t' r' a) (Pattern t r) (Pattern t' r')
matchCasePattern f (MatchCase p a b) = (\p' -> MatchCase p' a b) <$> f p

data Pattern t r
  = PUnbound
  | PVar
  | PBoolean !Bool
  | PInt !Int64
  | PNat !Word64
  | PFloat !Double
  | PText !t
  | PChar !Char
  | PConstructor !r !ConstructorId [Pattern t r]
  | PAs (Pattern t r)
  | PEffectPure (Pattern t r)
  | PEffectBind !r !ConstructorId [Pattern t r] (Pattern t r)
  | PSequenceLiteral [Pattern t r]
  | PSequenceOp (Pattern t r) !SeqOp (Pattern t r)
  deriving (Generic, Functor, Foldable, Traversable, Show)

data SeqOp
  = PCons
  | PSnoc
  | PConcat
  deriving (Eq, Show)

extraMap ::
  forall
    text
    termRef
    typeRef
    termLink
    typeLink
    vt
    text'
    termRef'
    typeRef'
    termLink'
    typeLink'
    vt'
    v
    a.
  (Ord v, Ord vt') =>
  (text -> text') ->
  (termRef -> termRef') ->
  (typeRef -> typeRef') ->
  (termLink -> termLink') ->
  (typeLink -> typeLink') ->
  (vt -> vt') ->
  ABT.Term (F' text termRef typeRef termLink typeLink vt) v a ->
  ABT.Term (F' text' termRef' typeRef' termLink' typeLink' vt') v a
extraMap ftext ftermRef ftypeRef ftermLink ftypeLink fvt = go'
  where
    go' = ABT.transform go
    go :: forall x. F' text termRef typeRef termLink typeLink vt x -> F' text' termRef' typeRef' termLink' typeLink' vt' x
    go = \case
      Int i -> Int i
      Nat n -> Nat n
      Float d -> Float d
      Boolean b -> Boolean b
      Text t -> Text (ftext t)
      Char c -> Char c
      Ref r -> Ref (ftermRef r)
      Constructor r cid -> Constructor (ftypeRef r) cid
      Request r cid -> Request (ftypeRef r) cid
      Handle e h -> Handle e h
      App f a -> App f a
      Ann a typ -> Ann a (Type.rmap ftypeRef $ ABT.vmap fvt typ)
      List s -> List s
      If c t f -> If c t f
      And p q -> And p q
      Or p q -> Or p q
      Lam b -> Lam b
      LetRec bs b -> LetRec bs b
      Let a b -> Let a b
      Match s cs -> Match s (goCase <$> cs)
      TermLink r -> TermLink (ftermLink r)
      TypeLink r -> TypeLink (ftypeLink r)
    goCase :: MatchCase text typeRef x -> MatchCase text' typeRef' x
    goCase (MatchCase p g b) = MatchCase (goPat p) g b
    goPat = rmapPattern ftext ftypeRef

rmapPattern :: (t -> t') -> (r -> r') -> Pattern t r -> Pattern t' r'
rmapPattern ft fr = go
  where
    go = \case
      PUnbound -> PUnbound
      PVar -> PVar
      PBoolean b -> PBoolean b
      PInt i -> PInt i
      PNat n -> PNat n
      PFloat d -> PFloat d
      PText t -> PText (ft t)
      PChar c -> PChar c
      PConstructor r i ps -> PConstructor (fr r) i (go <$> ps)
      PAs p -> PAs (go p)
      PEffectPure p -> PEffectPure (go p)
      PEffectBind r i ps p -> PEffectBind (fr r) i (go <$> ps) (go p)
      PSequenceLiteral ps -> PSequenceLiteral (go <$> ps)
      PSequenceOp p1 op p2 -> PSequenceOp (go p1) op (go p2)

-- | Given the pieces of a single term component,
-- replaces all 'Nothing' self-referential hashes with a variable reference
-- to the relevant piece of the component in the component map.
unhashComponent ::
  forall v extra.
  ABT.Var v =>
  -- | The hash of the component, this is used to fill in self-references.
  Hash ->
  -- | A function to convert a reference to a variable. The actual var names aren't important.
  (Reference.Id -> v) ->
  -- A SINGLE term component. Self references should have a 'Nothing' hash in term
  -- references/term links
  Map Reference.Id (Term v, extra) ->
  -- | The component with all self-references replaced with variable references.
  Map Reference.Id (v, HashableTerm v, extra)
unhashComponent componentHash refToVar m =
  withGeneratedVars
    & traversed . _2 %~ fillSelfReferences
  where
    usedVars :: Set v
    usedVars = foldMap (Set.fromList . ABT.allVars . fst) m
    withGeneratedVars :: Map Reference.Id (v, Term v, extra)
    withGeneratedVars = evalState (Map.traverseWithKey assignVar m) usedVars
    assignVar :: Reference.Id -> (trm, extra) -> StateT (Set v) Identity (v, trm, extra)
    assignVar r (trm, extra) = (,trm,extra) <$> ABT.freshenS (refToVar r)
    fillSelfReferences :: Term v -> HashableTerm v
    fillSelfReferences = (ABT.cata alg)
      where
        rewriteTermReference :: Reference.Id' (Maybe Hash) -> Either v Reference.Reference
        rewriteTermReference rid@(Reference.Id mayH pos) =
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
        alg :: () -> ABT.ABT (F v) v (HashableTerm v) -> HashableTerm v
        alg () = \case
          ABT.Var v -> ABT.var () v
          ABT.Cycle body -> ABT.cycle () body
          ABT.Abs v body -> ABT.abs () v body
          ABT.Tm t -> case t of
            -- Check refs to see if they refer to the current component, replace them with
            -- vars if they do.
            (Ref (Reference.ReferenceDerived rid)) ->
              rewriteTermReference rid
                & either (ABT.var ()) (ABT.tm () . Ref)
            (Ref (Reference.ReferenceBuiltin t)) ->
              ABT.tm () $ Ref (Reference.ReferenceBuiltin t)
            TermLink referent -> case referent of
              Referent.Ref (Reference.ReferenceDerived rid) ->
                rewriteTermReference rid
                  & either (ABT.var ()) (ABT.tm () . TermLink . Referent.Ref)
              Referent.Ref (Reference.ReferenceBuiltin t) ->
                ABT.tm () $ TermLink (Referent.Ref (Reference.ReferenceBuiltin t))
              Referent.Con typeRef conId -> ABT.tm () $ TermLink (Referent.Con typeRef conId)
            -- All other cases are unchanged, but we need to manually reconstruct them to
            -- safely coerce the term types.
            Int i -> ABT.tm () $ Int i
            Nat n -> ABT.tm () $ Nat n
            Float d -> ABT.tm () $ Float d
            Boolean b -> ABT.tm () $ Boolean b
            Text t -> ABT.tm () $ Text t
            Char c -> ABT.tm () $ Char c
            Constructor typeRef conId -> ABT.tm () $ Constructor typeRef conId
            Request typeRef conId -> ABT.tm () $ Request typeRef conId
            Handle e h -> ABT.tm () $ Handle e h
            App f a -> ABT.tm () $ App f a
            Ann a typ -> ABT.tm () $ Ann a typ
            List s -> ABT.tm () $ List s
            If c t f -> ABT.tm () $ If c t f
            And p q -> ABT.tm () $ And p q
            Or p q -> ABT.tm () $ Or p q
            Lam b -> ABT.tm () $ Lam b
            LetRec bs b -> ABT.tm () $ LetRec bs b
            Let a b -> ABT.tm () $ Let a b
            Match s cases -> ABT.tm () $ Match s cases
            TypeLink r -> ABT.tm () $ TypeLink r
