-- | Handles generating kind constraints to be fed to the kind
-- constraint solver (found in "Unison.KindInference.Solve").
module Unison.KindInference.Generate
  ( typeConstraints,
    termConstraints,
    declComponentConstraints,
    builtinConstraints,
  )
where

import Data.Foldable (foldlM)
import Data.Set qualified as Set
import U.Core.ABT qualified as ABT
import Unison.Builtin.Decls (rewriteTypeRef)
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (builtinAnnotation))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl, asDataDecl)
import Unison.DataDeclaration qualified as DD
import Unison.Kind qualified as Unison
import Unison.KindInference.Constraint.Context (ConstraintContext (..))
import Unison.KindInference.Constraint.Provenance (Provenance (..))
import Unison.KindInference.Constraint.Provenance qualified as Provenance
import Unison.KindInference.Constraint.Unsolved (Constraint (..))
import Unison.KindInference.Generate.Monad (Gen, GeneratedConstraint, freshVar, lookupType, pushType, scopedType)
import Unison.KindInference.UVar (UVar)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Util.Recursion
import Unison.Var (Type (User), Var (typed), freshIn)

--------------------------------------------------------------------------------
-- Constraints arising from Types
--------------------------------------------------------------------------------

-- | Generate kind constraints arising from a given type. The given
-- @UVar@ is constrained to have the kind of the given type.
typeConstraints :: (Var v, Ord loc) => UVar v loc -> Type.Type v loc -> Gen v loc [GeneratedConstraint v loc]
typeConstraints resultVar typ =
  flatten bottomUp <$> typeConstraintTree resultVar typ

typeConstraintTree :: (Var v, Ord loc) => UVar v loc -> Type.Type v loc -> Gen v loc (ConstraintTree v loc)
typeConstraintTree resultVar term@ABT.Term {annotation, out} = do
  case out of
    ABT.Abs _ _ -> error "[typeConstraintTree] malformed type: Abs without an enclosing Forall or IntroOuter"
    ABT.Var v ->
      lookupType (Type.var annotation v) >>= \case
        Nothing -> error ("[typeConstraintTree] bug: encountered var " <> show v <> " missing from context")
        Just x -> pure $ Constraint (Unify (Provenance ContextLookup annotation) resultVar x) (Node [])
    ABT.Cycle _ -> error "[typeConstraintTree] malformed type: Encountered Cycle in a type?"
    ABT.Tm t0 -> case t0 of
      Type.Arrow dom cod -> do
        let ctx = AppArrow annotation dom cod
        k1 <- freshVar dom
        domConstraints <- typeConstraintTree k1 dom
        k2 <- freshVar cod
        codConstraints <- typeConstraintTree k2 cod
        pure $
          Constraint
            (IsType resultVar (Provenance ctx annotation))
            ( Node
                [ ParentConstraint (IsType k1 (Provenance ctx $ ABT.annotation dom)) domConstraints,
                  ParentConstraint (IsType k2 (Provenance ctx $ ABT.annotation cod)) codConstraints
                ]
            )
      Type.App abs arg -> do
        absVar <- freshVar abs
        absArgVar <- freshVar arg
        absConstraints <- typeConstraintTree absVar abs
        argVar <- freshVar arg
        argConstraints <- typeConstraintTree argVar arg
        let wellKindedAbs = IsArr absVar (Provenance (AppAbs absVar absArgVar) (ABT.annotation abs)) absArgVar resultVar
            applicationUnification = Unify (Provenance (AppArg absVar absArgVar argVar) (ABT.annotation arg)) absArgVar argVar
        pure $
          Constraint
            applicationUnification
            ( Node
                [ ParentConstraint wellKindedAbs absConstraints,
                  argConstraints
                ]
            )
      Type.Forall ABT.Term {annotation, out} -> case out of
        ABT.Abs v x ->
          scopedType (Type.var annotation v) \_ -> do
            typeConstraintTree resultVar x
        _ -> error "[typeConstraintTree] Forall wrapping a non-abs"
      Type.IntroOuter ABT.Term {annotation, out} -> case out of
        ABT.Abs v x -> handleIntroOuter v annotation (\c -> Constraint c <$> typeConstraintTree resultVar x)
        _ -> error "[typeConstraintTree] IntroOuter wrapping a non-abs"
      Type.Ann x kind -> do
        ct <- typeConstraintTree resultVar x
        gcs <- constrainToKind (Provenance Annotation annotation) resultVar (fromUnisonKind kind)
        pure (foldr Constraint ct gcs)
      Type.Ref r ->
        lookupType (Type.ref annotation r) >>= \case
          Nothing -> error ("[typeConstraintTree] Ref lookup failure: " <> show term)
          Just x -> pure $ Constraint (Unify (Provenance ContextLookup annotation) resultVar x) (Node [])
      Type.Effect effTyp b -> do
        effKind <- freshVar effTyp
        effConstraints <- typeConstraintTree effKind effTyp
        restConstraints <- typeConstraintTree resultVar b
        pure $ Node [effConstraints, restConstraints]
      Type.Effects effs -> do
        ParentConstraint (IsAbility resultVar (Provenance EffectsList annotation)) . Node <$> for effs \eff -> do
          effKind <- freshVar eff
          effConstraints <- typeConstraintTree effKind eff
          pure $ ParentConstraint (IsAbility effKind (Provenance EffectsList $ ABT.annotation eff)) effConstraints

handleIntroOuter :: (Var v) => v -> loc -> (GeneratedConstraint v loc -> Gen v loc r) -> Gen v loc r
handleIntroOuter v loc k = do
  let typ = Type.var loc v
  new <- freshVar typ
  orig <-
    lookupType typ >>= \case
      Nothing -> error "[malformed type] IntroOuter not in lexical scope of matching Forall"
      Just a -> pure a
  k (Unify (Provenance ScopeReference loc) new orig)

--------------------------------------------------------------------------------
-- Constraints arising from Type annotations
--------------------------------------------------------------------------------

-- | Check that all annotations in a term are well-kinded
termConstraints :: forall v loc. (Var v, Ord loc) => Term.Term v loc -> Gen v loc [GeneratedConstraint v loc]
termConstraints x = flatten bottomUp <$> termConstraintTree x

termConstraintTree :: forall v loc. (Var v, Ord loc) => Term.Term v loc -> Gen v loc (ConstraintTree v loc)
termConstraintTree = fmap Node . dfAnns processAnn cons nil . hackyStripAnns
  where
    processAnn :: loc -> Type.Type v loc -> Gen v loc [ConstraintTree v loc] -> Gen v loc [ConstraintTree v loc]
    processAnn ann typ mrest = do
      instantiateType typ \typ gcs -> do
        typKind <- freshVar typ
        annConstraints <- ParentConstraint (IsType typKind (Provenance TypeAnnotation ann)) <$> typeConstraintTree typKind typ
        let annConstraints' = foldr Constraint annConstraints gcs
        rest <- mrest
        pure (annConstraints' : rest)
    cons mlhs mrhs = (++) <$> mlhs <*> mrhs
    nil = pure []

-- | Helper for @termConstraints@ that instantiates the outermost
-- foralls and keeps the type in scope (in the type map) while
-- checking lexically nested type annotations.
instantiateType ::
  forall v loc r.
  (Var v, Ord loc) =>
  Type.Type v loc ->
  (Type.Type v loc -> [GeneratedConstraint v loc] -> Gen v loc r) ->
  Gen v loc r
instantiateType type0 k =
  let go acc = \case
        ABT.Tm' (Type.Forall ABT.Term {annotation, out = ABT.Abs x t}) -> do
          scopedType (Type.var annotation x) \_ -> do
            go acc t
        ABT.Tm' (Type.IntroOuter ABT.Term {annotation, out = ABT.Abs x t}) -> do
          handleIntroOuter x annotation \c -> go (c : acc) t
        t -> k t (reverse acc)
   in go [] type0

-- | Process type annotations depth-first. Allows processing
-- annotations with lexical scoping.
dfAnns :: (loc -> Type.Type v loc -> b -> b) -> (b -> b -> b) -> b -> Term.Term v loc -> b
dfAnns annAlg cons nil = cata \(ABT.Term' _ ann abt0) -> case abt0 of
  ABT.Var _ -> nil
  ABT.Cycle x -> x
  ABT.Abs _ x -> x
  ABT.Tm t -> case t of
    Term.Ann trm typ -> annAlg ann typ trm
    x -> foldr cons nil x

-- Our rewrite signature machinery generates type annotations that are
-- not well kinded. Work around this for now by stripping those
-- annotations.
hackyStripAnns :: (Ord v) => Term.Term v loc -> Term.Term v loc
hackyStripAnns =
  snd . cata \(ABT.Term' _ ann abt0) -> case abt0 of
    ABT.Var v -> (False, ABT.var ann v)
    ABT.Cycle (_, x) -> (False, ABT.cycle ann x)
    ABT.Abs v (_, x) -> (False, ABT.abs ann v x)
    ABT.Tm tm0 -> case tm0 of
      Term.App (isHack, abs) (_, arg) ->
        let argMod = case isHack of
              True -> stripAnns
              False -> id
         in (isHack, Term.app ann abs (argMod arg))
      Term.Constructor cref@(ConstructorReference r _) ->
        let isHack = r == rewriteTypeRef
         in (isHack, Term.constructor ann cref)
      t -> (False, ABT.tm ann (snd <$> t))
  where
    stripAnns = cata \(ABT.Term' _ ann abt0) -> case abt0 of
      ABT.Var v -> ABT.var ann v
      ABT.Cycle x -> ABT.cycle ann x
      ABT.Abs v x -> ABT.abs ann v x
      ABT.Tm tm0 -> case tm0 of
        Term.Ann trm _typ -> trm
        t -> ABT.tm ann t

--------------------------------------------------------------------------------
-- Constraints arising from Decls
--------------------------------------------------------------------------------

-- | Generate kind constraints for a mutally recursive component of
-- decls
declComponentConstraints ::
  forall v loc.
  (Var v, Ord loc) =>
  [(Reference, Decl v loc)] ->
  Gen v loc [GeneratedConstraint v loc]
declComponentConstraints decls = flatten bottomUp <$> declComponentConstraintTree decls

declComponentConstraintTree ::
  forall v loc.
  (Var v, Ord loc) =>
  [(Reference, Decl v loc)] ->
  Gen v loc (ConstraintTree v loc)
declComponentConstraintTree decls = do
  decls <- for decls \(ref, decl) -> do
    -- Add a kind variable for every datatype
    declKind <- pushType (Type.ref (DD.annotation $ asDataDecl decl) ref)
    pure (ref, decl, declKind)
  (declConstraints, constructorConstraints) <-
    unzip <$> for decls \(ref, decl, declKind) -> do
      let declAnn = DD.annotation $ asDataDecl decl
      let declType = Type.ref declAnn ref
      -- Unify the datatype with @k_1 -> ... -> k_n -> *@ where @n@ is
      -- the number of type parameters
      let tyVars = map (\tyVar -> Type.var declAnn tyVar) (DD.bound $ asDataDecl decl)
      tyvarKinds <- for tyVars \tyVar -> do
        -- it would be nice to annotate these type vars with their
        -- precise location, but that information doesn't seem to be
        -- available via "DataDeclaration", so we currently settle for
        -- the whole decl annotation.
        k <- freshVar tyVar
        pure (k, tyVar)

      let tyvarKindsOnly = map fst tyvarKinds
      constructorConstraints <-
        Node <$> for (DD.constructors' $ asDataDecl decl) \(constructorAnn, _, constructorType) -> do
          withInstantiatedConstructorType declType tyvarKindsOnly constructorType \constructorType -> do
            constructorKind <- freshVar constructorType
            ct <- typeConstraintTree constructorKind constructorType
            pure $ ParentConstraint (IsType constructorKind (Provenance DeclDefinition constructorAnn)) ct

      (fullyAppliedKind, _fullyAppliedType, declConstraints) <-
        let phi (dk, dt, cts) (ak, at) = do
              -- introduce a kind uvar for each app node
              let t' = Type.app declAnn dt at
              v <- freshVar t'
              let cts' = Constraint (IsArr dk (Provenance DeclDefinition declAnn) ak v) cts
              pure (v, t', cts')
         in foldlM phi (declKind, declType, Node []) tyvarKinds

      let finalDeclConstraints = case decl of
            Left _effectDecl -> Constraint (IsAbility fullyAppliedKind (Provenance DeclDefinition declAnn)) declConstraints
            Right _dataDecl -> Constraint (IsType fullyAppliedKind (Provenance DeclDefinition declAnn)) declConstraints
      pure (finalDeclConstraints, constructorConstraints)
  pure (Node declConstraints `StrictOrder` Node constructorConstraints)

-- | This is a helper to unify the kind constraints on type variables
-- across a decl's constructors.
--
-- With a decl like
--
-- @
-- unique type T a = C0 Nat a | C1 (a Nat)
-- @
--
-- @C0@ will have type @forall a. Nat -> a -> T a@ and @C1@ will have
-- type @forall a. (a Nat) -> T a@. We must unify the kind constraints
-- that the two constructors make on @a@ in order to determine the
-- kind of @a@ (or observe that there are contradictory
-- constraints). In this example @C0@ constrains @a@ to be of type *
-- because it is applied to the arrow type, whereas @C1@ constrains
-- @a@ to be of kind * -> * since it is applied to a Nat.
--
-- We unify these variables by instantiating the outermost foralls
-- with fresh kind variables, then follow any arrows to find the
-- result type which must have type @T b@ for some b, then unify @b@
-- with some kind variable representing the unification of @a@ for
-- each constructor.
withInstantiatedConstructorType ::
  forall v loc.
  (Var v, Ord loc) =>
  Type.Type v loc ->
  [UVar v loc] ->
  Type.Type v loc ->
  (Type.Type v loc -> Gen v loc (ConstraintTree v loc)) ->
  Gen v loc (ConstraintTree v loc)
withInstantiatedConstructorType declType tyParams0 constructorType0 k =
  let goForall constructorType = case constructorType of
        ABT.Tm' (Type.Forall ABT.Term {annotation, out = ABT.Abs x t}) -> do
          scopedType (Type.var annotation x) \_ -> do
            goForall t
        _ -> do
          cs <- goArrow constructorType
          rest <- k constructorType
          pure $ StrictOrder (foldr Constraint (Node []) cs) rest

      goArrow :: Type.Type v loc -> Gen v loc [GeneratedConstraint v loc]
      goArrow = \case
        Type.Arrow' _ o -> goArrow o
        Type.Effect' es _ -> goEffs es
        resultTyp@(Type.Apps' f xs)
          | f == declType -> unifyVars resultTyp xs
        f | f == declType -> pure []
        resultTyp -> error ("[goArrow] unexpected result type: " <> show resultTyp)

      goEffs = \case
        [] -> error ("[goEffs] couldn't find the expected ability: " <> show declType)
        e : es
          | e == declType -> pure []
          | Type.Apps' f xs <- e,
            f == declType ->
              unifyVars e xs
          | otherwise -> goEffs es

      unifyVars :: Type.Type v loc -> [Type.Type v loc] -> Gen v loc [GeneratedConstraint v loc]
      unifyVars typ vs = for (zip vs tyParams0) \(v, tp) -> do
        lookupType v >>= \case
          Nothing -> error ("[unifyVars] unknown type in decl result: " <> show v)
          Just x ->
            pure (Unify (Provenance DeclDefinition (ABT.annotation typ)) x tp)
   in goForall constructorType0

--------------------------------------------------------------------------------
-- Constraints on builtins
--------------------------------------------------------------------------------

-- | Constraints on language builtins, used to initialize the kind
-- inference state ('Unison.KindInference.Solve.initialState')
builtinConstraints :: forall v loc. (Ord loc, BuiltinAnnotation loc, Var v) => Gen v loc [GeneratedConstraint v loc]
builtinConstraints = flatten bottomUp <$> builtinConstraintTree

-- | Kind constraints for builtin types
builtinConstraintTree :: forall v loc. (Ord loc, BuiltinAnnotation loc, Var v) => Gen v loc (ConstraintTree v loc)
builtinConstraintTree =
  mergeTrees
    [ traverse
        (constrain Type)
        [ Type.nat,
          Type.int,
          Type.float,
          Type.boolean,
          Type.text,
          Type.char,
          Type.bytes,
          Type.any,
          Type.termLink,
          Type.typeLink,
          Type.fileHandle,
          flip Type.ref Type.filePathRef,
          Type.threadId,
          Type.socket,
          Type.udpSocket,
          Type.udpListenSocket,
          Type.udpClientSockAddr,
          Type.processHandle,
          Type.ibytearrayType,
          flip Type.ref Type.charClassRef,
          flip Type.ref Type.tlsRef,
          flip Type.ref Type.tlsClientConfigRef,
          flip Type.ref Type.tlsServerConfigRef,
          flip Type.ref Type.tlsSignedCertRef,
          flip Type.ref Type.tlsPrivateKeyRef,
          flip Type.ref Type.tlsCipherRef,
          flip Type.ref Type.tlsVersionRef,
          flip Type.ref Type.codeRef,
          flip Type.ref Type.valueRef,
          flip Type.ref Type.timeSpecRef,
          flip Type.ref Type.hashAlgorithmRef
        ],
      traverse
        (constrain (Type :-> Type))
        [ Type.list,
          Type.iarrayType,
          flip Type.ref Type.mvarRef,
          flip Type.ref Type.tvarRef,
          flip Type.ref Type.ticketRef,
          flip Type.ref Type.promiseRef,
          flip Type.ref Type.patternRef
        ],
      traverse
        (constrain Ability)
        [ Type.builtinIO,
          flip Type.ref Type.stmRef
        ],
      traverse
        (constrain (Type :-> Ability))
        [flip Type.ref Type.scopeRef],
      traverse
        (constrain (Ability :-> Type))
        [Type.mbytearrayType],
      traverse
        (constrain (Ability :-> Type :-> Type))
        [Type.effectType, Type.marrayType, Type.refType]
    ]
  where
    mergeTrees :: [Gen v loc [ConstraintTree v loc]] -> Gen v loc (ConstraintTree v loc)
    mergeTrees = fmap (Node . concat) . sequence

    constrain :: Kind -> (loc -> Type.Type v loc) -> Gen v loc (ConstraintTree v loc)
    constrain k t = do
      kindVar <- pushType (t builtinAnnotation)
      foldr Constraint (Node []) <$> constrainToKind (Provenance Builtin builtinAnnotation) kindVar k

--------------------------------------------------------------------------------
-- Helpers for constructing constraints
--------------------------------------------------------------------------------

-- | Constrain a @UVar@ to the provided @Kind@
constrainToKind :: (Var v) => Provenance v loc -> UVar v loc -> Kind -> Gen v loc [GeneratedConstraint v loc]
constrainToKind prov resultVar0 = fmap ($ []) . go resultVar0
  where
    go resultVar = \case
      Type -> do
        pure (IsType resultVar prov :)
      Ability -> do
        pure (IsAbility resultVar prov :)
      lhs :-> rhs -> do
        let inputTypeVar = Type.var (prov ^. Provenance.loc) (freshIn Set.empty (typed (User "a")))
        let outputTypeVar = Type.var (prov ^. Provenance.loc) (freshIn Set.empty (typed (User "a")))
        input <- freshVar inputTypeVar
        output <- freshVar outputTypeVar
        ctl <- go input lhs
        ctr <- go output rhs
        pure ((IsArr resultVar prov input output :) . ctl . ctr)

data Kind = Type | Ability | Kind :-> Kind

infixr 9 :->

-- | Convert the 'Unison.Kind' annotation type to our internal 'Kind'
fromUnisonKind :: Unison.Kind -> Kind
fromUnisonKind = \case
  Unison.Star -> Type
  Unison.Arrow a b -> fromUnisonKind a :-> fromUnisonKind b

--------------------------------------------------------------------------------
-- Constraint ordering
--------------------------------------------------------------------------------

-- | The order in which constraints are generated has a great impact
-- on the error observed. To separate the concern of constraint
-- generation and constraint ordering the constraints are generated as
-- a constraint tree, and the flattening of this tree determines the
-- generated constraint order.
data ConstraintTree v loc
  = Node [ConstraintTree v loc]
  | Constraint (GeneratedConstraint v loc) (ConstraintTree v loc)
  | ParentConstraint (GeneratedConstraint v loc) (ConstraintTree v loc)
  | StrictOrder (ConstraintTree v loc) (ConstraintTree v loc)

newtype TreeWalk = TreeWalk (forall a. ([a] -> [a]) -> [([a] -> [a], [a] -> [a])] -> [a] -> [a])

bottomUp :: TreeWalk
bottomUp = TreeWalk \down pairs0 -> foldr (\(d, u) b -> d . u . b) id pairs0 . down

flatten :: TreeWalk -> ConstraintTree v loc -> [GeneratedConstraint v loc]
flatten (TreeWalk f) = ($ []) . flattenTop
  where
    flattenTop :: ConstraintTree v loc -> [GeneratedConstraint v loc] -> [GeneratedConstraint v loc]
    flattenTop t0 =
      f id [flattenRec id t0]

    flattenRec ::
      ([GeneratedConstraint v loc] -> [GeneratedConstraint v loc]) ->
      ConstraintTree v loc ->
      ([GeneratedConstraint v loc] -> [GeneratedConstraint v loc], [GeneratedConstraint v loc] -> [GeneratedConstraint v loc])
    flattenRec down = \case
      Node cts ->
        let pairs = map (flattenRec id) cts
         in (f down pairs, id)
      Constraint c ct -> flattenRec (down . (c :)) ct
      ParentConstraint c ct ->
        let (down', up) = flattenRec down ct
         in (down', up . (c :))
      StrictOrder a b ->
        let as = flattenTop a
            bs = flattenTop b
         in (f down [(as . bs, id)], id)
