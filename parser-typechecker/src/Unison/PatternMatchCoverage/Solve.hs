{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Unison.PatternMatchCoverage.Solve
  ( uncoverAnnotate,
    classify,
    expandSolution,
    generateInhabitants,
  )
where

import Control.Monad.State
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Unison.Builtin.Decls (unitRef)
import Unison.ConstructorReference (ConstructorReference, reference_)
import Unison.Debug (DebugFlag (PatternCoverageConstraintSolver), shouldDebug)
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.PatternMatchCoverage.Class
import Unison.PatternMatchCoverage.Constraint (Constraint)
import Unison.PatternMatchCoverage.Constraint qualified as C
import Unison.PatternMatchCoverage.EffectHandler
import Unison.PatternMatchCoverage.GrdTree
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.IntervalSet qualified as IntervalSet
import Unison.PatternMatchCoverage.Literal
import Unison.PatternMatchCoverage.NormalizedConstraints
import Unison.PatternMatchCoverage.PmGrd
import Unison.PatternMatchCoverage.PmLit (PmLit)
import Unison.PatternMatchCoverage.PmLit qualified as PmLit
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Pretty qualified as P
import Unison.Util.Recursion
import Unison.Var (Var)

-- | top-down traversal of the 'GrdTree' that produces:
--
-- * a refinement type describing values that do not match the 'GrdTree'
--   (the "uncovered" set)
-- * a new 'GrdTree' annotated with refinement types at the nodes describing
--   values that cause an effect to be performed and values that match
--   the case at the leaves.
--
-- If the former is inhabited then its inhabitants are unmatched
-- values. If the leaves of the latter are inhabited then the case is
-- redundant.
uncoverAnnotate ::
  forall vt v loc m l.
  (Pmc vt v loc m) =>
  Set (NormalizedConstraints vt v loc) ->
  GrdTree (PmGrd vt v loc) l ->
  ( m
      ( Set (NormalizedConstraints vt v loc),
        GrdTree (Set (NormalizedConstraints vt v loc)) (Set (NormalizedConstraints vt v loc), l)
      )
  )
uncoverAnnotate z grdtree0 = cata phi grdtree0 z
  where
    phi = \case
      -- There is no way to fail matching a leaf, return the empty set
      -- to represent false.
      LeafF l -> \nc -> do
        nc' <- ensureInhabited' nc
        pure (Set.empty, Leaf (nc', l))
      ForkF ks -> \nc0 -> do
        -- depth-first fold in match-case order to acculate the
        -- constraints for a match failure at every case.
        (ncfinal, ts) <- foldlM (\(nc, ts) a -> a nc >>= \(nc', t) -> pure (nc', t : ts)) (nc0, []) ks
        pure (ncfinal, Fork $ reverse ts)
      GrdF grd k -> \nc0 -> case grd of
        PmEffect var con convars -> handleGrd (PosEffect var (Effect con) convars) (NegEffect var (Effect con)) k nc0
        PmEffectPure var resume -> handleGrd (PosEffect var NoEffect [resume]) (NegEffect var NoEffect) k nc0
        PmCon var con convars -> do
          handleGrd (PosCon var con convars) (NegCon var con) k nc0
        PmLit var lit -> do
          handleGrd (PosLit var lit) (NegLit var lit) k nc0
        PmListHead listVar n el elt -> do
          nc <- addLiteral' nc0 (PosListHead listVar n el elt)
          k nc
        PmListTail listVar n el elt -> do
          nc <- addLiteral' nc0 (PosListTail listVar n el elt)
          k nc
        PmListInterval listVar lb ub -> do
          let iset = IntervalSet.singleton (lb, ub)
          handleGrd (NegListInterval listVar (IntervalSet.complement iset)) (NegListInterval listVar iset) k nc0
        PmBang var -> do
          (ncCont, t) <- k nc0
          ncEff <- addLiteral' nc0 (Effectful var)
          let t' = Grd ncEff t
          pure (ncCont, t')
        PmLet var expr typ -> do
          nc <- addLiteral' nc0 (Let var expr typ)
          k nc

    -- Constructors and literals are handled uniformly except that
    -- they pass different positive and negative literals.
    handleGrd pos neg k nc0 = do
      ncNoMatch <- addLiteral' nc0 neg
      ncMatch <- addLiteral' nc0 pos
      (ncMatch, t) <- k ncMatch
      -- A match can fail bacause it fails to match the immediate
      -- pattern or it can match the immediate pattern but fail to
      -- match some pattern or guard defined later in this same case.
      --
      -- This split can lead to an exponential number of terms, so we
      -- limit this growth to a constant, conservatively
      -- approximating. This is known as "throttling" in the paper and
      -- described in section 5.2.
      let ncFinalCandidate = Set.union ncMatch ncNoMatch
          ncFinal = case Set.size ncFinalCandidate >= 30 of
            True -> nc0
            False -> ncFinalCandidate
      pure (ncFinal, t)

    ensureInhabited' ::
      Set (NormalizedConstraints vt v loc) ->
      m (Set (NormalizedConstraints vt v loc))
    ensureInhabited' ncs0 = foldlM phi Set.empty ncs0
      where
        phi ncs nc =
          ensureInhabited initFuel nc <&> \case
            Nothing -> ncs
            Just nc -> Set.insert nc ncs

    -- Add a literal to each term in our DNF, dropping terms that
    -- become contradictory
    addLiteral' ::
      Set (NormalizedConstraints vt v loc) ->
      Literal vt v loc ->
      m (Set (NormalizedConstraints vt v loc))
    addLiteral' ncs0 lit = foldlM phi Set.empty ncs0
      where
        phi ncs nc =
          addLiteral lit nc <&> \case
            Nothing -> ncs
            Just nc -> Set.insert nc ncs

-- | Collect accessible, inaccessible, and redundant GRHSs
classify ::
  forall vt v loc l.
  GrdTree (Set (NormalizedConstraints vt v loc)) (Set (NormalizedConstraints vt v loc), l) ->
  ([l], [l], [l])
classify = cata classifyAlg

classifyAlg ::
  forall vt v loc l.
  GrdTreeF (Set (NormalizedConstraints vt v loc)) (Set (NormalizedConstraints vt v loc), l) ([l], [l], [l]) ->
  ([l], [l], [l])
classifyAlg = \case
  LeafF (rt, l) ->
    case inh rt of
      True -> ([l], [], [])
      False -> ([], [], [l])
  GrdF rt rest ->
    -- The presence of a 'GrdF' node indicates that an effect was
    -- performed (see 'uncoverAnnotate').
    case inh rt of
      True ->
        -- The rest of the subtree is redundant, but an effect is
        -- performed. Classify this as "Inaccessible".
        case rest of
          ([], [], x : xs) -> ([], [x], xs)
          _ -> rest
      False -> rest
  ForkF xs -> foldr (\(a, b, c) ~(acc, inacc, redun) -> (a ++ acc, b ++ inacc, c ++ redun)) ([], [], []) xs
  where
    -- inhabitation check
    inh = not . Set.null

-- | Expand a full DNF term (i.e. each term identifies exactly one
-- solution) into an inhabiting pattern.
generateInhabitants ::
  forall vt v loc.
  (Var v) =>
  v ->
  NormalizedConstraints vt v loc ->
  Pattern ()
generateInhabitants x nc =
  let (_xcanon, xvi, nc') = expectCanon x nc
   in case vi_con xvi of
        Vc'Constructor pos _neg -> case pos of
          Nothing -> Pattern.Unbound ()
          Just (dc, convars) ->
            Pattern.Constructor () dc (map (\(v, _) -> generateInhabitants v nc') convars)
        Vc'Effect pos _neg -> case pos of
          Nothing -> Pattern.Unbound ()
          Just (effHandler, convars) -> case effHandler of
            NoEffect -> Pattern.EffectPure () case convars of
              [(v, _)] -> generateInhabitants v nc'
              _ -> error "NoEffect has the incorrect number of convars"
            Effect cr -> Pattern.EffectBind () cr (map (\(v, _) -> generateInhabitants v nc') convars) (Pattern.Unbound ())
        Vc'Boolean pos _neg -> case pos of
          Nothing -> Pattern.Unbound ()
          Just b -> Pattern.Boolean () b
        Vc'ListRoot _typ consPos snocPos intset ->
          let matchedLength = on (+) length consPos snocPos
              mmaxLength = IntervalSet.lookupMax intset
              matchIsIncomplete = case mmaxLength of
                Nothing -> True
                Just maxLength -> matchedLength < maxLength
              rootPat = case matchIsIncomplete of
                True -> Pattern.Unbound ()
                False -> Pattern.SequenceLiteral () []
              snoced = foldr (\a b -> Pattern.SequenceOp () b Pattern.Snoc (generateInhabitants a nc')) rootPat snocPos
              consed = foldr (\a b -> Pattern.SequenceOp () (generateInhabitants a nc') Pattern.Cons b) snoced consPos
           in consed
        _ -> Pattern.Unbound ()

-- | Instantiate a variable to a given constructor.
instantiate ::
  forall vt v loc x m.
  (Pmc vt v loc m) =>
  Fuel ->
  NormalizedConstraints vt v loc ->
  v ->
  -- | constructor
  x ->
  -- | type of datacon's args
  [Type vt loc] ->
  -- | produce positive constraint
  (v -> x -> [(v, Type vt loc)] -> [Constraint vt v loc]) ->
  m (Maybe (NormalizedConstraints vt v loc, [(v, Type vt loc)]))
instantiate fuel nc x c argTyps posConstraint = do
  -- todo: centralize this declVar logic. Currently in 'addLiteral' and here.
  newVars :: [(var, typ)] <- traverse (\t -> (,t) <$> fresh) argTyps
  let nc' = foldr (\(v, t) b -> declVar v t id b) nc newVars
      cons = posConstraint x c newVars
  mnc <- runMaybeT do
    nc <- MaybeT (addConstraints cons nc')
    -- mark all new fields as dirty as we need to ensure they are
    -- inhabited
    let nc' =
          -- HACK ALERT: Even if an ability handler constructor is
          -- uninhabited, the type system requires at least one
          -- constructor of the ability to be matched on to determine
          -- that it should dischange the ability. So, we don't do the
          -- inhabitation check if it is for an ability type that we
          -- haven't handled in a prior case.
          let (_xcanon, xvi, _) = expectCanon x nc
              missingEffectBind r neg =
                foldr
                  ( \a b -> case a of
                      Effect cr | view reference_ cr == r -> False
                      _ -> b
                  )
                  True
                  neg
           in case vi_con xvi of
                Vc'Effect (Just (Effect cr, _)) neg | missingEffectBind (view reference_ cr) neg -> nc
                _ -> foldr (\(v, _) b -> markDirty v b) nc newVars
    -- branching factor
    let newFuel = case length newVars > 1 of
          True -> min fuel 3
          False -> fuel
    -- we must ensure that all strict fields are inhabited
    MaybeT (ensureInhabited newFuel nc')
  pure ((\x -> (x, newVars)) <$> mnc)

-- | Given a variable and a term in DNF, expand it to an identical DNF
-- expression with enough positive info to print pattern suggestions.
expandSolution ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  NormalizedConstraints vt v loc ->
  m (Set (NormalizedConstraints vt v loc))
expandSolution x nc =
  let go fuel x nc
        -- If we run out of fuel conservatively assume the term is
        -- inhabited.
        | fuel == 0 = pure (Set.singleton nc)
        | otherwise =
            let (_xcanon, xvi, nc') = expectCanon x nc
             in withConstructors (pure (Set.singleton nc')) xvi \cs posConstraint _negConstraint ->
                  -- We have some constructors to attempt
                  -- instantiation with. Instantiate each one, if
                  -- doesn't lead to a contradiction then add it to
                  -- the set of valid solutions.
                  let phi (cref, cvt) = do
                        instantiate initFuel nc' x cref cvt posConstraint >>= \case
                          Nothing -> pure Set.empty -- contradiction
                          Just (nc'', newVars) -> case newVars of
                            [] -> pure (Set.singleton nc'')
                            _ ->
                              -- If we have the match expression:
                              -- @
                              -- match blerg : Maybe (Maybe ()) with
                              --   Nothing -> ()
                              --   Just Nothing -> ()
                              -- @
                              --
                              -- Then we would like to suggest @Just (Just _)@ rather than @Just _@.
                              -- To accomplish this, we recurse and expand variables for which we have
                              -- positive or negative information.

                              -- branching factor
                              let newFuel = case length newVars > 1 of
                                    True -> min fuel 3
                                    False -> fuel
                               in Set.fromList
                                    <$> foldlM
                                      ( \b (v, _t) ->
                                          Set.toList . Set.unions
                                            <$> traverse
                                              ( \nc ->
                                                  case expectCanon v nc of
                                                    (_vc, vi, nc') -> case vi_con vi of
                                                      Vc'Constructor pos neg
                                                        -- always instantiate unit, this ensures we print tuple patterns correctly
                                                        | Type.Ref' x <- vi_typ vi, x == unitRef -> go newFuel v nc'
                                                        | Just _ <- pos -> go newFuel v nc'
                                                        | not (Set.null neg) -> go (newFuel - 1) v nc'
                                                      Vc'Effect pos neg
                                                        | Just _ <- pos -> go newFuel v nc'
                                                        | not (Set.null neg) -> go (newFuel - 1) v nc'
                                                      Vc'Boolean _pos neg
                                                        | not (Set.null neg) -> go (newFuel - 1) v nc'
                                                      Vc'ListRoot _typ _posCons _posSnoc neg
                                                        | not (IntervalSet.singleton (0, maxBound) == neg) -> go (newFuel - 1) v nc'
                                                      _ -> pure (Set.singleton nc')
                                              )
                                              b
                                      )
                                      [nc'']
                                      newVars
                   in foldr (\a b s -> phi a >>= \a' -> b (Set.union a' s)) pure cs Set.empty
   in go initFuel x nc

withConstructors ::
  forall vt v loc r m.
  (Pmc vt v loc m) =>
  m r ->
  VarInfo vt v loc ->
  ( forall x.
    [(x, [Type vt loc])] ->
    (v -> x -> [(v, Type vt loc)] -> [Constraint vt v loc]) ->
    (v -> x -> Constraint vt v loc) ->
    m r
  ) ->
  m r
withConstructors nil vinfo k = do
  getConstructors typ >>= \case
    AbilityType resultType cs -> do
      arg <- for (Map.toList cs) \(cref, _) -> do
        cvts <- getConstructorVarTypes typ cref
        pure (Effect cref, cvts)
      k ((NoEffect, [resultType]) : arg) (\v cref args -> [C.PosEffect v cref args]) (\v cref -> C.NegEffect v cref)
    ConstructorType cs -> do
      arg <- for cs \(_, cref, _) -> do
        cvts <- getConstructorVarTypes typ cref
        pure (cref, cvts)
      k arg (\v cref args -> [C.PosCon v cref args]) (\v cref -> C.NegCon v cref)
    SequenceType _cs ->
      let Vc'ListRoot elemType consPos snocPos iset = case vi_con vinfo of
            Vc'ListRoot {} -> vi_con vinfo
            _ -> error "impossible: constraint for sequence type not a list root"
          varCount = length consPos + length snocPos
          minLen = fromMaybe 0 $ IntervalSet.lookupMin iset

          mkPosCons :: (Int -> [v] -> [Constraint vt v loc]) -> Int -> [v] -> [Constraint vt v loc]
          mkPosCons z elvs0 = foldr (\_ b n (elv : elvs) -> C.PosListHead v n elv : b (n + 1) elvs) z consPos elvs0

          mkPosSnoc :: (Int -> [v] -> [Constraint vt v loc]) -> Int -> [v] -> [Constraint vt v loc]
          mkPosSnoc z elvs0 = foldr (\_ b n (elv : elvs) -> C.PosListTail v n elv : b (n + 1) elvs) z snocPos elvs0

          constraints :: [(([(v, Type vt loc)] -> [Constraint vt v loc], Constraint vt v loc), [Type vt loc])]
          constraints =
            let mk f elvs = mkPosCons (\_ elvs -> mkPosSnoc (\_ elvs -> f elvs) 0 elvs) 0 (map fst elvs)
             in [ ((mk \[] -> [], C.NegListInterval v (IntervalSet.singleton (minLen, maxBound))), replicate varCount elemType)
                ]

          mkPos _v (pos, _neg) args =
            pos args
          mkNeg _v (_pos, neg) =
            neg
       in k constraints mkPos mkNeg
    BooleanType -> do
      k [(True, []), (False, [])] (\v b _ -> [C.PosLit v (PmLit.Boolean b)]) (\v b -> C.NegLit v (PmLit.Boolean b))
    OtherType -> nil
  where
    typ = vi_typ vinfo
    v = vi_id vinfo

-- | Test that the given variable is inhabited. This test is
-- undecidable in general so we adopt a fuel based approach as
-- described in section 3.7.
inhabited ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  Fuel ->
  v ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
inhabited fuel x nc0 =
  let (_xcanon, xvi, nc') = expectCanon x nc0
      -- more ability handler hack stuff
      --
      -- We don't want to add a negative effect constraint as we want
      -- to ensure there is at least one constructor given per
      -- ability.
      shouldAddNegative :: Bool
      shouldAddNegative = case vi_con xvi of
        Vc'Effect {} -> False
        _ -> True
   in withConstructors (pure (Just nc')) xvi \cs posConstraint negConstraint ->
        -- one of the constructors must be inhabited, Return the
        -- first non-contradictory instantiation.
        let phi (cref, cvt) b nc = do
              instantiate fuel nc x cref cvt posConstraint >>= \case
                Nothing -> do
                  case shouldAddNegative of
                    True ->
                      -- record failed instantiation attempt so we don't
                      -- attempt to instantiate this constructor again
                      addConstraint (negConstraint x cref) nc >>= \case
                        Nothing -> b nc
                        Just nc -> b nc
                    False -> b nc
                Just _ -> pure (Just nc)
         in foldr phi (\_ -> pure Nothing) cs nc'

newtype Fuel = Fuel Int
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num)

initFuel :: Fuel
initFuel = 8

-- | Check that all variables marked dirty are inhabited.
ensureInhabited ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  Fuel ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
ensureInhabited fuel nc0@NormalizedConstraints {dirtySet}
  | fuel == 0 = pure (Just clean) -- out of fuel, assume inhabited
  | otherwise = do
      -- all dirty vars must be inhabited or this NormalizedConstraints
      -- is dropped
      let phi dirtyVar b nc = do
            nc <- MaybeT (inhabited (fuel - 1) dirtyVar nc)
            b nc
       in runMaybeT (foldr phi pure dirtySet clean)
  where
    clean = nc0 {dirtySet = mempty}

-- | Add a formula literal to our normalized constraint set. This
-- corresponds to fig 7.
addLiteral ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  Literal vt v loc ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
addLiteral lit0 nabla0 = runMaybeT do
  nc <- MaybeT $ case lit0 of
    F -> pure Nothing
    T -> pure (Just nabla0)
    PosCon var datacon convars ->
      let ctx = foldr (\(trm, typ) b -> declVar trm typ id b) nabla0 convars
          c = C.PosCon var datacon convars
       in addConstraint c ctx
    NegCon var datacon -> addConstraint (C.NegCon var datacon) nabla0
    PosEffect var datacon convars ->
      let ctx = foldr (\(trm, typ) b -> declVar trm typ id b) nabla0 convars
          c = C.PosEffect var datacon convars
       in addConstraint c ctx
    NegEffect var datacon -> addConstraint (C.NegEffect var datacon) nabla0
    PosLit var lit -> addConstraint (C.PosLit var lit) nabla0
    NegLit var lit -> addConstraint (C.NegLit var lit) nabla0
    PosListHead listRoot n listElem listElemType -> do
      let nabla1 = declVar listElem listElemType id nabla0
          c = C.PosListHead listRoot n listElem
      addConstraint c nabla1
    PosListTail listRoot n listElem listElemType -> do
      let nabla1 = declVar listElem listElemType id nabla0
          c = C.PosListTail listRoot n listElem
      addConstraint c nabla1
    NegListInterval listVar iset -> addConstraint (C.NegListInterval listVar iset) nabla0
    Effectful var -> addConstraint (C.Effectful var) nabla0
    Let var _expr typ -> pure (Just (declVar var typ id nabla0))
  MaybeT (ensureInhabited initFuel nc)

insertVarInfo ::
  forall vt v loc.
  (Ord v) =>
  v ->
  VarInfo vt v loc ->
  NormalizedConstraints vt v loc ->
  NormalizedConstraints vt v loc
insertVarInfo k v nc@NormalizedConstraints {constraintMap} =
  nc {constraintMap = UFMap.insert k v constraintMap}

-- | Add a constraint to our normalized constraint set. This
-- corresponds to fig 7.
addConstraint ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  Constraint vt v loc ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
addConstraint con0 nc = do
  ppe <- getPrettyPrintEnv
  debugConstraint ppe <$> case con0 of
    C.PosLit var pmlit ->
      let updateLiteral pos neg lit
            | Just lit1 <- pos,
              lit1 == lit =
                -- we already have this positive constraint
                (pure (), Ignore)
            | Set.member lit neg = (contradiction, Ignore)
            | otherwise = (pure (), Update (Just lit, neg))
       in modifyLiteralC var pmlit updateLiteral nc
    C.NegLit var pmlit ->
      let updateLiteral pos neg lit
            -- the constraint contradicts positive info
            | Just lit1 <- pos, lit1 == lit = (contradiction, Ignore)
            -- we already have this negative constraint
            | Set.member lit neg = (pure (), Ignore)
            | otherwise = (pure (), Update (pos, Set.insert lit neg))
       in modifyLiteralC var pmlit updateLiteral nc
    C.NegListInterval var negMatchInterval ->
      let updateList _typ pCons pSnoc posMatchInterval
            -- No lengths are accepted
            | IntervalSet.null newMatchInterval = (contradiction, Ignore)
            -- This length constraint forces equating some cons and snoc matches
            | let unconflictedLen = length pCons + length pSnoc,
              Just maxLen <- IntervalSet.lookupMax newMatchInterval,
              maxLen < unconflictedLen =
                let varsToEquate = unconflictedLen - maxLen
                    (newPSnoc, vars) =
                      let (_as, bs) = Seq.splitAt (length pCons - varsToEquate) pCons
                          (cs, ds) = Seq.splitAt (length pSnoc - varsToEquate) pSnoc
                       in (cs, zip (toList bs) (toList ds))
                 in (equate vars, Update (pCons, newPSnoc, newMatchInterval))
            | otherwise =
                (populateCons var pCons newMatchInterval, Update (pCons, pSnoc, newMatchInterval))
            where
              newMatchInterval = IntervalSet.difference posMatchInterval negMatchInterval
       in modifyListC var updateList nc
    C.PosListHead r n e ->
      let updateList _elmType posCons posSnocs iset
            -- there is an existing positive constraint on this element
            | Just existingElemVar <- Seq.lookup n posCons = (equate [(e, existingElemVar)], Ignore)
            -- a list of this length is proscribed
            | let minPatLen = length posCons + 1,
              Just maxLen <- IntervalSet.lookupMax iset,
              maxLen < minPatLen =
                (contradiction, Ignore)
            -- the length constraint forces us to equate some cons and snoc patterns
            | let unconflictedLen = length posCons + length posSnocs + 1,
              Just maxLen <- IntervalSet.lookupMax iset,
              maxLen < unconflictedLen =
                let posCons' = posCons Seq.|> e
                    e' = Seq.index posSnocs (maxLen - length posCons')
                 in (equate [(e, e')], Update (posCons', posSnocs, iset))
            | otherwise =
                let posCons' = posCons Seq.|> e
                    iset' = IntervalSet.delete (0, length posCons' - 1) iset
                 in (pure (), Update (posCons', posSnocs, iset'))
       in modifyListC r updateList nc
    C.PosListTail r n e ->
      let updateList _elmType posCons posSnoc iset
            -- there is an existing positive constraint on this element
            | Just existingElemVar <- Seq.lookup n posSnoc = (equate [(e, existingElemVar)], Ignore)
            -- a list of this length is proscribed
            | let minPatLen = length posSnoc + 1,
              Just maxLen <- IntervalSet.lookupMax iset,
              maxLen < minPatLen =
                (contradiction, Ignore)
            -- the length constraint forces us to equate some cons and snoc patterns
            | let unconflictedLen = length posCons + length posSnoc + 1,
              Just maxLen <- IntervalSet.lookupMax iset,
              maxLen < unconflictedLen =
                let posSnoc' = posSnoc Seq.|> e
                    e' = Seq.index posCons (maxLen - length posSnoc')
                 in (equate [(e, e')], Update (posCons, posSnoc', iset))
            | otherwise =
                let posSnoc' = posSnoc Seq.|> e
                    iset' = IntervalSet.delete (0, length posSnoc' - 1) iset
                 in (populateCons r posCons iset', Update (posCons, posSnoc', iset'))
       in modifyListC r updateList nc
    C.PosCon var datacon convars ->
      let updateConstructor pos neg
            | Just (datacon1, convars1) <- pos = case datacon == datacon1 of
                True -> do
                  -- we already have an assertion, so equate convars
                  let varsToEquate = zipWith (\(y, _) (z, _) -> (y, z)) convars convars1
                  (equate varsToEquate, Ignore)
                False -> (contradiction, Ignore)
            -- contradicts negative info
            | True <- Set.member datacon neg = (contradiction, Ignore)
            | otherwise =
                -- no conflicting info, add constraint
                (pure (), Update (Just (datacon, convars), neg))
       in modifyConstructorC var updateConstructor nc
    C.NegCon var datacon ->
      let updateConstructor pos neg
            -- contradicts positive info
            | Just (datacon1, _) <- pos, datacon1 == datacon = (contradiction, Ignore)
            -- we already have this negative constraint
            | Set.member datacon neg = (pure (), Ignore)
            | otherwise = (pure (), Update (pos, Set.insert datacon neg))
       in modifyConstructorC var updateConstructor nc
    C.PosEffect var datacon convars ->
      let updateConstructor pos neg
            | Just (datacon1, convars1) <- pos = case datacon == datacon1 of
                True -> do
                  -- we already have an assertion, so equate convars
                  let varsToEquate = zipWith (\(y, _) (z, _) -> (y, z)) convars convars1
                  (equate varsToEquate, Ignore)
                False -> (contradiction, Ignore)
            -- contradicts negative info
            | True <- Set.member datacon neg = (contradiction, Ignore)
            | otherwise =
                -- no conflicting info, add constraint
                (pure (), Update (Just (datacon, convars), neg))
       in modifyEffectC var updateConstructor nc
    C.NegEffect var datacon ->
      let updateConstructor pos neg
            -- contradicts positive info
            | Just (datacon1, _) <- pos, datacon1 == datacon = (contradiction, Ignore)
            -- we already have this negative constraint
            | Set.member datacon neg = (pure (), Ignore)
            | otherwise = (pure (), Update (pos, Set.insert datacon neg))
       in modifyEffectC var updateConstructor nc
    C.Effectful var ->
      case expectCanon var nc of
        (var, vi, nc)
          | otherwise -> pure $ Just $ insertVarInfo var vi {vi_eff = IsEffectful} nc
    C.Eq x y -> union x y nc
  where
    debugConstraint ppe x =
      let debugOutput =
            P.sep
              "\n"
              [ P.hang (P.red "input constraints: ") (prettyNormalizedConstraints ppe nc),
                P.hang (P.yellow "additional constraint: ") (C.prettyConstraint ppe con0),
                P.hang (P.green "resulting constraint: ") (maybe "contradiction" (prettyNormalizedConstraints ppe) x),
                ""
              ]
       in if shouldDebug PatternCoverageConstraintSolver then trace (P.toAnsiUnbroken debugOutput) x else x

-- | Like 'addConstraint', but for a list of constraints
addConstraints ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  [Constraint vt v loc] ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
addConstraints cs nc0 = runMaybeT $ foldlM (\b a -> MaybeT (addConstraint a b)) nc0 cs

-- | Equate two variables
union ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  v ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
union v0 v1 nc@NormalizedConstraints {constraintMap} =
  UFMap.union v0 v1 constraintMap noMerge \chosenCanon nonCanonValue m ->
    -- In this block we want to collect the constraints from the
    -- non-canonical value and add them to the canonical value.

    -- literals are handled uniformly
    let handleLit :: forall x. (x -> PmLit) -> Maybe x -> Set x -> ([Constraint vt v loc], [Constraint vt v loc])
        handleLit toPmLit pos neg =
          let posC = case pos of
                Nothing -> []
                Just lit -> [C.PosLit chosenCanon (toPmLit lit)]
              negC = foldr (\a b -> C.NegLit chosenCanon (toPmLit a) : b) [] neg
           in (posC, negC)
        constraints = posCon ++ negCon ++ effCon
        (posCon, negCon) = case vi_con nonCanonValue of
          Vc'Constructor pos neg ->
            let posC = case pos of
                  Nothing -> []
                  Just (datacon, convars) -> [C.PosCon chosenCanon datacon convars]
                negC = foldr (\a b -> C.NegCon chosenCanon a : b) [] neg
             in (posC, negC)
          Vc'Effect pos neg ->
            let posC = case pos of
                  Nothing -> []
                  Just (datacon, convars) -> [C.PosEffect chosenCanon datacon convars]
                negC = foldr (\a b -> C.NegEffect chosenCanon a : b) [] neg
             in (posC, negC)
          Vc'ListRoot _typ posCons posSnoc iset ->
            let consConstraints = map (\(i, x) -> C.PosListHead chosenCanon i x) (zip [0 ..] (toList posCons))
                snocConstraints = map (\(i, x) -> C.PosListTail chosenCanon i x) (zip [0 ..] (toList posSnoc))
                neg = [C.NegListInterval chosenCanon (IntervalSet.complement iset)]
             in (consConstraints ++ snocConstraints, neg)
          Vc'Boolean pos neg -> handleLit PmLit.Boolean pos neg
          Vc'Int pos neg -> handleLit PmLit.Int pos neg
          Vc'Nat pos neg -> handleLit PmLit.Nat pos neg
          Vc'Float pos neg -> handleLit PmLit.Float pos neg
          Vc'Text pos neg -> handleLit PmLit.Text pos neg
          Vc'Char pos neg -> handleLit PmLit.Char pos neg
        effCon = case vi_eff nonCanonValue of
          IsNotEffectful -> []
          IsEffectful -> [C.Effectful chosenCanon]
     in addConstraints constraints nc {constraintMap = m}
  where
    noMerge m = pure nc {constraintMap = m}

modifyListC ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  ( Type vt loc ->
    Seq v ->
    Seq v ->
    IntervalSet ->
    (C vt v loc m (), ConstraintUpdate (Seq v, Seq v, IntervalSet))
  ) ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
modifyListC v f nc0 =
  let (ccomp, nc1) = modifyListF v f nc0
   in fmap snd <$> runC nc1 ccomp

modifyListF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  ( Type vt loc ->
    Seq v ->
    Seq v ->
    IntervalSet ->
    f (ConstraintUpdate (Seq v, Seq v, IntervalSet))
  ) ->
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
modifyListF v f nc =
  let g vc = getCompose (posAndNegList (\typ pcons psnoc iset -> Compose (f typ pcons psnoc iset)) vc)
   in modifyVarConstraints v g nc

modifyConstructorC ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  ( (Maybe (ConstructorReference, [(v, Type vt loc)])) ->
    Set ConstructorReference ->
    (C vt v loc m (), ConstraintUpdate (Maybe (ConstructorReference, [(v, Type vt loc)]), Set ConstructorReference))
  ) ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
modifyConstructorC v f nc0 =
  let (ccomp, nc1) = modifyConstructorF v f nc0
   in fmap snd <$> runC nc1 ccomp

modifyConstructorF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  ( (Maybe (ConstructorReference, [(v, Type vt loc)])) ->
    Set ConstructorReference ->
    f (ConstraintUpdate (Maybe (ConstructorReference, [(v, Type vt loc)]), Set ConstructorReference))
  ) ->
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
modifyConstructorF v f nc =
  let g vc = getCompose (posAndNegConstructor (\pos neg -> Compose (f pos neg)) vc)
   in modifyVarConstraints v g nc

modifyEffectC ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  ( (Maybe (EffectHandler, [(v, Type vt loc)])) ->
    Set EffectHandler ->
    (C vt v loc m (), ConstraintUpdate (Maybe (EffectHandler, [(v, Type vt loc)]), Set EffectHandler))
  ) ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
modifyEffectC v f nc0 =
  let (ccomp, nc1) = modifyEffectF v f nc0
   in fmap snd <$> runC nc1 ccomp

modifyEffectF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  ( (Maybe (EffectHandler, [(v, Type vt loc)])) ->
    Set EffectHandler ->
    f (ConstraintUpdate (Maybe (EffectHandler, [(v, Type vt loc)]), Set EffectHandler))
  ) ->
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
modifyEffectF v f nc =
  let g vc = getCompose (posAndNegEffect (\pos neg -> Compose (f pos neg)) vc)
   in modifyVarConstraints v g nc

modifyLiteralC ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  v ->
  PmLit ->
  ( forall a.
    (Ord a) =>
    -- positive info
    Maybe a ->
    -- negative info
    Set a ->
    -- the passed in PmLit, unpacked
    a ->
    (C vt v loc m (), ConstraintUpdate (Maybe a, Set a))
  ) ->
  NormalizedConstraints vt v loc ->
  m (Maybe (NormalizedConstraints vt v loc))
modifyLiteralC v lit f nc0 =
  let (ccomp, nc1) = modifyLiteralF v lit f nc0
   in fmap snd <$> runC nc1 ccomp

-- | Update constraints on some literal by only depending on their Ord
-- instance.
modifyLiteralF ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  PmLit ->
  ( forall a.
    (Ord a) =>
    -- positive info
    Maybe a ->
    -- negative info
    Set a ->
    -- the passed in PmLit, unpacked
    a ->
    f (ConstraintUpdate (Maybe a, Set a))
  ) ->
  NormalizedConstraints vt v loc ->
  f (NormalizedConstraints vt v loc)
modifyLiteralF v lit f nc =
  let g vc = getCompose (posAndNegLiteral (\pos neg candidate -> Compose (f pos neg candidate)) lit vc)
   in modifyVarConstraints v g nc

modifyVarConstraints ::
  forall vt v loc f.
  (Var v, Functor f) =>
  v ->
  ( VarConstraints vt v loc ->
    f (ConstraintUpdate (VarConstraints vt v loc))
  ) ->
  NormalizedConstraints vt v loc ->
  -- | applied to 'Vc'Constructor'
  f (NormalizedConstraints vt v loc)
modifyVarConstraints v updateVarConstraint nc0 = do
  updateF v (\_v vi -> fmap (\vc -> vi {vi_con = vc}) <$> updateVarConstraint (vi_con vi)) nc0
{-# INLINE modifyVarConstraints #-}

-- | Modify the positive and negative constraints of a constructor.
posAndNegConstructor ::
  forall f vt v loc.
  (Functor f) =>
  ( (Maybe (ConstructorReference, [(v, Type vt loc)])) ->
    Set ConstructorReference ->
    f (Maybe (ConstructorReference, [(v, Type vt loc)]), Set ConstructorReference)
  ) ->
  VarConstraints vt v loc ->
  f (VarConstraints vt v loc)
posAndNegConstructor f = \case
  Vc'Constructor pos neg -> uncurry Vc'Constructor <$> f pos neg
  _ -> error "impossible: posAndNegConstructor called on something other than Vc'Constructor"
{-# INLINE posAndNegConstructor #-}

-- | Modify the positive and negative constraints of an effect.
posAndNegEffect ::
  forall f vt v loc.
  (Functor f) =>
  ( (Maybe (EffectHandler, [(v, Type vt loc)])) ->
    Set EffectHandler ->
    f (Maybe (EffectHandler, [(v, Type vt loc)]), Set EffectHandler)
  ) ->
  VarConstraints vt v loc ->
  f (VarConstraints vt v loc)
posAndNegEffect f = \case
  Vc'Effect pos neg -> uncurry Vc'Effect <$> f pos neg
  _ -> error "impossible: posAndNegEffect called on something other than Vc'Effect"
{-# INLINE posAndNegEffect #-}

-- | Modify the positive and negative constraints in a way that
-- doesn't rely upon the particular literal type, but only on it being
-- an instance of Ord.
posAndNegLiteral ::
  forall f vt v loc.
  (Functor f) =>
  ( forall a.
    (Ord a) =>
    Maybe a ->
    Set a ->
    a ->
    f (Maybe a, Set a)
  ) ->
  PmLit ->
  VarConstraints vt v loc ->
  f (VarConstraints vt v loc)
posAndNegLiteral f lit = \case
  Vc'Boolean pos neg
    | PmLit.Boolean b <- lit -> uncurry Vc'Boolean <$> f pos neg b
  Vc'Int pos neg
    | PmLit.Int b <- lit -> uncurry Vc'Int <$> f pos neg b
  Vc'Nat pos neg
    | PmLit.Nat b <- lit -> uncurry Vc'Nat <$> f pos neg b
  Vc'Float pos neg
    | PmLit.Float b <- lit -> uncurry Vc'Float <$> f pos neg b
  Vc'Text pos neg
    | PmLit.Text b <- lit -> uncurry Vc'Text <$> f pos neg b
  Vc'Char pos neg
    | PmLit.Char b <- lit -> uncurry Vc'Char <$> f pos neg b
  Vc'Constructor _ _ -> error "impossible: posAndNegLiteral called on constructor"
  _ -> error "impossible: incompatible PmLit and VarConstraints types"
{-# INLINE posAndNegLiteral #-}

posAndNegList ::
  forall f vt v loc.
  (Functor f) =>
  ( Type vt loc ->
    Seq v ->
    Seq v ->
    IntervalSet ->
    f (Seq v, Seq v, IntervalSet)
  ) ->
  VarConstraints vt v loc ->
  f (VarConstraints vt v loc)
posAndNegList f = \case
  Vc'ListRoot typ posCons posSnocs iset -> (\(posCons, posSnocs, iset) -> Vc'ListRoot typ posCons posSnocs iset) <$> f typ posCons posSnocs iset
  _ -> error "impossible: posAndNegList called on a something that isn't a list"
{-# INLINE posAndNegList #-}

newtype C vt v loc m a = C
  { unC ::
      NormalizedConstraints vt v loc ->
      m (Maybe (a, NormalizedConstraints vt v loc))
  }
  deriving
    (Functor, Applicative, Monad, MonadState (NormalizedConstraints vt v loc))
    via StateT (NormalizedConstraints vt v loc) (MaybeT m)
  deriving (MonadTrans) via ComposeT (StateT (NormalizedConstraints vt v loc)) MaybeT

contradiction :: (Applicative m) => C vt v loc m a
contradiction = C \_ -> pure Nothing

equate :: (Pmc vt v loc m) => [(v, v)] -> C vt v loc m ()
equate vs = addConstraintsC (map (uncurry C.Eq) vs)

lookupListElemTypeC :: (Pmc vt v loc m) => v -> C vt v loc m (Type vt loc)
lookupListElemTypeC listVar = do
  nc0 <- get
  let (_var, vi, nc1) = expectCanon listVar nc0
  put nc1
  pure $ getConst (posAndNegList (\elemTyp _ _ _ -> Const elemTyp) (vi_con vi))

addConstraintsC :: (Pmc vt v loc m) => [Constraint vt v loc] -> C vt v loc m ()
addConstraintsC cs = do
  nc <- get
  lift (addConstraints cs nc) >>= \case
    Nothing -> contradiction
    Just nc -> put nc

declVarC ::
  (Pmc vt v loc m) =>
  v ->
  Type vt loc ->
  (VarInfo vt v loc -> VarInfo vt v loc) ->
  C vt v loc m ()
declVarC v vt vimod = do
  nc0 <- get
  let nc1 = declVar v vt vimod nc0
  put nc1

freshC ::
  (Pmc vt v loc m) =>
  C vt v loc m v
freshC = lift fresh

populateCons :: (Pmc vt v loc m) => v -> Seq v -> IntervalSet -> C vt v loc m ()
populateCons listVar pCons iset = do
  case IntervalSet.lookupMin iset of
    Just minLen
      | minLen > 0,
        let targets = [length pCons .. minLen - 1],
        not (null targets) -> do
          elemTyp <- lookupListElemTypeC listVar
          for_ targets \idx -> do
            elv <- freshC
            declVarC elv elemTyp id
            addConstraintsC [C.PosListHead listVar idx elv]
    _ -> pure ()

runC ::
  (Applicative m) =>
  NormalizedConstraints vt v loc ->
  C vt v loc m a ->
  m (Maybe (a, NormalizedConstraints vt v loc))
runC nc0 ca = unC ca nc0
