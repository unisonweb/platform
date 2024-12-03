-- | Pattern match coverage checking is implemented following the
-- algorithm described in [Lower Your
-- Guards](https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf). The
-- goal of pattern match coverage checking is to identify the
-- following problems that may arise in a pattern match:
--
-- * It is missing clauses (/i.e./ it is non-exhaustive)
-- * It contains redundant patterns (/i.e./ the case can be deleted without altering the program)
-- * It contains inaccessible patterns (/i.e/ the rhs can never be entered)
--
-- Furthermore, in the case of a non-exhaustive match, the goal to
-- present the user with concrete values that do not match any of the
-- existing patterns.
--
-- /N.B./ An inaccessible pattern in unison would be one that performs
-- effects in a guard although the constraints are unsatisfiable. Such
-- a pattern cannot be deleted without altering the program.
--
-- == High-level algorithm overview
--
-- 1. [Desugar]("Unison.PatternMatchCoverage.Desugar") a match expression into a 'Unison.PatternMatchCoverage.GrdTree.GrdTree'.
-- 2. Annotate the @GrdTree@ leaves with [refinement types]("Unison.PatternMatchCoverage.NormalizedConstraints")
-- describing values that match this branch. Redundant and inaccessible patterns are then identified by @GrdTree@ leaves
-- with uninhabited refinement types. Inaccessible patterns are distinguished by an effect being performed between the
-- @GrdTree@ root and the leaf.
-- 3. Traverse the @GrdTree@ building up a refinement type describing uncovered values. If the resulting refinement type
-- is inhabited then the match is missing clauses.
-- 4. Find inhabitants of the uncovered refinement type to present to the user.
--
-- Step (1) is implemented by 'desugarMatch'. Steps (2) and (3) are
-- implemented as a single traversal: 'uncoverAnnotate'/'classify'. Step (4) is
-- implemented by 'expandSolution'/'generateInhabitants'.
module Unison.PatternMatchCoverage
  ( checkMatch,
  )
where

import Data.Set qualified as Set
import Debug.Trace
import Unison.Debug
import Unison.Pattern (Pattern)
import Unison.PatternMatchCoverage.Class (Pmc (..))
import Unison.PatternMatchCoverage.Desugar (desugarMatch)
import Unison.PatternMatchCoverage.GrdTree (prettyGrdTree)
import Unison.PatternMatchCoverage.NormalizedConstraints qualified as NC
import Unison.PatternMatchCoverage.PmGrd (prettyPmGrd)
import Unison.PatternMatchCoverage.Solve (classify, expandSolution, generateInhabitants, uncoverAnnotate)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Util.Pretty qualified as P

-- | Perform pattern match coverage checking on a match expression
checkMatch ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  -- | scrutinee type
  Type.Type vt loc ->
  -- | match cases
  [Term.MatchCase loc (Term.Term' vt v loc)] ->
  -- | (redundant locations, inaccessible locations, inhabitants of uncovered refinement type)
  m ([loc], [loc], [Pattern ()])
checkMatch scrutineeType cases = do
  ppe <- getPrettyPrintEnv
  v0 <- fresh
  grdtree0 <- desugarMatch scrutineeType v0 cases
  doDebug (P.hang (title "desugared:") (prettyGrdTree (prettyPmGrd ppe) (\_ -> "<loc>") grdtree0)) (pure ())
  let initialUncovered = Set.singleton (NC.markDirty v0 $ NC.declVar v0 scrutineeType id NC.emptyNormalizedConstraints)
  (uncovered, grdtree1) <- uncoverAnnotate initialUncovered grdtree0
  doDebug
    ( P.sep
        "\n"
        [ P.hang (title "annotated:") (prettyGrdTree (NC.prettyDnf ppe) (NC.prettyDnf ppe . fst) grdtree1),
          P.hang (title "uncovered:") (NC.prettyDnf ppe uncovered)
        ]
    )
    (pure ())
  uncoveredExpanded <- concat . fmap Set.toList <$> traverse (expandSolution v0) (Set.toList uncovered)
  doDebug (P.hang (title "uncovered expanded:") (NC.prettyDnf ppe (Set.fromList uncoveredExpanded))) (pure ())
  let sols = map (generateInhabitants v0) uncoveredExpanded
  let (_accessible, inaccessible, redundant) = classify grdtree1
  pure (redundant, inaccessible, sols)
  where
    title = P.bold
    doDebug out = case shouldDebug PatternCoverage of
      True -> trace (P.toAnsiUnbroken out)
      False -> id
