module Unison.PrettyPrintEnv.MonadPretty
  ( MonadPretty,
    Env (..),
    runPretty,
    addTypeVars,
    willCaptureType,
    withBoundTerm,
    withBoundTerms,
  )
where

import Control.Lens (views)
import Control.Monad.Reader (MonadReader, Reader, local, runReader)
import Data.Set qualified as Set
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Unison.Var qualified as Var

type MonadPretty v m = (Var v, MonadReader (Env v) m)

-- See Note [Bound and free term variables] for an explanation of boundTerms/freeTerms
data Env v = Env
  { boundTerms :: !(Set v),
    boundTypes :: !(Set v),
    freeTerms :: !(Set v),
    ppe :: !PrettyPrintEnv
  }
  deriving stock (Generic)

modifyTypeVars :: (MonadPretty v m) => (Set v -> Set v) -> m a -> m a
modifyTypeVars = local . over #boundTypes

-- | Add type variables to the set of variables that need to be avoided
addTypeVars :: (MonadPretty v m) => [v] -> m a -> m a
addTypeVars = modifyTypeVars . Set.union . Set.fromList

-- | Check if a list of type variables contains any variables that need to be
-- avoided
willCaptureType :: (MonadPretty v m) => [v] -> m Bool
willCaptureType vs = views #boundTypes (Set.intersects (Set.fromList vs))

withBoundTerm :: (MonadPretty v m) => v -> m a -> m a
withBoundTerm v =
  local (over #boundTerms (Set.insert (Var.reset v)))

withBoundTerms :: (MonadPretty v m) => [v] -> m a -> m a
withBoundTerms vs =
  local (over #boundTerms (Set.union (foldMap (Set.singleton . Var.reset) vs)))

runPretty :: (Var v) => PrettyPrintEnv -> Reader (Env v) a -> a
runPretty ppe m =
  runReader
    m
    Env
      { boundTerms = Set.empty,
        boundTypes = Set.empty,
        freeTerms = Set.empty,
        ppe
      }

-- Note [Bound and free term variables]
--
-- When rendering a Unison file, we render top-level bindings independently, which may end up referring to each
-- other after all are parsed together. Any individual term, therefore, may have free variables. For example,
--
--     foo = ... bar ...
--               ^^^
--               this "bar" variable is free in foo
--
--     bar = ...
--     ^^^
--     it's ultimately bound by a different top-level term rendering
--
-- Therefore, we pass down all free variables of a top-level term binding, so that we can decide, when rendering one of
-- them, whether to add a leading dot.
--
-- Now, when do we need to add a leading dot? Basically, any time a binder introduces a var that, when rendered reset,
-- clashes with the free var.
--
-- Here are a few examples using a made-up Unison syntax in which we can see whether a let is recursive or
-- non-recursive, and using "%" to separate a var name from its unique id.
--
-- Example 1
--
--   Made-up syntax                      Actual syntax
--   --------------                      ----------------
--   foo%0 =                             foo =
--     let bar%0 = bar%0                   bar = #someref -- rendered as ".bar", then parsed as var "bar"
--     in 5                                5
--
--   bar%0 = ...                         bar = ...
--
-- In this example, we have a non-recursive let that binds a local variable called bar%0. The body of the bar%0 binding
-- can itself refer to a different bar%0, which isn't captured, since a non-recursive let binding body can't refer to
-- the binding.
--
-- So, when rendering the free bar%0 in the right-hand side, we ask: should we add a leading dot? And the answer is: is
-- the var bar%0 in the set of all reset locally-bound vars {bar%0}? Yes? Then yes.
--
-- Example 2
--
--   Made-up syntax                      Actual syntax
--   --------------                      ----------------
--   foo%0 =                             foo =
--     letrec bar%1 = do bar%0 hey%0       bar = do #someref hey -- rendered as ".bar", then parsed as var "bar"
--            hey%0 = do bar%1             hey = do bar
--     in 5                                5
--
--   bar%0 = ...                         bar = ...
--
-- In this example, we have a recursive let that binds a bar%1 variable, and refers to bar%0 from inside. Same
-- situation, but variable resetting is relevant, because when walking underneath binder bar%1, we want to add its reset
-- form (bar%0) to the set of bound variables to check against, when rendering a free var (which we assume to have
-- unique id 0).
