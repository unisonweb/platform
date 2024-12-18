module Unison.PrettyPrintEnv.MonadPretty
  ( MonadPretty,
    Env (..),
    runPretty,
    addTypeVars,
    willCaptureType,
  )
where

import Control.Lens (views)
import Control.Monad.Reader (MonadReader, Reader, local, runReader)
import Data.Set qualified as Set
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

type MonadPretty v m = (Var v, MonadReader (Env v) m)

data Env v = Env
  { boundTerms :: !(Set v),
    boundTypes :: !(Set v),
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

runPretty :: (Var v) => PrettyPrintEnv -> Reader (Env v) a -> a
runPretty ppe m =
  runReader
    m
    Env
      { boundTerms = Set.empty,
        boundTypes = Set.empty,
        ppe
      }
