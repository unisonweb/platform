module Unison.Codebase.Editor.HandleInput.MoveBranch where

import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Output (Output (..))
import qualified Unison.Codebase.Path as Path
import Unison.Prelude

-- | Moves a branch and its history from one location to another, and saves the new root
-- branch.
doMoveBranch :: forall r. Text -> Bool -> Path.Path' -> Path.Path' -> Cli r ()
doMoveBranch actionDescription hasConfirmed src' dest' = do
  srcAbs <- Cli.resolvePath' src'
  destAbs <- Cli.resolvePath' dest'
  destBranchExists <- Cli.branchExistsAtPath' dest'
  let isRootMove = (Path.isRoot srcAbs || Path.isRoot destAbs)
  when (isRootMove && not hasConfirmed) do
    Cli.returnEarly MoveRootBranchConfirmation
  srcBranch <- Cli.expectBranchAtPath' src'

  -- We want the move to appear as a single step in the root namespace, but we need to make
  -- surgical changes in both the root and the destination, so we make our modifications at the shared parent of
  -- those changes such that they appear as a single change in the root.
  let (changeRootPath, srcLoc, destLoc) = Path.longestPathPrefix (Path.unabsolute srcAbs) (Path.unabsolute destAbs)
  Cli.updateAt actionDescription (Path.Absolute changeRootPath) \changeRoot ->
    changeRoot
      & Branch.modifyAt srcLoc (const Branch.empty)
      & Branch.modifyAt destLoc (const srcBranch)
  if (destBranchExists && not isRootMove)
    then Cli.respond (MovedOverExistingBranch dest')
    else Cli.respond Success
