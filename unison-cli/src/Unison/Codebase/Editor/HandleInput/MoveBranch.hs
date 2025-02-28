module Unison.Codebase.Editor.HandleInput.MoveBranch (doMoveBranch, moveBranchFunc) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Prelude

-- | Note: Currently only allows moving within the same project-branch, should be easy to change in the future if
-- needed.
moveBranchFunc :: Bool -> Path.Path' -> Path.Path' -> Cli (Maybe (Path.Absolute, Branch IO -> Branch IO))
moveBranchFunc hasConfirmed src' dest' = do
  -- We currently only support moving within the same project branch.
  srcPP@(PP.ProjectPath _proj _projBranch srcAbs) <- Cli.resolvePath' src'
  PP.ProjectPath _ _ destAbs <- Cli.resolvePath' dest'
  destBranchExists <- Cli.branchExistsAtPath' dest'
  let isRootMove = (Path.isRoot srcAbs || Path.isRoot destAbs)
  when (isRootMove && not hasConfirmed) do
    Cli.returnEarly MoveRootBranchConfirmation
  Cli.getMaybeBranchFromProjectPath srcPP >>= traverse \srcBranch -> do
    -- We want the move to appear as a single step in the root namespace, but we need to make
    -- surgical changes in both the root and the destination, so we make our modifications at the shared parent of
    -- those changes such that they appear as a single change in the root.
    let (changeRootPath, srcLoc, destLoc) = Path.longestPathPrefix srcAbs destAbs
    let doMove changeRoot =
          changeRoot
            & Branch.modifyAt (Path.unrelative srcLoc) (const Branch.empty)
            & Branch.modifyAt (Path.unrelative destLoc) (const srcBranch)
    if (destBranchExists && not isRootMove)
      then Cli.respond (MovedOverExistingBranch dest')
      else pure ()
    pure (changeRootPath, doMove)

-- | Moves a branch and its history from one location to another, and saves the new root
-- branch.
doMoveBranch :: Text -> Bool -> Path.Path' -> Path.Path' -> Cli ()
doMoveBranch actionDescription hasConfirmed src' dest' = do
  moveBranchFunc hasConfirmed src' dest' >>= \case
    Nothing -> Cli.respond (BranchNotFound src')
    Just (absPath, func) -> do
      pp <- Cli.resolvePath' (Path.AbsolutePath' absPath)
      _ <- Cli.updateAt actionDescription pp func
      Cli.respond Success
