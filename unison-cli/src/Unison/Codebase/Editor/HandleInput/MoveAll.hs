module Unison.Codebase.Editor.HandleInput.MoveAll (handleMoveAll) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.MoveBranch (moveBranchFunc)
import Unison.Codebase.Editor.HandleInput.MoveTerm (moveTermSteps)
import Unison.Codebase.Editor.HandleInput.MoveType (moveTypeSteps)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Prelude

handleMoveAll :: Bool -> Path.Path' -> Path.Path' -> Text -> Cli ()
handleMoveAll hasConfirmed src' dest' description = do
  moveBranchFunc <- moveBranchFunc hasConfirmed src' dest'
  moveTermTypeSteps <- case (,) <$> Path.toName' src' <*> Path.toName' dest' of
    Nothing -> pure []
    Just (HQ'.NameOnly -> src, dest) -> do
      termSteps <- moveTermSteps src dest
      typeSteps <- moveTypeSteps src dest
      pure (termSteps ++ typeSteps)
  case (moveBranchFunc, moveTermTypeSteps) of
    (Nothing, []) -> Cli.respond (Output.MoveNothingFound src')
    (mupdates, steps) -> do
      pp <- Cli.getCurrentProjectPath
      Cli.updateAndStepAt description (pp ^. #branch) (maybeToList mupdates) steps
      Cli.respond Output.Success
