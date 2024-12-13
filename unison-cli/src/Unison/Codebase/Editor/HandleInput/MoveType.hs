module Unison.Codebase.Editor.HandleInput.MoveType (doMoveType, moveTypeSteps) where

import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Prelude

moveTypeSteps ::
  HQ'.HashQualified (Path.Split Path') -> Path.Split Path' -> Cli [(Path.Absolute, Branch0 m -> Branch0 m)]
moveTypeSteps src' dest' = do
  src <- traverse Cli.resolveSplit' src'
  srcTypes <- Cli.getTypesAt src
  case Set.toList srcTypes of
    [] -> pure []
    _ : _ : _ -> do
      hqLength <- Cli.runTransaction Codebase.hashLength
      Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' Set.empty srcTypes)
    [srcType] -> do
      dest <- Cli.resolveSplit' dest'
      destTypes <- Cli.getTypesAt $ HQ'.NameOnly dest
      when (not (Set.null destTypes)) do
        Cli.returnEarly (Output.TypeAlreadyExists dest' destTypes)
      pure
        [ -- Mitchell: throwing away any hash-qualification here seems wrong!
          BranchUtil.makeDeleteTypeName (first (view PP.absPath_) $ HQ'.toName src) srcType,
          BranchUtil.makeAddTypeName (first (view PP.absPath_) dest) srcType
        ]

doMoveType :: HQ'.HashQualified (Path.Split Path') -> Path.Split Path' -> Text -> Cli ()
doMoveType src' dest' description = do
  steps <- moveTypeSteps src' dest'
  when (null steps) do
    Cli.returnEarly (Output.TypeNotFound src')
  pb <- Cli.getCurrentProjectBranch
  Cli.stepManyAt pb description steps
  Cli.respond Output.Success
