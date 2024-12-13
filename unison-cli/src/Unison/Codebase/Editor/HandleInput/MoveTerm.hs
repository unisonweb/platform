module Unison.Codebase.Editor.HandleInput.MoveTerm (doMoveTerm, moveTermSteps) where

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

moveTermSteps ::
  HQ'.HashQualified (Path.Split Path') -> Path.Split Path' -> Cli [(Path.Absolute, Branch0 m -> Branch0 m)]
moveTermSteps src' dest' = do
  src <- traverse Cli.resolveSplit' src'
  srcTerms <- Cli.getTermsAt src
  case Set.toList srcTerms of
    [] -> pure []
    _ : _ : _ -> do
      hqLength <- Cli.runTransaction Codebase.hashLength
      Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' srcTerms Set.empty)
    [srcTerm] -> do
      dest <- Cli.resolveSplit' dest'
      destTerms <- Cli.getTermsAt $ HQ'.NameOnly dest
      when (not (Set.null destTerms)) do
        Cli.returnEarly (Output.TermAlreadyExists dest' destTerms)
      pure
        [ -- Mitchell: throwing away any hash-qualification here seems wrong!
          BranchUtil.makeDeleteTermName (first (view PP.absPath_) $ HQ'.toName src) srcTerm,
          BranchUtil.makeAddTermName (first (view PP.absPath_) dest) srcTerm
        ]

doMoveTerm :: HQ'.HashQualified (Path.Split Path') -> Path.Split Path' -> Text -> Cli ()
doMoveTerm src' dest' description = do
  steps <- moveTermSteps src' dest'
  when (null steps) do
    Cli.returnEarly (Output.TermNotFound src')
  pb <- Cli.getCurrentProjectBranch
  Cli.stepManyAt pb description steps
  Cli.respond Output.Success
