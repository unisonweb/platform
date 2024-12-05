module Unison.Codebase.Editor.HandleInput.MoveTerm (doMoveTerm, moveTermSteps) where

import Control.Lens (_1, _2)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude

moveTermSteps :: HQ'.HashQualified Name -> Name -> Cli [(Path.Absolute, Branch0 m -> Branch0 m)]
moveTermSteps src' dest' = do
  src <- Cli.resolveHQName src'
  srcTerms <- Cli.getTermsAt src
  case Set.toList srcTerms of
    [] -> pure []
    _ : _ : _ -> do
      hqLength <- Cli.runTransaction Codebase.hashLength
      Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' srcTerms Set.empty)
    [srcTerm] -> do
      dest <- Cli.resolveName dest'
      destTerms <- Cli.getTermsAt (HQ'.NameOnly <$> dest)
      when (not (Set.null destTerms)) do
        Cli.returnEarly (Output.TermAlreadyExists dest' destTerms)
      let p = src & _1 %~ view PP.absPath_
      pure
        [ -- Mitchell: throwing away any hash-qualification here seems wrong!
          BranchUtil.makeDeleteTermName (over _2 HQ'.toName p) srcTerm,
          BranchUtil.makeAddTermName (over _1 (view PP.absPath_) dest) srcTerm
        ]

doMoveTerm :: HQ'.HashQualified Name -> Name -> Text -> Cli ()
doMoveTerm src' dest' description = do
  steps <- moveTermSteps src' dest'
  when (null steps) do
    Cli.returnEarly (Output.TermNotFound src')
  pb <- Cli.getCurrentProjectBranch
  Cli.stepManyAt pb description steps
  Cli.respond Output.Success
