module Unison.Codebase.Editor.HandleInput.Ls
  ( handleLs,
  )
where

import Control.Monad.Reader (ask)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.Path (Path')
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Server.Backend qualified as Backend

handleLs :: Path' -> Cli ()
handleLs pathArg = do
  Cli.Env {codebase} <- ask
  pp <- Cli.resolvePath' pathArg
  projectRootBranch <- Cli.runTransaction $ Codebase.expectShallowProjectBranchRoot pp.branch
  entries <- liftIO (Backend.lsAtPath codebase projectRootBranch (pp.absPath))
  Cli.setNumberedArgs $ fmap (SA.ShallowListEntry pathArg) entries
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  -- This used to be a delayed action which only forced the loading of the root
  -- branch when it was necessary for printing the results, but that got wiped out
  -- when we ported to the new Cli monad.
  -- It would be nice to restore it, but it's pretty rare that it actually results
  -- in an improvement, so perhaps it's not worth the effort.
  let buildPPE = pure suffixifiedPPE
  Cli.respond $ ListShallow buildPPE entries
