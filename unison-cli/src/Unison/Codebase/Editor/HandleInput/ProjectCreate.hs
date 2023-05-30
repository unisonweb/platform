-- | @project.create@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreate
  ( projectCreate,
  )
where

import Data.UUID.V4 qualified as UUID
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)
import Witch (unsafeFrom)

-- | Create a new project.
--
-- 1. If a project already exists with the given name, bail.
--
-- 2. Otherwise, create a scaffold out a new project with a "main" branch, and add it to the namespace (at a magic
--    location that the user isn't supposed to look at).
--
-- Big danger: we first commit the project identity and metadata (like its name) to the codebase, then manipulate our
-- in-memory namespace and flush its contents out in a separate transaction. This means that if lightning strikes at the
-- wrong time, we'll be in an inconsistent state.
--
-- This could be fixed in a few different ways:
--
--   1. Make a better `stepAt` helper that can mutate the namespace in a transaction.
--
--   2. Add more code to detect the inconsistency and work around it. For example, if we ever see that a project id
--      exists in the codebase but not at its corresponding place in the namespace, we could consider it garbage and
--      delete it. Then, any user who tried to create a project called "foo" shortly before getting hit by lightning
--      could simply try creating "foo" again later.
--
--   3. Don't store projects in the root namespace at all. We don't even want them there, it's just a little too
--      convenient because *not* storing them in the root namespace would require a lot of reworking and rewriting. We'd
--      rather hit some shorter-term project milestones and clean our mess up Later (TM).
--
-- For now, it doesn't seem worth it to do (1) or (2), since we want to do (3) eventually, and we'd rather not waste too
-- much time getting everything perfectly correct before we get there.
projectCreate :: ProjectName -> Cli ()
projectCreate projectName = do
  projectId <- liftIO (ProjectId <$> UUID.nextRandom)
  branchId <- liftIO (ProjectBranchId <$> UUID.nextRandom)

  let branchName = unsafeFrom @Text "main"
  Cli.runEitherTransaction do
    Queries.projectExistsByName projectName >>= \case
      False -> do
        Queries.insertProject projectId projectName
        Queries.insertProjectBranch
          Sqlite.ProjectBranch
            { projectId,
              branchId,
              name = branchName,
              parentBranchId = Nothing
            }
        Queries.setMostRecentBranch projectId branchId
        pure (Right ())
      True -> pure (Left (Output.ProjectNameAlreadyExists projectName))

  let path = projectBranchPath ProjectAndBranch {project = projectId, branch = branchId}
  Cli.stepAt "project.create" (Path.unabsolute path, const mainBranchContents)
  Cli.respond (Output.CreatedProject projectName branchName)
  Cli.cd path

-- The initial contents of the main branch of a new project.
--
-- FIXME we should put a README here, or something
mainBranchContents :: Branch0 m
mainBranchContents =
  Branch.empty0
