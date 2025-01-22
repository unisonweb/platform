module Unison.Server.Local.Endpoints.Projects.Queries
  ( listProjects,
    listProjectBranches,
  )
where

import Data.Text (Text)
import U.Codebase.Sqlite.DbId (ProjectId)
import Unison.Server.Local.Endpoints.Projects.Types
import Unison.Sqlite

-- | Load all project listings, optionally requiring an infix match with a query.
listProjects :: Maybe Text -> Transaction [ProjectListing]
listProjects mayUnsafeQuery = do
  let mayQuery = fmap (likeEscape '\\') mayUnsafeQuery
  queryListRow
    [sql|
      SELECT project.name, branch.name
      FROM project
        LEFT JOIN most_recent_branch mrb
          ON project.id = mrb.project_id
        LEFT JOIN project_branch branch
          ON mrb.branch_id = branch.branch_id
        WHERE (:mayQuery IS NULL OR project.name LIKE '%' || :mayQuery || '%' ESCAPE '\')
      ORDER BY branch.last_accessed DESC NULLS LAST, project.name ASC
    |]

-- | Load all project listings, optionally requiring an infix match with a query.
listProjectBranches :: ProjectId -> Maybe Text -> Transaction [ProjectBranchListing]
listProjectBranches projectId mayUnsafeQuery = do
  let mayQuery = fmap (likeEscape '\\') mayUnsafeQuery
  queryListRow
    [sql|
        SELECT project_branch.name
        FROM project_branch
        WHERE project_branch.project_id = :projectId
          AND (:mayQuery IS NULL OR project_branch.name LIKE '%' || :mayQuery || '%' ESCAPE '\')
        ORDER BY project_branch.last_accessed DESC NULLS LAST, project_branch.name ASC
      |]
