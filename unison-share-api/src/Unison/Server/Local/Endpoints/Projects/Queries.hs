module Unison.Server.Local.Endpoints.Projects.Queries (listProjects) where

import Data.Text (Text)
import Unison.Server.Local.Endpoints.Projects.Types
import Unison.Sqlite

-- | Load all project listings, optionally requiring an infix match with a query.
listProjects :: Maybe Text -> Transaction [ProjectListing]
listProjects mayQuery =
  queryListRow
    [sql|
      SELECT project.name, branch.name
      FROM project
        LEFT JOIN most_recent_branch mrb
          ON project.id = mrb.project_id
        LEFT JOIN project_branch branch
          ON mrb.branch_id = branch.branch_id
        WHERE (:mayQuery IS NULL OR project.name LIKE '%' || :mayQuery || '%')
      ORDER BY branch.last_accessed DESC, project.name ASC
    |]
