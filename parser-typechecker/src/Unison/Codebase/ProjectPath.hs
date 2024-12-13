module Unison.Codebase.ProjectPath
  ( ProjectPathG (..),
    ProjectPathIds,
    ProjectPathNames,
    ProjectPath,
    fromProjectAndBranch,
    projectBranchRoot,
    toRoot,
    absPath_,
    path_,
    path,
    toProjectAndBranch,
    projectAndBranch_,
    toText,
    toIds,
    toNames,
    projectPathParser,
    parseProjectPath,

    -- * Re-exports, this also helps with using dot-notation
    ProjectAndBranch (..),
    Project (..),
    ProjectBranch (..),
  )
where

import Control.Lens hiding (from)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Text qualified as Text
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Project qualified as Project

data ProjectPathG proj branch = ProjectPath
  { project :: proj,
    branch :: branch,
    absPath :: Path.Absolute
  }
  deriving stock (Eq, Functor, Ord, Show, Generic)

type ProjectPathIds = ProjectPathG ProjectId ProjectBranchId

type ProjectPathNames = ProjectPathG ProjectName ProjectBranchName

instance From ProjectPath Text where
  from = from . toNames

instance From ProjectPathNames Text where
  from (ProjectPath proj branch (Path.Absolute path)) =
    into @Text (ProjectAndBranch proj branch) <> ":" <> Path.toText path

instance From (ProjectPathG () ProjectBranchName) Text where
  from (ProjectPath () branch (Path.Absolute path)) =
    "/" <> into @Text branch <> ":" <> Path.toText path

type ProjectPath = ProjectPathG Project ProjectBranch

projectBranchRoot :: ProjectAndBranch Project ProjectBranch -> ProjectPath
projectBranchRoot (ProjectAndBranch proj branch) = ProjectPath proj branch Path.root

-- | Discard any path within the project and get the project's root
toRoot :: ProjectPath -> ProjectPath
toRoot (ProjectPath proj branch _) = ProjectPath proj branch Path.root

fromProjectAndBranch :: ProjectAndBranch Project ProjectBranch -> Path.Absolute -> ProjectPath
fromProjectAndBranch (ProjectAndBranch proj branch) path = ProjectPath proj branch path

-- | Project a project context into a project path of just IDs
toIds :: ProjectPath -> ProjectPathIds
toIds (ProjectPath proj branch path) = ProjectPath (proj ^. #projectId) (branch ^. #branchId) path

-- | Project a project context into a project path of just names
toNames :: ProjectPath -> ProjectPathNames
toNames (ProjectPath proj branch path) = ProjectPath (proj ^. #name) (branch ^. #name) path

toProjectAndBranch :: ProjectPathG p b -> ProjectAndBranch p b
toProjectAndBranch (ProjectPath proj branch _) = ProjectAndBranch proj branch

instance Bifunctor ProjectPathG where
  bimap f g (ProjectPath p b path) = ProjectPath (f p) (g b) path

instance Bifoldable ProjectPathG where
  bifoldMap f g (ProjectPath p b _) = f p <> g b

instance Bitraversable ProjectPathG where
  bitraverse f g (ProjectPath p b path) = ProjectPath <$> f p <*> g b <*> pure path

toText :: ProjectPathG Project ProjectBranch -> Text
toText (ProjectPath proj branch path) =
  into @Text (proj ^. #name) <> "/" <> into @Text (branch ^. #name) <> ":" <> Path.toText path

absPath_ :: Lens' (ProjectPathG p b) Path.Absolute
absPath_ = lens absPath set
  where
    set (ProjectPath n b _) p = ProjectPath n b p

path :: (ProjectPathG p b) -> Path.Path
path (ProjectPath _ _ p) = Path.unabsolute p

path_ :: Lens' (ProjectPathG p b) Path.Path
path_ = absPath_ . Path.absPath_

projectAndBranch_ :: Lens (ProjectPathG p b) (ProjectPathG p' b') (ProjectAndBranch p b) (ProjectAndBranch p' b')
projectAndBranch_ = lens go set
  where
    go (ProjectPath proj branch _) = ProjectAndBranch proj branch
    set (ProjectPath _ _ p) (ProjectAndBranch proj branch) = ProjectPath proj branch p

type Parser = Megaparsec.Parsec Void Text

projectPathParser :: Parser ProjectPathNames
projectPathParser = do
  (projName, hasTrailingSlash) <- Project.projectNameParser
  projBranchName <- Project.projectBranchNameParser (not hasTrailingSlash)
  _ <- Megaparsec.char ':'
  path' >>= \case
    Path.AbsolutePath' p -> pure $ ProjectPath projName projBranchName p
    Path.RelativePath' {} -> fail "Expected an absolute path"
  where
    path' :: Parser Path.Path'
    path' = do
      pathStr <- Megaparsec.takeRest
      case Path.parsePath' (Text.unpack pathStr) of
        Left err -> fail (Text.unpack err)
        Right x -> pure x

parseProjectPath :: Text -> Either Text ProjectPathNames
parseProjectPath txt = first (Text.pack . Megaparsec.errorBundlePretty) $ Megaparsec.parse projectPathParser "" txt
