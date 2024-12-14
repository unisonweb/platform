module Unison.CommandLine.BranchRelativePath
  ( BranchRelativePath (..),
    parseBranchRelativePath,
    branchRelativePathParser,
    parseIncrementalBranchRelativePath,
    IncrementalBranchRelativePath (..),
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Project qualified as Project
import Unison.Util.ColorText qualified as CT
import Unison.Util.Pretty qualified as P

data BranchRelativePath
  = -- | A path rooted at some specified branch/project
    BranchPathInCurrentProject ProjectBranchName Path.Absolute
  | QualifiedBranchPath ProjectName ProjectBranchName Path.Absolute
  | -- | A path which is relative to the user's current location.
    UnqualifiedPath Path.Path'
  deriving stock (Eq, Show)

instance Path.Pathy BranchRelativePath where
  descend brp seg = case brp of
    BranchPathInCurrentProject branch abs -> BranchPathInCurrentProject branch $ Path.descend abs seg
    QualifiedBranchPath proj branch abs -> QualifiedBranchPath proj branch $ Path.descend abs seg
    UnqualifiedPath path -> UnqualifiedPath $ Path.descend path seg
  prefix pre suf = case pre of
    BranchPathInCurrentProject branch abs -> BranchPathInCurrentProject branch $ Path.prefix abs suf
    QualifiedBranchPath proj branch abs -> QualifiedBranchPath proj branch $ Path.prefix abs suf
    UnqualifiedPath path -> UnqualifiedPath $ Path.prefix path suf
  split = \case
    BranchPathInCurrentProject branch abs -> first (BranchPathInCurrentProject branch) <$> Path.split abs
    QualifiedBranchPath proj branch abs -> first (QualifiedBranchPath proj branch) <$> Path.split abs
    UnqualifiedPath path -> first UnqualifiedPath <$> Path.split path
  toText = \case
    BranchPathInCurrentProject pbName path -> ProjectPath () pbName path & into @Text
    QualifiedBranchPath projName pbName path -> ProjectPath projName pbName path & into @Text
    UnqualifiedPath path' -> Path.toText path'

-- | Strings without colons are parsed as loose code paths. A path with a colon may specify:
-- 1. A project and branch
-- 2. Only a branch, in which case the project is assumed to be the current project
-- 3. Only a path, in which case the path is rooted at the branch root
--
-- Specifying only a project is not allowed.
--
-- >>> parseBranchRelativePath "foo"
-- Right (UnqualifiedPath foo)
-- >>> parseBranchRelativePath "foo/bar:"
-- Right (QualifiedBranchPath (UnsafeProjectName "foo") (UnsafeProjectBranchName "bar") .)
-- >>> parseBranchRelativePath "foo/bar:.some.path"
-- Right (QualifiedBranchPath (UnsafeProjectName "foo") (UnsafeProjectBranchName "bar") .some.path)
-- >>> parseBranchRelativePath "/bar:.some.path"
-- Right (BranchPathInCurrentProject (UnsafeProjectBranchName "bar") .some.path)
-- >>> parseBranchRelativePath ":.some.path"
-- Right (UnqualifiedPath .some.path)
--
-- >>> parseBranchRelativePath ".branch"
-- Right (UnqualifiedPath .branch)
parseBranchRelativePath :: String -> Either (P.Pretty CT.ColorText) BranchRelativePath
parseBranchRelativePath =
  first (P.string . Megaparsec.errorBundlePretty) . Megaparsec.parse branchRelativePathParser "<none>" . Text.pack

-- |
-- >>> from @BranchRelativePath @Text (BranchPathInCurrentProject "foo" (Path.absoluteEmpty "bar"))
instance From BranchRelativePath Text where
  from = Path.toText

data IncrementalBranchRelativePath
  = -- | no dots, slashes, or colons, so could be a project name or a single path segment
    ProjectOrPath' Text Path.Path'
  | -- | dots, no slashes or colons, must be a relative or absolute path
    OnlyPath' Path.Path'
  | -- | valid project, no slash
    IncompleteProject ProjectName
  | -- | valid project/branch, slash, no colon
    IncompleteBranch (Maybe ProjectName) (Maybe ProjectBranchName)
  | -- | valid project/branch, with colon
    IncompletePath (Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName) (Maybe Path.Absolute)
  | PathRelativeToCurrentBranch Path.Absolute
  deriving stock (Show)

-- |
-- >>> parseIncrementalBranchRelativePath "foo"
-- Right (ProjectOrRelative "foo" foo)
--
-- >>> parseIncrementalBranchRelativePath "foo/bar:"
-- Right (IncompletePath (Left (ProjectAndBranch {project = UnsafeProjectName "foo", branch = UnsafeProjectBranchName "bar"})) Nothing)
--
-- >>> parseIncrementalBranchRelativePath "foo/bar:some.path"
-- Right (IncompletePath (Left (ProjectAndBranch {project = UnsafeProjectName "foo", branch = UnsafeProjectBranchName "bar"})) (Just some.path))
--
-- >>> parseIncrementalBranchRelativePath "/bar:some.path"
-- Right (IncompletePath (Right (UnsafeProjectBranchName "bar")) (Just some.path))
--
-- >>> parseIncrementalBranchRelativePath ":some.path"
-- Right (PathRelativeToCurrentBranch some.path)
--
-- >>> parseIncrementalBranchRelativePath "/branch"
-- Right (IncompleteBranch Nothing (Just (UnsafeProjectBranchName "branch")))
--
-- >>> parseIncrementalBranchRelativePath "/"
-- Right (IncompleteBranch Nothing Nothing)
parseIncrementalBranchRelativePath :: String -> Either (P.Pretty CT.ColorText) IncrementalBranchRelativePath
parseIncrementalBranchRelativePath =
  first (P.string . Megaparsec.errorBundlePretty)
    . Megaparsec.parse incrementalBranchRelativePathParser "<none>"
    . Text.pack

incrementalBranchRelativePathParser :: Megaparsec.Parsec Void Text IncrementalBranchRelativePath
incrementalBranchRelativePathParser =
  asum
    [ startingAtSlash Nothing,
      pathRelativeToCurrentBranch,
      projectName
    ]
  where
    projectName = do
      -- Attempt to parse a project name from the string prefix, or a
      -- Path' cosuming the entire string, switch based on if we
      -- unambiguously parse one or the other.
      parseThese Project.projectNameParser path' >>= \case
        -- project name parser consumed the slash
        This (_, (projectName, True)) ->
          startingAtBranch (Just projectName)
        -- project name parser did not consume a slash
        --
        -- Either we are at the end of input or the next character
        -- is not a slash, so we have invalid input
        This (_, (projectName, False)) ->
          let end = do
                Megaparsec.eof
                pure (IncompleteProject projectName)
           in end <|> startingAtSlash (Just projectName)
        -- The string doesn't parse as a project name but does parse as a path
        That (_, path) -> pure (OnlyPath' path)
        -- The string parses both as a project name and a path
        These _ (_, path) -> ProjectOrPath' <$> Megaparsec.takeRest <*> pure path

    startingAtBranch :: Maybe ProjectName -> Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtBranch mproj =
      optionalBranch >>= \case
        Nothing -> pure (IncompleteBranch mproj Nothing)
        Just branch ->
          startingAtColon (maybe (Right branch) (\proj -> Left (ProjectAndBranch proj branch)) mproj)
            <|> pure (IncompleteBranch mproj (Just branch))

    startingAtSlash ::
      Maybe ProjectName ->
      Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtSlash mproj = Megaparsec.char '/' *> startingAtBranch mproj

    startingAtColon ::
      (Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName) ->
      Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    startingAtColon projStuff = do
      _ <- Megaparsec.char ':'
      p <- optionalEof brPath
      pure (IncompletePath projStuff p)

    pathRelativeToCurrentBranch :: Megaparsec.Parsec Void Text IncrementalBranchRelativePath
    pathRelativeToCurrentBranch = do
      _ <- Megaparsec.char ':'
      p <- brPath
      pure (PathRelativeToCurrentBranch p)

    optionalEof :: Megaparsec.Parsec Void Text a -> Megaparsec.Parsec Void Text (Maybe a)
    optionalEof pa = Just <$> pa <|> (Nothing <$ Megaparsec.eof)

    optionalBranch :: Megaparsec.Parsec Void Text (Maybe ProjectBranchName)
    optionalBranch = optionalEof branchNameParser

    branchNameParser = Project.projectBranchNameParser False

    brPath :: Megaparsec.Parsec Void Text Path.Absolute
    brPath = do
      offset <- Megaparsec.getOffset
      path' >>= \case
        Path.AbsolutePath' _ -> failureAt offset "Branch qualified paths don't require a leading '.'"
        -- Branch relative paths are written as relative paths, but are always absolute to the branch root
        Path.RelativePath' (Path.Relative x) -> pure $ Path.Absolute x
    path' = Megaparsec.try do
      offset <- Megaparsec.getOffset
      pathStr <- Megaparsec.takeRest
      case Path.parsePath' (Text.unpack pathStr) of
        Left err -> failureAt offset err
        Right x -> pure x

    failureAt :: forall a. Int -> Text -> Megaparsec.Parsec Void Text a
    failureAt offset str = Megaparsec.parseError (Megaparsec.FancyError offset (Set.singleton (Megaparsec.ErrorFail (Text.unpack str))))

    parseThese ::
      forall a b.
      Megaparsec.Parsec Void Text a ->
      Megaparsec.Parsec Void Text b ->
      Megaparsec.Parsec Void Text (These (Int, a) (Int, b))
    parseThese pa pb = do
      ea <- Megaparsec.observing $ Megaparsec.lookAhead $ Megaparsec.try $ first Text.length <$> Megaparsec.match pa
      eb <- Megaparsec.observing $ Megaparsec.lookAhead $ Megaparsec.try $ first Text.length <$> Megaparsec.match pb
      case (ea, eb) of
        (Left aerr, Left berr) ->
          Megaparsec.parseError (aerr <> berr)
        (Left _, Right (blen, b)) -> do
          Megaparsec.takeP Nothing blen
          pure (That (blen, b))
        (Right (alen, a), Left _) -> do
          Megaparsec.takeP Nothing alen
          pure (This (alen, a))
        (Right a, Right b) -> pure (These a b)

branchRelativePathParser :: Megaparsec.Parsec Void Text BranchRelativePath
branchRelativePathParser =
  incrementalBranchRelativePathParser >>= \case
    ProjectOrPath' _txt path -> pure (UnqualifiedPath path)
    OnlyPath' path -> pure (UnqualifiedPath path)
    IncompleteProject _proj -> fail "Branch relative paths require a branch. Expected `/` here."
    IncompleteBranch _mproj _mbranch -> fail "Branch relative paths require a colon. Expected `:` here."
    PathRelativeToCurrentBranch p -> pure (UnqualifiedPath (Path.AbsolutePath' p))
    IncompletePath projStuff mpath -> case projStuff of
      Left (ProjectAndBranch projName branchName) ->
        pure $ QualifiedBranchPath projName branchName (fromMaybe Path.root mpath)
      Right branch ->
        pure $ BranchPathInCurrentProject branch (fromMaybe Path.root mpath)
