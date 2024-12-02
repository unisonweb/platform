module Unison.CommandLine.FZFResolvers
  ( FZFResolver (..),
    definitionOptions,
    termDefinitionOptions,
    typeDefinitionOptions,
    namespaceOptions,
    projectDependencyResolver,
    projectNameOptions,
    projectBranchOptions,
    projectBranchOptionsWithinCurrentProject,
    fuzzySelectFromList,
    multiResolver,
    definitionResolver,
    typeDefinitionResolver,
    termDefinitionResolver,
    namespaceResolver,
    namespaceOrDefinitionResolver,
    projectAndOrBranchArg,
    projectOrBranchResolver,
    projectBranchResolver,
    projectBranchWithinCurrentProjectResolver,
    projectNameResolver,
    fuzzySelectHeader,
  )
where

import Control.Lens
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.Project as SqliteProject
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Position qualified as Position
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation qualified as Relation

type OptionFetcher = Codebase IO Symbol Ann -> PP.ProjectPath -> Branch0 IO -> IO [Text]

data FZFResolver = FZFResolver
  { getOptions :: OptionFetcher
  }

instance Show FZFResolver where
  show _ = "<FZFResolver>"

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
genericDefinitionOptions :: Bool -> Bool -> OptionFetcher
genericDefinitionOptions includeTerms includeTypes _codebase _projCtx searchBranch0 = liftIO do
  let termsAndTypes =
        Monoid.whenM includeTerms Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms searchBranch0))
          <> Monoid.whenM includeTypes Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes searchBranch0))
  termsAndTypes
    & Set.toList
    & map (HQ.toText . fmap (Name.setPosition Position.Relative))
    & pure

-- | Select a definition from the given branch.
definitionOptions :: OptionFetcher
definitionOptions = genericDefinitionOptions True True

-- | Select a term definition from the given branch.
-- Returned names will match the provided 'Position' type.
termDefinitionOptions :: OptionFetcher
termDefinitionOptions = genericDefinitionOptions True False

-- | Select a type definition from the given branch.
-- Returned names will match the provided 'Position' type.
typeDefinitionOptions :: OptionFetcher
typeDefinitionOptions = genericDefinitionOptions False True

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
namespaceOptions :: OptionFetcher
namespaceOptions _codebase _projCtx searchBranch0 = do
  let intoPath' :: Path -> Path'
      intoPath' = Path' . Right . Path.Relative
  searchBranch0
    & Branch.deepPaths
    & Set.delete (Path.empty {- The current path just renders as an empty string which isn't a valid arg -})
    & Set.toList
    & map (Path.toText' . intoPath')
    & pure

-- | Lists all dependencies of the current project.
--
-- E.g. if the current project has `lib.base` and `lib.distributed`, it will list:
-- ["base", "distributed"]
projectDependencyOptions :: OptionFetcher
projectDependencyOptions _codebase _projCtx searchBranch0 = do
  searchBranch0
    & Branch.getAt0 (Path.singleton NameSegment.libSegment)
    & Branch.nonEmptyChildren
    & Map.keys
    & fmap NameSegment.toEscapedText
    & pure

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectFromList :: [Text] -> FZFResolver
fuzzySelectFromList options =
  (FZFResolver {getOptions = \_codebase _projCtx _branch -> pure options})

-- | Combine multiple option fetchers into one resolver.
multiResolver :: [OptionFetcher] -> FZFResolver
multiResolver resolvers =
  let getOptions :: Codebase IO Symbol Ann -> PP.ProjectPath -> Branch0 IO -> IO [Text]
      getOptions codebase projCtx searchBranch0 = do
        List.nubOrd <$> foldMapM (\f -> f codebase projCtx searchBranch0) resolvers
   in (FZFResolver {getOptions})

definitionResolver :: FZFResolver
definitionResolver = FZFResolver {getOptions = definitionOptions}

typeDefinitionResolver :: FZFResolver
typeDefinitionResolver = FZFResolver {getOptions = typeDefinitionOptions}

termDefinitionResolver :: FZFResolver
termDefinitionResolver = FZFResolver {getOptions = termDefinitionOptions}

namespaceResolver :: FZFResolver
namespaceResolver = FZFResolver {getOptions = namespaceOptions}

namespaceOrDefinitionResolver :: FZFResolver
namespaceOrDefinitionResolver = multiResolver [definitionOptions, namespaceOptions]

projectDependencyResolver :: FZFResolver
projectDependencyResolver = FZFResolver {getOptions = projectDependencyOptions}

-- | A project name, branch name, or both.
projectAndOrBranchArg :: FZFResolver
projectAndOrBranchArg = multiResolver [projectBranchOptions, projectNameOptions]

projectOrBranchResolver :: FZFResolver
projectOrBranchResolver = multiResolver [projectBranchOptions, namespaceOptions]

projectBranchResolver :: FZFResolver
projectBranchResolver = FZFResolver {getOptions = projectBranchOptions}

projectBranchWithinCurrentProjectResolver :: FZFResolver
projectBranchWithinCurrentProjectResolver = FZFResolver {getOptions = projectBranchOptionsWithinCurrentProject}

projectNameResolver :: FZFResolver
projectNameResolver = FZFResolver {getOptions = projectNameOptions}

-- | All possible local project names
-- E.g. '@unison/base'
projectNameOptions :: OptionFetcher
projectNameOptions codebase _projCtx _searchBranch0 = do
  fmap (into @Text . SqliteProject.name) <$> Codebase.runTransaction codebase Q.loadAllProjects

-- | All possible local project/branch names.
-- E.g. '@unison/base/main'
projectBranchOptions :: OptionFetcher
projectBranchOptions codebase projCtx _searchBranch0 = do
  projs <- Codebase.runTransaction codebase Q.loadAllProjectBranchNamePairs
  projs
    & foldMap
      ( \(names, projIds) ->
          if projIds.project == projCtx.project.projectId
            then -- If the branch is in the current project, put a shortened version of the branch name first,
            -- then the long-form name at the end of the list (in case the user still types the full name)
              [(0 :: Int, "/" <> into @Text names.branch), (2, into @Text names)]
            else [(1, into @Text names)]
      )
    -- Put branches in this project first.
    & List.sort
    & fmap snd
    & pure

-- | All possible local branch names within the current project.
-- E.g. '@unison/base/main'
projectBranchOptionsWithinCurrentProject :: OptionFetcher
projectBranchOptionsWithinCurrentProject codebase projCtx _searchBranch0 = do
  Codebase.runTransaction codebase (Q.loadAllProjectBranchesBeginningWith (projCtx ^. #project . #projectId) Nothing)
    <&> fmap (into @Text . snd)

-- | Exported from here just so the debug command and actual implementation can use the same
-- messaging.
--
-- >>> fuzzySelectHeader "definition to view"
-- "Select a definition to view:"
--
-- >>> fuzzySelectHeader "alias name"
-- "Select an alias name:"
fuzzySelectHeader :: Text -> Text
fuzzySelectHeader argDesc = "Select " <> aOrAn argDesc <> " " <> argDesc <> ":"
  where
    aOrAn :: Text -> Text
    aOrAn txt =
      Text.uncons txt & \case
        Just (c, _) | c `elem` ("aeiou" :: [Char]) -> "an"
        _ -> "a"
