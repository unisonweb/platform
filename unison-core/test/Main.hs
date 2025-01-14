{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Text qualified as Text
import Data.These (These (..))
import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Core.Project
import Unison.Prelude
import Unison.Project (ProjectAndBranchNames (..))

main :: IO ()
main =
  withCP65001 $
    (run . tests)
      [ scope "Project" (tests projectTests)
      ]

projectTests :: [Test ()]
projectTests =
  [ scope "project name" do
      let t s expected =
            scope (Text.unpack s) $
              expectEqual (UnsafeProjectName expected) =<< expectRight' (tryFrom @Text @ProjectName s)
      t "project" "project"
      t "project/" "project"
      t "@user/project" "@user/project"
      t "@user/project/" "@user/project",
    scope "project branch name" do
      let t s expected =
            scope (Text.unpack s) $
              expectEqual (UnsafeProjectBranchName expected) =<< expectRight' (tryFrom @Text @ProjectBranchName s)
      t "branch" "branch"
      t "/branch" "branch"
      t "@user/branch" "@user/branch"
      t "/@user/branch" "@user/branch"
      t "releases/1.2.3" "releases/1.2.3"
      t "/releases/1.2.3" "releases/1.2.3"
      t "releases/drafts/1.2.3" "releases/drafts/1.2.3"
      t "/releases/drafts/1.2.3" "releases/drafts/1.2.3",
    scope "project name, project branch name, or both" do
      let t input expected =
            scope (Text.unpack input) $
              expectEqual expected =<< expectRight' (tryFrom @Text @(These ProjectName ProjectBranchName) input)
      t "project" (This "project")
      t "project/" (This "project")
      t "@user/project" (This "@user/project")
      t "@user/project/" (This "@user/project")
      t "/branch" (That "branch")
      t "/@user/branch" (That "@user/branch")
      t "/releases/1.2.3" (That "releases/1.2.3")
      t "/releases/drafts/1.2.3" (That "releases/drafts/1.2.3")
      t "project/branch" (These "project" "branch")
      t "project/@user/branch" (These "project" "@user/branch")
      t "project/releases/1.2.3" (These "project" "releases/1.2.3")
      t "project/releases/drafts/1.2.3" (These "project" "releases/drafts/1.2.3")
      t "@user/project/branch" (These "@user/project" "branch")
      t "@user/project/@user/branch" (These "@user/project" "@user/branch")
      t "@user/project/releases/1.2.3" (These "@user/project" "releases/1.2.3")
      t "@user/project/releases/drafts/1.2.3" (These "@user/project" "releases/drafts/1.2.3"),
    scope "project name, project branch name, or both, take 2" do
      let t input expected =
            scope (Text.unpack input) $
              expectEqual expected =<< expectRight' (tryFrom @Text @ProjectAndBranchNames input)
      t "project" (ProjectAndBranchNames'Ambiguous "project" "project")
      t "project/" (ProjectAndBranchNames'Unambiguous (This "project"))
      t "@user/project" (ProjectAndBranchNames'Ambiguous "@user/project" "@user/project")
      t "@user/project/" (ProjectAndBranchNames'Unambiguous (This "@user/project"))
      t "/branch" (ProjectAndBranchNames'Unambiguous (That "branch"))
      t "/@user/branch" (ProjectAndBranchNames'Unambiguous (That "@user/branch"))
      t "releases/1.2.3" (ProjectAndBranchNames'Unambiguous (That "releases/1.2.3"))
      t "/releases/1.2.3" (ProjectAndBranchNames'Unambiguous (That "releases/1.2.3"))
      t "releases/drafts/1.2.3" (ProjectAndBranchNames'Unambiguous (That "releases/drafts/1.2.3"))
      t "/releases/drafts/1.2.3" (ProjectAndBranchNames'Unambiguous (That "releases/drafts/1.2.3"))
      t "project/branch" (ProjectAndBranchNames'Unambiguous (These "project" "branch"))
      t "project/@user/branch" (ProjectAndBranchNames'Unambiguous (These "project" "@user/branch"))
      t "project/releases/1.2.3" (ProjectAndBranchNames'Unambiguous (These "project" "releases/1.2.3"))
      t "project/releases/drafts/1.2.3" (ProjectAndBranchNames'Unambiguous (These "project" "releases/drafts/1.2.3"))
      t "@user/project/branch" (ProjectAndBranchNames'Unambiguous (These "@user/project" "branch"))
      t "@user/project/@user/branch" (ProjectAndBranchNames'Unambiguous (These "@user/project" "@user/branch"))
      t "@user/project/releases/1.2.3" (ProjectAndBranchNames'Unambiguous (These "@user/project" "releases/1.2.3"))
      t "@user/project/releases/drafts/1.2.3" (ProjectAndBranchNames'Unambiguous (These "@user/project" "releases/drafts/1.2.3")),
    scope "project name with optional project branch name" do
      let t input project branch =
            scope (Text.unpack input) $
              expectEqual (ProjectAndBranch project branch)
                =<< expectRight' (tryFrom @Text @(ProjectAndBranch ProjectName (Maybe ProjectBranchName)) input)
      t "project" "project" Nothing
      t "project/" "project" Nothing
      t "project/branch" "project" (Just "branch")
      t "project/@user/branch" "project" (Just "@user/branch")
      t "project/releases/1.2.3" "project" (Just "releases/1.2.3")
      t "project/releases/drafts/1.2.3" "project" (Just "releases/drafts/1.2.3")
      t "@user/project" "@user/project" Nothing
      t "@user/project/" "@user/project" Nothing
      t "@user/project/branch" "@user/project" (Just "branch")
      t "@user/project/@user/branch" "@user/project" (Just "@user/branch")
      t "@user/project/releases/1.2.3" "@user/project" (Just "releases/1.2.3")
      t "@user/project/releases/drafts/1.2.3" "@user/project" (Just "releases/drafts/1.2.3"),
    scope "project branch name with optional project name" do
      let t input project branch =
            scope (Text.unpack input) $
              expectEqual (ProjectAndBranch project branch)
                =<< expectRight' (tryFrom @Text @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) input)
      t "branch" Nothing "branch"
      t "@user/branch" Nothing "@user/branch"
      t "releases/1.2.3" Nothing "releases/1.2.3"
      t "releases/drafts/1.2.3" Nothing "releases/drafts/1.2.3"
      t "/branch" Nothing "branch"
      t "/@user/branch" Nothing "@user/branch"
      t "/releases/1.2.3" Nothing "releases/1.2.3"
      t "/releases/drafts/1.2.3" Nothing "releases/drafts/1.2.3"
      t "project/branch" (Just "project") "branch"
      t "project/@user/branch" (Just "project") "@user/branch"
      t "project/releases/1.2.3" (Just "project") "releases/1.2.3"
      t "project/releases/drafts/1.2.3" (Just "project") "releases/drafts/1.2.3"
      t "@user/project/branch" (Just "@user/project") "branch"
      t "@user/project/@user/branch" (Just "@user/project") "@user/branch"
      t "@user/project/releases/1.2.3" (Just "@user/project") "releases/1.2.3"
      t "@user/project/releases/drafts/1.2.3" (Just "@user/project") "releases/drafts/1.2.3"
  ]

-- Evil instances for convenience/readability
instance IsString ProjectName where fromString = UnsafeProjectName . fromString

instance IsString ProjectBranchName where fromString = UnsafeProjectBranchName . fromString
