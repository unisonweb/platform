# List Projects And Branches Test

I create projects and branches in reverse alphabetical order, and starting with `z`
to place them after `main` alphabetically.
This is because the results from the listing endpoints is sorted by (timestamp, name); but
the default sqlite timestamp only has second-level precision and the transcript will sometimes
lump many of those together. Doing it this way ensures both the creation timestamp and name sort
the same direction so we don't end up with flaky non-deterministic tests.

``` ucm :hide
project-apple/main> project.create-empty project-cherry

project-apple/main> project.create-empty project-banana

project-apple/main> branch a-branch-cherry

project-apple/a-branch-cherry> branch a-branch-banana

project-apple/a-branch-banana> branch a-branch-apple
```

``` api
-- Should list all projects
GET /api/projects
  [
      {
          "activeBranchRef": "a-branch-apple",
          "projectName": "project-apple"
      },
      {
          "activeBranchRef": "main",
          "projectName": "project-banana"
      },
      {
          "activeBranchRef": "main",
          "projectName": "project-cherry"
      },
      {
          "activeBranchRef": "main",
          "projectName": "scratch"
      }
  ]
-- Can query for some infix of the project name
GET /api/projects?query=bana
  [
      {
          "activeBranchRef": "main",
          "projectName": "project-banana"
      }
  ]
-- Should list all branches
GET /api/projects/project-apple/branches
  [
      {
          "branchName": "a-branch-apple"
      },
      {
          "branchName": "a-branch-banana"
      },
      {
          "branchName": "a-branch-cherry"
      },
      {
          "branchName": "main"
      }
  ]
-- Can query for some  infix of the project name
GET /api/projects/project-apple/branches?query=bana
  [
      {
          "branchName": "a-branch-banana"
      }
  ]
```
