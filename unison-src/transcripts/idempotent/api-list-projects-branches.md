# List Projects And Branches Test

I create projects and branches in reverse alphabetical order, and starting with `z`
to place them after `main` alphabetically.
This is because the results from the listing endpoints is sorted by (timestamp, name); but
the default sqlite timestamp only has second-level precision and the transcript will sometimes
lump many of those together. Doing it this way ensures both the creation timestamp and name sort
the same direction so we don't end up with flaky non-deterministic tests.

``` ucm :hide
scratch/main> project.create-empty project-cherry

scratch/main> project.create-empty project-banana

scratch/main> project.create-empty project-apple

project-apple/main> branch z-branch-cherry

project-apple/main> branch z-branch-banana

project-apple/main> branch z-branch-apple
```

``` api
-- Should list all projects
GET /api/projects
  [
      {
          "activeBranchRef": "z-branch-apple",
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
          "branchName": "main"
      },
      {
          "branchName": "z-branch-apple"
      },
      {
          "branchName": "z-branch-banana"
      },
      {
          "branchName": "z-branch-cherry"
      }
  ]
-- Can query for some  infix of the project name
GET /api/projects/project-apple/branches?query=bana
  [
      {
          "branchName": "z-branch-banana"
      }
  ]
```
