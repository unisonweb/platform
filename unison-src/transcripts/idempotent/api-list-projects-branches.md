# List Projects And Branches Test

``` ucm :hide
scratch/main> project.create-empty project-apple

scratch/main> project.create-empty project-banana

scratch/main> project.create-empty project-cherry

project-apple/main> branch branch-apple

project-apple/main> branch branch-banana

project-apple/main> branch branch-cherry
```

``` api
-- Should list all projects
GET /api/projects
  [
      {
          "activeBranchRef": "branch-cherry",
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
          "branchName": "branch-apple"
      },
      {
          "branchName": "branch-banana"
      },
      {
          "branchName": "branch-cherry"
      },
      {
          "branchName": "main"
      }
  ]
-- Can query for some  infix of the project name
GET /api/projects/project-apple/branches?query=bana
  [
      {
          "branchName": "branch-banana"
      }
  ]
```
