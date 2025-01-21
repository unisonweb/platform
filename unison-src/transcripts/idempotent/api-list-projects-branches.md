# List Projects And Branches Test

``` ucm :hide
scratch/main> project.create-empty project-one

scratch/main> project.create-empty project-two

scratch/main> project.create-empty project-three

project-one/main> branch branch-one

project-one/main> branch branch-two

project-one/main> branch branch-three
```

``` api
-- Should list all projects
GET /api/projects
  [
      {
          "activeBranchRef": "branch-three",
          "projectName": "project-one"
      },
      {
          "activeBranchRef": "main",
          "projectName": "project-three"
      },
      {
          "activeBranchRef": "main",
          "projectName": "project-two"
      },
      {
          "activeBranchRef": "main",
          "projectName": "scratch"
      }
  ]
-- Can query for some infix of the project name
GET /api/projects?query=thre
  [
      {
          "activeBranchRef": "main",
          "projectName": "project-three"
      }
  ]
-- Should list all branches
GET /api/projects/project-one/branches
  [
      {
          "branchName": "branch-one"
      },
      {
          "branchName": "branch-three"
      },
      {
          "branchName": "branch-two"
      },
      {
          "branchName": "main"
      }
  ]
-- Should list all branches beginning with branch-t
GET /api/projects/project-one/branches?prefix=branch-t
  [
      {
          "branchName": "branch-three"
      },
      {
          "branchName": "branch-two"
      }
  ]
```
