# delete.project

``` ucm
scratch/main> project.create-empty foo

  🎉 I've created the project foo.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.

  Write your first Unison code with UCM:

    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.

  🎉 🥳 Happy coding!

scratch/main> project.create-empty bar

  🎉 I've created the project bar.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.

  Write your first Unison code with UCM:

    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.

  🎉 🥳 Happy coding!

-- I can delete the project I'm currently on

scratch/main> delete.project scratch

foo/main> projects

  1. bar
  2. foo

-- I can delete a different project

foo/main> delete.project bar

foo/main> projects

  1. foo

-- I can delete the last project, a new scratch project will be created

foo/main> delete.project foo

project/main> projects

  1. project
  2. scratch

-- If the last project is scratch, a scratch2 project will be created.

scratch/main> delete.project project

scratch/main> delete.project scratch

project/main> projects

  1. project
  2. scratch2
```
