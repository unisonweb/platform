Deleting the branch you are on takes you to its parent (though this is impossible to see in a transcript, since we set
your working directory with each command).

```ucm
.> project.create foo

  🎉 I've created the project foo.

foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

foo/topic> delete.branch /topic

```
A branch need not be preceded by a forward slash.

```ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

foo/topic> delete.branch topic

```
You can precede the branch name by a project name.

```ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

  ☝️  The namespace . is empty.

.> delete.branch foo/topic

```
You can delete the only branch in a project.

```ucm
foo/main> delete.branch /main

```
