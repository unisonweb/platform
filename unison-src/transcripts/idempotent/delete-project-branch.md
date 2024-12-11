Deleting the branch you are on takes you to its parent (though this is impossible to see in a transcript, since we set
your working directory with each command).

``` ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

foo/topic> delete.branch "/topic"
```

A branch need not be preceded by a forward slash.

``` ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

foo/topic> delete.branch topic
```

You can precede the branch name by a project name.

``` ucm
foo/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

scratch/main> delete.branch "foo/topic"
```

You can delete the only branch in a project.

``` ucm
foo/main> delete.branch "/main"
```

You can delete the last branch in the project, a new one will be created.

``` ucm
scratch/main> delete.branch "scratch/main"

scratch/main> branches

       Branch   Remote branch
  1.   main     
  2.   main2    
```

If the the last branch isn't /main, then /main will be created.

``` ucm
scratch/main2> delete.branch "/main"

scratch/main2> delete.branch "/main2"

scratch/other> branches

       Branch   Remote branch
  1.   main     
  2.   other    
```
