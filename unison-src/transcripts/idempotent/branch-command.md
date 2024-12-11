The `branch` command creates a new branch.

``` ucm :hide
scratch/main> project.create-empty foo

scratch/main> project.create-empty bar
```

First, we'll create a term to include in the branches.

``` unison :hide
someterm = 18
```

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.

scratch/main> add

  ⍟ I've added these definitions:

    someterm : Nat
```

Now, the `branch` demo:

`branch` can create a branch from a different branch in the same project, from a different branch in a different
project. It can also create an empty branch.

``` ucm
foo/main> branch topic1

  Done. I've created the topic1 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic1`.

foo/main> branch "/topic2"

  Done. I've created the topic2 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic2`.

foo/main> branch "foo/topic3"

  Done. I've created the topic3 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic3`.

foo/main> branch main topic4

  Done. I've created the topic4 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic4`.

foo/main> branch main "/topic5"

  Done. I've created the topic5 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic5`.

foo/main> branch main "foo/topic6"

  Done. I've created the topic6 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic6`.

foo/main> branch "/main" topic7

  Done. I've created the topic7 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic7`.

foo/main> branch "/main" "/topic8"

  Done. I've created the topic8 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic8`.

foo/main> branch "/main" "foo/topic9"

  Done. I've created the topic9 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic9`.

foo/main> branch "foo/main" topic10

  Done. I've created the topic10 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic10`.

foo/main> branch "foo/main" "/topic11"

  Done. I've created the topic11 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic11`.

scratch/main> branch "foo/main" "foo/topic12"

  Done. I've created the topic12 branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic12`.

foo/main> branch "bar/topic"

  Done. I've created the bar/topic branch based off foo/main.

bar/main> branch "foo/main" topic2

  Done. I've created the bar/topic2 branch based off foo/main.

bar/main> branch "foo/main" "/topic3"

  Done. I've created the bar/topic3 branch based off foo/main.

scratch/main> branch "foo/main" "bar/topic4"

  Done. I've created the bar/topic4 branch based off foo/main.

foo/main> branch.empty empty1

  Done. I've created an empty branch foo/empty1.

  Tip: Use `merge /somebranch` to initialize this branch.

foo/main> branch.empty "/empty2"

  Done. I've created an empty branch foo/empty2.

  Tip: Use `merge /somebranch` to initialize this branch.

foo/main> branch.empty "foo/empty3"

  Done. I've created an empty branch foo/empty3.

  Tip: Use `merge /somebranch` to initialize this branch.

scratch/main> branch.empty "foo/empty4"

  Done. I've created an empty branch foo/empty4.

  Tip: Use `merge /somebranch` to initialize this branch.
```

The `branch` command can create branches named `releases/drafts/*` (because why not).

``` ucm
foo/main> branch "releases/drafts/1.2.3"

  Done. I've created the releases/drafts/1.2.3 branch based off
  of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /releases/drafts/1.2.3`.

foo/main> switch "/releases/drafts/1.2.3"
```

The `branch` command can't create branches named `releases/*` nor `releases/drafts/*`.

``` ucm :error
foo/main> branch "releases/1.2.3"

  Branch names like releases/1.2.3 are reserved for releases.

  Tip: to download an existing release, try
       `clone /releases/1.2.3`.

  Tip: to draft a new release, try `release.draft 1.2.3`.

foo/main> switch "/releases/1.2.3"

  foo/releases/1.2.3 does not exist.
```
