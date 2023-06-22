The `switch` command switches to an existing project or branch.

Setup stuff.

```unison
someterm = 18
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      someterm : Nat

```
```ucm
.> project.create foo

  🎉 I've created the project foo.

.> project.create bar

  🎉 I've created the project bar.

foo/main> add

  ⍟ I've added these definitions:
  
    someterm : Nat

foo/main> branch bar

  Done. I've created the bar branch based off of main.
  
  Tip: Use `merge /bar /main` to merge your work back into the
       main branch.

foo/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
Now, the demo. When unambiguous, `switch` switches to either a project or a branch in the current project. A branch in
the current project can be preceded by a forward slash (which makes it unambiguous). A project can be followed by a
forward slash (which makes it unambiguous).

```ucm
.> switch foo

.> switch foo/topic

foo/main> switch topic

foo/main> switch /topic

foo/main> switch bar/

```
It's an error to try to switch to something ambiguous.

```ucm
foo/main> switch bar

  I'm not sure if you wanted to switch to the branch foo/bar or
  the project bar. Could you be more specific?
  
  1. /bar (the branch bar in the current project)
  2. bar/ (the project bar, with the branch left unspecified)
  
  Tip: use `switch 1` or `switch 2` to pick one of these.

```
It's an error to try to switch to something that doesn't exist, of course.

```ucm
.> switch foo/no-such-branch

  foo/no-such-branch does not exist.

```
```ucm
.> switch no-such-project

  no-such-project does not exist.

```
```ucm
foo/main> switch no-such-project-or-branch

  Neither project no-such-project-or-branch nor branch
  /no-such-project-or-branch exists.

```
