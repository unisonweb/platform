``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

First we make some changes to the codebase so there's data in the reflog.

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      x : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    x : Nat
```

``` unison
y = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      y : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    y : Nat

scratch/main> branch /other

  Done. I've created the other branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /other`.

scratch/other> alias.term y z

  Done.

newproject/main> builtins.merge lib.builtins

  Done.

newproject/main> alias.type lib.builtins.Nat MyNat

  Done.
```

Should see reflog entries from the current branch

``` ucm
scratch/main> reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch         Hash          Description
  1.   scratch/main   #6mdl5gruh5   add
  2.   scratch/main   #3rqf1hbev7   add
  3.   scratch/main   #ms9lggs2rg   builtins.merge scratch/main:lib.builtins
  4.   scratch/main   #sg60bvjo91   Project Created
```

Should see reflog entries from the current project

``` ucm
scratch/main> project.reflog

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch          Hash          Description
  1.   scratch/other   #148flqs4b1   alias.term y scratch/other:z
  2.   scratch/other   #6mdl5gruh5   Branch created from scratch/main
  3.   scratch/main    #6mdl5gruh5   add
  4.   scratch/main    #3rqf1hbev7   add
  5.   scratch/main    #ms9lggs2rg   builtins.merge scratch/main:lib.builtins
  6.   scratch/main    #sg60bvjo91   Project Created
```

Should see reflog entries from all projects

``` ucm
scratch/main> reflog.global

  Below is a record of recent changes, you can use
  `reset #abcdef` to reset the current branch to a previous
  state.

  Tip: Use `diff.namespace 1 7` to compare between points in
       history.

       Branch            Hash          Description
  1.   newproject/main   #2rjhs2vq43   alias.term lib.builtins.Nat newproject/main:MyNat
  2.   newproject/main   #ms9lggs2rg   builtins.merge newproject/main:lib.builtins
  3.   newproject/main   #sg60bvjo91   Branch Created
  4.   scratch/other     #148flqs4b1   alias.term y scratch/other:z
  5.   scratch/other     #6mdl5gruh5   Branch created from scratch/main
  6.   scratch/main      #6mdl5gruh5   add
  7.   scratch/main      #3rqf1hbev7   add
  8.   scratch/main      #ms9lggs2rg   builtins.merge scratch/main:lib.builtins
  9.   scratch/main      #sg60bvjo91   Project Created
```
