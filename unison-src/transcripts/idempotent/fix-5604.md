``` ucm
scratch/main> builtins.mergeio lib.builtin

  Done.
```

The situation:

  - topic has two names for a thing (foo and lib.bar)
  - main delete one alias (foo, leaving lib.bar)
  - When merging topic into main, for some reason we render topic's stuff that depends on that thing as referring to
    `foo`, which main deleted\!

``` ucm
scratch/main> alias.type lib.builtin.Nat MyNat

  Done.
```

``` unison
foo : Nat
foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : MyNat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo : MyNat

scratch/main> branch topic

  Done. I've created the topic branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

scratch/topic> switch /main

scratch/main> delete.type MyNat

  Done.
```

``` unison
foo : Nat
foo = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> switch /topic
```

``` unison
bar : MyNat
bar = foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : MyNat
```

``` ucm
scratch/topic> add

  ⍟ I've added these definitions:

    bar : MyNat

scratch/topic> switch /main
```

This merge should succeed, but it renders `bar` using the deleted name `MyNat` (rather than the still-existing name
`Nat`), and then fails with an out-of-scope error.

``` ucm :error
scratch/main> merge /topic

  Loading branches...

  Loading definitions...

  Computing diffs...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  I couldn't automatically merge scratch/topic into
  scratch/main. However, I've added the definitions that need
  attention to the top of scratch.u.

  When you're done, you can run

    merge.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the merge instead, you can
  run

    delete.branch /merge-topic-into-main

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
bar : MyNat
bar =
  use Nat +
  foo + foo

```
