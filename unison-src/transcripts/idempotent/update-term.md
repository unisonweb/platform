``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo : Nat
```

``` unison
foo : Nat
foo = 6
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

scratch/main> view foo

  foo : Nat
  foo = 6
```
