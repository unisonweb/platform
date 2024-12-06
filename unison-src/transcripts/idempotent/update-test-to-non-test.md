``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
test> foo = []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> foo = []
    
```

After adding the test `foo`, we expect `view` to render it like a test. (Bug: It doesn't.)

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo : [Result]

scratch/main> view foo

  foo : [Result]
  foo = []
```

``` unison
foo = 1
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

After updating `foo` to not be a test, we expect `view` to not render it like a test.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo

  foo : Nat
  foo = 1
```
