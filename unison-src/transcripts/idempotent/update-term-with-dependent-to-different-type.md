``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
foo : Nat
foo = 5

bar : Nat
bar = foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar : Nat
    foo : Nat
```

``` unison
foo : Int
foo = +5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int
```

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.
```

``` unison :added-by-ucm scratch.u
foo : Int
foo = +5

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

bar : Nat
bar =
  use Nat +
  foo + 10

```
