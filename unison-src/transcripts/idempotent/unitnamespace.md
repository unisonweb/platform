``` unison
`()`.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      `()`.foo : ##Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    `()`.foo : ##Text

scratch/main> find

  1. `()`.foo : ##Text

scratch/main> find-in `()`

  1. foo : ##Text

scratch/main> delete.namespace `()`

  Done.
```
