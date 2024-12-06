This transcript demonstrates that unique types no longer always get a fresh GUID: they share GUIDs with already-saved
unique types of the same name.

``` unison
unique type A = A

unique type B = B C
unique type C = C B
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type A
      type B
      type C
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type A
    type B
    type C
```

``` unison
unique type A = A

unique type B = B C
unique type C = C B
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.
```

If the name stays the same, the churn is even prevented if the type is updated and then reverted to the original form.

``` ucm
scratch/main> names A

  Type
  Hash:  #uj8oalgadr
  Names: A

  Term
  Hash:   #uj8oalgadr#0
  Names:  A.A
```

``` unison
unique type A = A ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type A
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names A

  Type
  Hash:  #ufo5tuc7ho
  Names: A

  Term
  Hash:   #ufo5tuc7ho#0
  Names:  A.A
```

``` unison
unique type A = A
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type A
```

Note that `A` is back to its original hash.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names A

  Type
  Hash:  #uj8oalgadr
  Names: A

  Term
  Hash:   #uj8oalgadr#0
  Names:  A.A
```
