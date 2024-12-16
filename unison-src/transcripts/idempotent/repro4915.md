Suggestion:

Since it doesn't work anyway, we should disable adding or updating definitions under lib.

``` ucm :hide
scratch/main> alias.type ##Nat lib.builtin.Nat
```

``` unison
lib.foo.bar : Nat
lib.foo.bar = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.foo.bar : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    lib.foo.bar : Nat
```

Arguments against:
We probably use this feature currently to set up transcripts, etc. A workaround could cbe setting up the mock libs in a separate branch and then using the "use branch as lib" mechanism.  (Currently `fork` iirc, and eventually something else when we have first-class-ier dependencies.)
