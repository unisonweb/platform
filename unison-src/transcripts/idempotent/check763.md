Regression test for https://github.com/unisonweb/unison/issues/763

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
(+-+) : Nat -> Nat -> Nat
(+-+) x y = x * y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      +-+ : Nat -> Nat -> Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    +-+ : Nat -> Nat -> Nat

scratch/main> move.term +-+ boppitybeep

  Done.

scratch/main> move.term boppitybeep +-+

  Done.
```
