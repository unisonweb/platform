A short script to test mutable references with local scope.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
test = Scope.run 'let
  r = Scope.ref 0
  Ref.write r 1
  i = Ref.read r
  Ref.write r 2
  j = Ref.read r
  Ref.write r 5
  (i, j, Ref.read r)

> test
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      test : (Nat, Nat, Nat)

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    10 | > test
           ⧩
           (1, 2, 5)
```
