# Update on conflict

Conflicted definitions prevent `update` from succeeding.

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

``` unison
x = 1
temp = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      temp : Nat
      x    : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    temp : Nat
    x    : Nat

scratch/main> debug.alias.term.force temp x

  Done.

scratch/main> delete.term temp

  Done.
```

``` unison
x = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat
```

``` ucm :error
scratch/main> update

  This branch has more than one term with the name `x`. Please
  delete or rename all but one of them, then try the update
  again.
```
