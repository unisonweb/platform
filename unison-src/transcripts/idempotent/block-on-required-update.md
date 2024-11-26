# Block on required update

Should block an `add` if it requires an update on an in-file dependency.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      x : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    x : Nat
```

Update `x`, and add a new `y` which depends on the update

``` unison
x = 10
y = x + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      y : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      x : Nat
```

Try to add only the new `y`. This should fail because it requires an update to `x`, but we only ran an 'add'.

``` ucm :error
scratch/main> add y

  x These definitions failed:

    Reason
    needs update   x   : Nat
    blocked        y   : Nat

    Tip: Use `help filestatus` to learn more.
```
