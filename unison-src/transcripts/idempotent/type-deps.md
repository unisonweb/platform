# Ensure type dependencies are properly considered in slurping

https://github.com/unisonweb/unison/pull/2821

``` ucm :hide
scratch/main> builtins.merge
```

Define a type.

``` unison :hide
structural type Y = Y
```

``` ucm :hide
scratch/main> add
```

Now, we update `Y`, and add a new type `Z` which depends on it.

``` unison
structural type Z = Z Y
structural type Y = Y Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Z
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Y
        (The old definition is also named builtin.Unit.)
```

Adding should fail for BOTH definitions, `Y` needs an update and `Z` is blocked by `Y`.

``` ucm :error
scratch/main> add

  x These definitions failed:

    Reason
    needs update structural type Y
    blocked      structural type Z

    Tip: Use `help filestatus` to learn more.

-- This shouldn't exist, because it should've been blocked.

scratch/main> view Z

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    Z
```
