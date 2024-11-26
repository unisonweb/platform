``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 17
lib.new.foo = +18
thingy = lib.old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.new.foo : Int
      lib.old.foo : Nat
      thingy      : Nat
```

``` ucm
proj/main> add

  ⍟ I've added these definitions:

    lib.new.foo : Int
    lib.old.foo : Nat
    thingy      : Nat
```

``` ucm :error
proj/main> upgrade old new

  I couldn't automatically upgrade old to new. However, I've
  added the definitions that need attention to the top of
  scratch.u.

  When you're done, you can run

    upgrade.commit

  to merge your changes back into main and delete the temporary
  branch. Or, if you decide to cancel the upgrade instead, you
  can run

    delete.branch /upgrade-old-to-new

  to delete the temporary branch and switch back to main.
```

``` unison :added-by-ucm scratch.u
thingy : Nat
thingy =
  use Nat +
  foo + 10
```

Resolve the error and commit the upgrade.

``` unison
thingy = foo + +10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      thingy : Int
```

``` ucm
proj/upgrade-old-to-new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

proj/upgrade-old-to-new> upgrade.commit

  I fast-forward merged proj/upgrade-old-to-new into proj/main.

proj/main> view thingy

  thingy : Int
  thingy =
    use Int +
    foo + +10

proj/main> ls lib

  1. builtin/ (469 terms, 74 types)
  2. new/     (1 term)

proj/main> branches

       Branch   Remote branch
  1.   main     
```
