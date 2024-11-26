``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 25
lib.new.foo = +30
a.x.x.x.x = 100
b.x.x.x.x = 100
c.y.y.y.y = lib.old.foo + 10
d.y.y.y.y = lib.old.foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a.x.x.x.x   : Nat
      b.x.x.x.x   : Nat
      bar         : Nat
      c.y.y.y.y   : Nat
      d.y.y.y.y   : Nat
      lib.new.foo : Int
      lib.old.foo : Nat
```

``` ucm
myproject/main> add

  ⍟ I've added these definitions:

    a.x.x.x.x   : Nat
    b.x.x.x.x   : Nat
    bar         : Nat
    c.y.y.y.y   : Nat
    d.y.y.y.y   : Nat
    lib.new.foo : Int
    lib.old.foo : Nat
```

``` ucm :error
myproject/main> upgrade old new

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
bar : Nat
bar =
  use Nat +
  x + c.y.y.y.y

c.y.y.y.y : Nat
c.y.y.y.y =
  use Nat +
  foo + 10

d.y.y.y.y : Nat
d.y.y.y.y =
  use Nat +
  foo + 10
```
