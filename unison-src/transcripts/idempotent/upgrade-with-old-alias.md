``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 141
lib.new.foo = 142
bar = 141
mything = lib.old.foo + 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar         : Nat
      lib.new.foo : Nat
      lib.old.foo : Nat
      mything     : Nat
```

``` ucm
myproject/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

myproject/main> upgrade old new

  I upgraded old to new, and removed old.

myproject/main> view mything

  mything : Nat
  mything =
    use Nat +
    foo + 100

myproject/main> view bar

  bar : Nat
  bar = 141
```
