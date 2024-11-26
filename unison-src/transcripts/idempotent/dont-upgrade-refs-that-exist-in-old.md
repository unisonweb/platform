If `foo#old` exists in old, and `foo#new` exists in new, you might think `upgrade old new` would rewrite references to
`#old` with references to `#new`. And it will... \!\!unless\!\! `#old` still exists in new.

``` ucm :hide
foo/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 18
lib.new.other = 18
lib.new.foo = 19
mything = lib.old.foo + lib.old.foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.new.foo   : Nat
      lib.new.other : Nat
      lib.old.foo   : Nat
      mything       : Nat
```

``` ucm
foo/main> add

  ⍟ I've added these definitions:

    lib.new.foo   : Nat
    lib.new.other : Nat
    lib.old.foo   : Nat
    mything       : Nat

foo/main> upgrade old new

  I upgraded old to new, and removed old.

foo/main> view mything

  mything : Nat
  mything =
    use Nat +
    other + other
```
