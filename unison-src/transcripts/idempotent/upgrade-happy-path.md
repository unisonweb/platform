``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 17
lib.new.foo = 18
thingy = lib.old.foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.new.foo : Nat
      lib.old.foo : Nat
      thingy      : Nat
```

``` ucm
proj/main> add

  ⍟ I've added these definitions:

    lib.new.foo : Nat
    lib.old.foo : Nat
    thingy      : Nat
```

Test tab completion and fzf options of upgrade command.

``` ucm
proj/main> debug.tab-complete upgrade ol

   old

proj/main> debug.fuzzy-options upgrade _

  Select a dependency to upgrade:
    * builtin
    * new
    * old

proj/main> debug.fuzzy-options upgrade old _

  Select a dependency to upgrade to:
    * builtin
    * new
    * old
```

``` ucm
proj/main> upgrade old new

  I upgraded old to new, and removed old.

proj/main> ls lib

  1. builtin/ (469 terms, 74 types)
  2. new/     (1 term)

proj/main> view thingy

  thingy : Nat
  thingy =
    use Nat +
    foo + 10
```
