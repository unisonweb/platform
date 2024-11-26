``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison :hide
foo = 1
lib.foo = 2
lib.bar = 3
cat.foo = 4
cat.lib.foo = 5
cat.lib.bar = 6
somewhere.bar = 7
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> find foo

  1. cat.foo : Nat
  2. foo : Nat

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4

scratch/main> find.all foo

  1. cat.foo : Nat
  2. cat.lib.foo : Nat
  3. lib.foo : Nat
  4. foo : Nat

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4
```

``` ucm
scratch/main> find-in cat foo

  1. foo : Nat

scratch/main> view 1

  cat.foo : Nat
  cat.foo = 4

scratch/main> find-in.all cat foo

  1. lib.foo : Nat
  2. foo : Nat

scratch/main> view 1

  cat.lib.foo : Nat
  cat.lib.foo = 5
```

Finding within a namespace

``` ucm
scratch/main> find bar

  1. somewhere.bar : Nat

scratch/other> debug.find.global bar

  Found results in scratch/main

  1. .cat.lib.bar : Nat
  2. .lib.bar : Nat
  3. .somewhere.bar : Nat

scratch/main> find-in somewhere bar

  1. bar : Nat
```

``` ucm :error
scratch/main> find baz

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  😶

  No results. Check your spelling, or try using tab completion
  to supply command arguments.

  `debug.find.global` can be used to search outside the current
  namespace.
```
