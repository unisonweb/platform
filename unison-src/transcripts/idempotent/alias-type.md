`alias.type` makes a new name for a type.

``` ucm :hide
project/main> builtins.mergeio lib.builtins
```

``` ucm
project/main> alias.type lib.builtins.Nat Foo

  Done.

project/main> ls

  1. Foo  (builtin type)
  2. lib/ (643 terms, 92 types)
```

It won't create a conflicted name, though.

``` ucm :error
project/main> alias.type lib.builtins.Int Foo

  ⚠️

  A type by that name already exists.
```

``` ucm
project/main> ls

  1. Foo  (builtin type)
  2. lib/ (643 terms, 92 types)
```

You can use `debug.alias.type.force` for that.

``` ucm
project/main> debug.alias.type.force lib.builtins.Int Foo

  Done.

project/main> ls

  1. Foo  (builtin type)
  2. Foo  (builtin type)
  3. lib/ (643 terms, 92 types)
```
