`alias.term` makes a new name for a term.

``` ucm :hide
project/main> builtins.mergeio lib.builtins
```

``` ucm
project/main> alias.term lib.builtins.bug foo

  Done.

project/main> ls

  1. foo  (a -> b)
  2. lib/ (643 terms, 92 types)
```

It won't create a conflicted name, though.

``` ucm :error
project/main> alias.term lib.builtins.todo foo

  ⚠️

  A term by that name already exists.
```

``` ucm
project/main> ls

  1. foo  (a -> b)
  2. lib/ (643 terms, 92 types)
```

You can use `debug.alias.term.force` for that.

``` ucm
project/main> debug.alias.term.force lib.builtins.todo foo

  Done.

project/main> ls

  1. foo  (a -> b)
  2. foo  (a -> b)
  3. lib/ (643 terms, 92 types)
```
