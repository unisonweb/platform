# The empty codebase

The Unison codebase, when first initialized, contains no definitions in its namespace.

Not even `Nat` or `+`\!

BEHOLD\!\!\!

``` ucm :error
scratch/main> ls

  nothing to show
```

Technically, the definitions all exist, but they have no names. `builtins.merge` brings them into existence, under the current namespace:

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.

scratch/main> ls lib

  1. builtins/ (469 terms, 74 types)
```

And for a limited time, you can get even more builtin goodies:

``` ucm
scratch/main> builtins.mergeio lib.builtinsio

  Done.

scratch/main> ls lib

  1. builtins/   (469 terms, 74 types)
  2. builtinsio/ (643 terms, 92 types)
```

More typically, you'd start out by pulling `base`.
