
```ucm:hide
scratch/main> builtins.merge
```

This tests a variable related bug in the ANF compiler.

The nested let would get flattened out, resulting in:

    bar = result

which would be handled by renaming. However, the _context_ portion of
the rest of the code was not being renamed correctly, so `bar` would
remain in the definition of `baz`.

```unison
foo _ =
  id x = x
  void x = ()
  bar = let
    void (Debug.watch "hello" "hello")
    result = 5
    void (Debug.watch "goodbye" "goodbye")
    result
  baz = id bar
  baz

> !foo
```

```ucm
scratch/main> add
```

