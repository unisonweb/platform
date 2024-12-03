``` ucm :hide
scratch/main> builtins.merge
```

This tests a variable related bug in the ANF compiler.

The nested let would get flattened out, resulting in:

``` 
bar = result
```

which would be handled by renaming. However, the *context* portion of
the rest of the code was not being renamed correctly, so `bar` would
remain in the definition of `baz`.

``` unison
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

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : ∀ _. _ -> Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    12 | > !foo
           ⧩
           5
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo : ∀ _. _ -> Nat
```
