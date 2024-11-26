``` ucm
test-ls/main> builtins.merge

  Done.
```

``` unison
foo.bar.add x y = x Int.+ y

foo.bar.subtract x y = x Int.- y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.bar.add      : Int -> Int -> Int
      foo.bar.subtract : Int -> Int -> Int
```

``` ucm
test-ls/main> add

  ⍟ I've added these definitions:

    foo.bar.add      : Int -> Int -> Int
    foo.bar.subtract : Int -> Int -> Int

test-ls/main> ls foo

  1. bar/ (2 terms)

test-ls/main> ls 1

  1. add      (Int -> Int -> Int)
  2. subtract (Int -> Int -> Int)
```
