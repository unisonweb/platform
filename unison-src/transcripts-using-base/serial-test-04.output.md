``` unison

mutual0 = cases
  0 -> "okay"
  n ->
    _ = openFile
    mutual1 (drop n 1)

mutual1 n =
  mutual0 n

mkTestCase = do
  saveTestCase "case-04" "v4" mutual1 5
```

``` ucm :added-by-ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      mkTestCase : '{IO, Exception} ()
      mutual0    : Nat -> Text
      mutual1    : Nat -> Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    mkTestCase : '{IO, Exception} ()
    mutual0    : Nat -> Text
    mutual1    : Nat -> Text
scratch/main> run mkTestCase

  ()
```
