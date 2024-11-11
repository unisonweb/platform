``` unison
l1 = [1.0,2.0,3.0]
l2 = [+1,+2,+3]
l3 = [?a, ?b, ?c]

combines = cases
  (x, y, z) ->
    rx = foldLeft (+) 0.0 x
    ry = foldLeft (+) +0 y
    rz = foldLeft (t c -> t ++ Char.toText c) "" z

    "(" ++ toText rx ++ ", " ++ toText ry ++ ", \"" ++ rz ++ "\")"

mkTestCase = do
  saveTestCase "case-01" "v4" combines (l1, l2, l3)
```

``` ucm :added-by-ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      combines   : ([Float], [Int], [Char]) -> Text
      l1         : [Float]
      l2         : [Int]
      l3         : [Char]
      mkTestCase : '{IO, Exception} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    combines   : ([Float], [Int], [Char]) -> Text
    l1         : [Float]
    l2         : [Int]
    l3         : [Char]
    mkTestCase : '{IO, Exception} ()
scratch/main> run mkTestCase

  ()
```
