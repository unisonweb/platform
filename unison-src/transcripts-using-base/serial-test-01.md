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

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```
