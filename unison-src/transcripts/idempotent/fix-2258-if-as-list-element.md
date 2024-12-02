Tests that `if` statements can appear as list and tuple elements.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
> [ if true then 1 else 0 ]

> [ if true then 1 else 0, 1]

> [1, if true then 1 else 0]

> (if true then 1 else 0, 0)

> (0, if true then 1 else 0)

> (1)

> (1,2)

> (1,2,3)

> [1,2,3]

> []

> [1]

> [1,2]

> [1,2,3]

> [
  1,
  2,
  3
  ]

> [
  1,
  2,
  3,]

> (1,2,3,)

> (1,
   2,)

structural ability Zoot where zoot : ()

Zoot.handler : Request {Zoot} a -> a
Zoot.handler = cases
  { a } -> a
  { zoot -> k } -> handle !k with Zoot.handler

fst = cases (x,_) -> x

> List.size
    [ if true then (x y -> y)
      else handle (x y -> x) with fst (Zoot.handler, 42),
      cases a, b -> a Nat.+ b, -- multi-arg cases lambda
      cases x, y -> x Nat.+ y
    ]
```
