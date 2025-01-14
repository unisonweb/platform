``` unison
structural ability Exit a where
  exit : a -> b

prod : [Nat] -> Nat
prod l =
  loop acc = cases
    [] -> acc
    0 +: _ -> exit 0
    n +: ns -> loop (acc * n) ns

  handle loop 0 l with cases
    { exit v -> _ } -> v
    { p } -> p

l1 = [1,3,5,7,9]
l2 = [1,2,0,3,4,5,6,7,8]
l3 = [1,2,4,8,16,3,5,6]

products = cases (x, y, z) ->
  px = prod x
  py = prod y
  pz = prod z

  "(" ++ toText px ++ ", " ++ toText py ++ ", \"" ++ toText pz ++ "\")"

mkTestCase = do
  saveTestCase "case-02" "v4" products (l1, l2, l3)

```

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```
