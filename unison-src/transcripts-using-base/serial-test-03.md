``` unison
structural ability DC r where
  shift : ((a -> r) -> r) -> a

structural type Delayed r = Done r | Delay (Nat -> Delayed r)

hreset : Request {DC r} r -> r
hreset = cases
  { shift e -> k } ->
    e (x -> handle k x with hreset)
  { x } -> x

reset : '{DC r} r -> r
reset body = handle !body with hreset

suspSum l =
  walk acc = cases
    [] -> acc
    x +: xs
      | x == 0 -> walk (shift (k -> Delay (x -> k (acc + x)))) xs
      | otherwise -> walk (acc + x) xs

  reset '(Done (walk 0 l))

l1 = [1, 2, 0, 3, 4, 0, 5, 6, 0, 7, 8]
l2 = [2, 0, 3, 0, 1, 0, 5, 0, 4, 0, 6]
l3 = [1, 2, 3, 4, 5, 6, 7, 8, 9]

feed m = cases
  Done r -> r
  Delay f -> feed m (f m)

finish : (Delayed Nat, Delayed Nat, Delayed Nat) -> Text
finish = cases (x, y, z) ->
  px = feed 2 x
  py = feed 2 y
  pz = feed 2 z

  "(" ++ toText px ++ ", " ++ toText py ++ ", \"" ++ toText pz ++ "\")"

mkTestCase = do
  trip = (suspSum l1, suspSum l2, suspSum l3)
  saveTestCase "case-03" "v4" finish trip
```

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```
