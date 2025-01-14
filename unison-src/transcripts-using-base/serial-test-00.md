``` unison
structural type Tree a = Leaf | Node (Tree a) a (Tree a)

foldMap  : r -> (r -> r -> r) -> (a -> r) -> Tree a -> r
foldMap z m f =
  walk = cases
    Leaf -> z
    Node l x r -> m (walk l) (m (f x) (walk r))

  walk

tree0 : Tree Nat
tree0 =
  (Node
     (Node Leaf 2 Leaf)
     1
     (Node Leaf 3 (Node Leaf 4 Leaf)))

tree1 : Tree Nat
tree1 =
  Node
    tree0
    0
    (Node
       (Node
          (Node Leaf 7 Leaf)
          6
          (Node
             Leaf
             8
             (Node Leaf 9 Leaf)))
       5
       Leaf)

tree2 : Tree Nat
tree2 = Node tree0 10 tree1

tree3 : Tree Text
tree3 =
  Node
    (Node Leaf "hello" Leaf)
    " "
    (Node (Node Leaf "good" Leaf)
          "bye"
          Leaf)

evaluate
  : (Tree Nat ->{} Nat)
  -> (Tree Text ->{} Text)
  -> (Tree Nat, Tree Nat, Tree Nat, Tree Text)
  -> Text
evaluate f g = cases
  (w, x, y, z) ->
    ow = f w
    ox = f x
    oy = f y
    oz = g z
    "(" ++ toText ow ++ ", " ++ toText ox ++ ", " ++ toText oy ++ ", " ++ oz ++ ")"

mkTestCase : '{IO,Exception} ()
mkTestCase = do
  balancedSum = foldMap 0 (Nat.+) (x -> x)
  catenate = foldMap "" (Text.++) (x -> x)
  f = evaluate balancedSum catenate
  tup = (tree0, tree1, tree2, tree3)

  saveTestCase "case-00" "v4" f tup
```

``` ucm
scratch/main> add
scratch/main> run mkTestCase
```
