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

``` ucm :added-by-ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Tree a
      evaluate   : (Tree Nat -> Nat)
                   -> (Tree Text -> Text)
                   -> (Tree Nat, Tree Nat, Tree Nat, Tree Text)
                   -> Text
      foldMap    : r
                   -> (r ->{g2} r ->{g1} r)
                   -> (a ->{g} r)
                   -> Tree a
                   ->{g2, g1, g} r
      mkTestCase : '{IO, Exception} ()
      tree0      : Tree Nat
      tree1      : Tree Nat
      tree2      : Tree Nat
      tree3      : Tree Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Tree a
    evaluate   : (Tree Nat -> Nat)
                 -> (Tree Text -> Text)
                 -> (Tree Nat, Tree Nat, Tree Nat, Tree Text)
                 -> Text
    foldMap    : r
                 -> (r ->{g2} r ->{g1} r)
                 -> (a ->{g} r)
                 -> Tree a
                 ->{g2, g1, g} r
    mkTestCase : '{IO, Exception} ()
    tree0      : Tree Nat
    tree1      : Tree Nat
    tree2      : Tree Nat
    tree3      : Tree Text
scratch/main> run mkTestCase

  ()
```
