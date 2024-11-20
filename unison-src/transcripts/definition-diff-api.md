``` ucm
diffs/main> builtins.mergeio lib.builtins
diffs/main> alias.term lib.builtins.Nat.gt lib.builtins.Nat.>
diffs/main> alias.term lib.builtins.Nat.drop lib.builtins.Nat.-
```

``` unison
term =
  _ = "Here's some text"
  1 + 1

type Type = Type Nat

ability Stream a where
  emit : a -> ()

take n s =
  use Nat > -
  h n = cases
    { emit a -> k } -> if n > 0
                         then
                           emit a
                           handle k() with h (n - 1)
                         else None
    { r }  -> Some r
  handle s() with h n

fakeRefModify f g = g []

foreach f xs = match xs with
    [] -> ()
    x +: rest -> let
      f x
      foreach f rest

handleRequest =
  use List +:
  finalizers = [1, 2, 3]
  addFinalizer f = fakeRefModify finalizers (fs -> f +: fs)
  foreach (f -> ()) finalizers

```

``` ucm
diffs/main> add
diffs/main> branch.create new
```

``` unison
term =
  _ = "Here's some different text"
  1 + 2

type Type a = Type a Text

ability Stream a where
  emit : a -> ()

take n s =
  use Nat > -
  h n = cases
    { emit a -> k } ->
        emit a
        if n > 0
          then handle k() with h (n - 1)
          else None
    { r }  -> Some r
  if n > 0
    then handle s () with h (n - 1)
    else None

fakeRefModify2 f g = g []

foreach xs f = match xs with
    [] -> ()
    x +: rest -> let
      f x
      foreach rest f

handleRequest =
    use List +:
    finalizers = [1, 2, 3]
    addFinalizer f = fakeRefModify2 finalizers (fs -> (f +: fs, ()))
    foreach finalizers (f -> ())
```

``` ucm
diffs/new> update
```

Diff terms

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=term&newTerm=term
```

More complex diff

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=take&newTerm=take
```

Regression test

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=handleRequest&newTerm=handleRequest
```


Diff types

``` api
GET /api/projects/diffs/diff/types?oldBranchRef=main&newBranchRef=new&oldType=Type&newType=Type
```
