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

id x = x
unitCase = id (x -> 1)

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

id x = x
unitCase = id (x -> (1, ()))
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

Regression test for weird behavior w/r to unit and parens.

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=unitCase&newTerm=unitCase
```


Diff types

``` api
GET /api/projects/diffs/diff/types?oldBranchRef=main&newBranchRef=new&oldType=Type&newType=Type
```
