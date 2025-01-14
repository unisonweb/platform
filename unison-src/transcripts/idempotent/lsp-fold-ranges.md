``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide

{{ Type doc }}
structural type Optional a =
  None
  | Some a

{{
  Multi line

  Term doc
}}
List.map :
  (a -> b)
  -> [a]
  -> [b]
List.map f = cases
  (x +: xs) -> f x +: List.map f xs
  [] -> []

test> z = let
  x = "hello"
  y = "world"
  [Ok (x ++ y)]
```

``` ucm
scratch/main> debug.lsp.fold-ranges


  《{{ Type doc }}》
  《structural type Optional a =
    None
    | Some a》

  《{{
    Multi line

    Term doc
  }}》
  《List.map :
    (a -> b)
    -> [a]
    -> [b]
  List.map f = cases
    (x +: xs) -> f x +: List.map f xs
    [] -> []》

  《test> z = let
    x = "hello"
    y = "world"
    [Ok (x ++ y)]》
```
