``` ucm :hide
scratch/main> builtins.merge lib.builtins
```
``` unison :hide-all
List.adjacentPairs : [a] -> [(a, a)]
List.adjacentPairs as =
  go xs acc =
      match xs with
      [x, y] ++ t -> go t (acc :+ (x, y))
      _ -> acc
  go as []

List.all : (a -> Boolean) -> [a] -> Boolean
List.all p xs =
  match xs with
    [] -> true
    x +: xs -> (p x) && (List.all p xs)

List.any : (a -> Boolean) -> [a] -> Boolean
List.any p xs =
  match xs with
    [] -> false
    x +: xs -> (p x) || (List.any p xs)

List.chunk : Nat -> [a] -> [[a]]
List.chunk n as =
  go acc rest =
    match splitAt n rest with
      (c, []) -> acc :+ c
      (c, cs) -> go (acc :+ c) cs
  go [] as

List.chunksOf : Nat -> [a] -> [[a]]
List.chunksOf n text =
  go acc text =
    p = splitAt n text
    match p with
      ([], _) -> acc
      (a, b) -> go (acc :+ a) b
  go [] text

List.dropWhile : (a -> Boolean) -> [a] -> [a]
List.dropWhile p xs =
  match xs with
    i +: l -> if p i then List.dropWhile p l else xs
    _ -> []

List.first : [a] -> Optional a
List.first a = List.at 0 a

List.init : [a] -> Optional [a]
List.init as =
  match as with
    [] -> None
    as :+ _ -> Some as

List.intersperse : a -> [a] -> [a]
List.intersperse a as =
  go acc as =
    match as with
      [] -> acc
      [x] -> acc :+ x
      x +: xs -> go (acc :+ x :+ a) xs
  go [] as

List.isEmpty : [a] -> Boolean
List.isEmpty as = List.size as == 0

List.last : [a] -> Optional a
List.last as =
  match as with
    [] -> None
    _ :+ a -> Some a

List.replicate : Nat -> a -> [a]
List.replicate n a =
  go n acc = if n == 0 then acc else go (Nat.drop n 1) (a +: acc)
  go n []

List.splitAt : Nat -> [a] -> ([a], [a])
List.splitAt n as = (List.take n as, List.drop n as)

List.tail : [a] -> Optional [a]
List.tail as =
  match as with
    [] -> None
    _ +: as -> Some as

List.takeWhile : (a ->{𝕖} Boolean) -> [a] ->{𝕖} [a]
List.takeWhile p xs =
  go xs acc =
    match xs with
      x +: xs -> if p x then go xs (acc :+ x) else acc
      _ -> acc
  go xs []
```
``` ucm :hide
scratch/main> add
```

The `alias.many` command can be used to copy definitions from the current namespace into your curated one.
The names that will be used in the target namespace are the names you specify, relative to the current namespace:

```
scratch/main> help alias.many

  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates aliases `relative1`, `relative2`, ...
  in the namespace `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases `.quux.foo.foo` and `.quux.bar.bar`.
```

Let's try it!

``` ucm
scratch/main> alias.many List.adjacentPairs List.all List.any List.chunk List.chunksOf List.dropWhile List.first List.init List.intersperse List.isEmpty List.last List.replicate List.splitAt List.tail List.takeWhile mylib
scratch/main> find-in mylib
```

Thanks, `alias.many`!
