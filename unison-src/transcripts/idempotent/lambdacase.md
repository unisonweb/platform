# Lambda case syntax

``` ucm :hide
scratch/main> builtins.merge
```

This function takes a single argument and immediately pattern matches on it. As we'll see below, it can be written using `cases` syntax:

``` unison
isEmpty x = match x with
  [] -> true
  _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      isEmpty : [t] -> Boolean
```

``` ucm :hide
scratch/main> add
```

Here's the same function written using `cases` syntax:

``` unison
isEmpty2 = cases
  [] -> true
  _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      isEmpty2 : [t] -> Boolean
        (also named isEmpty)
```

Notice that Unison detects this as an alias of `isEmpty`, and if we view `isEmpty`

``` ucm
scratch/main> view isEmpty

  isEmpty : [t] -> Boolean
  isEmpty = cases
    [] -> true
    _  -> false
```

it shows the definition using `cases` syntax opportunistically, even though the code was originally written without that syntax.

## Multi-argument cases

Functions that take multiple arguments and immediately match on a tuple of arguments can also be rewritten to use `cases`. Here's a version using regular `match` syntax on a tuple:

``` unison :hide
merge : [a] -> [a] -> [a]
merge xs ys = match (xs, ys) with
  ([], ys) -> ys
  (xs, []) -> xs
  (h +: t, h2 +: t2) ->
    if h <= h2 then h  +: merge t (h2 +: t2)
    else            h2 +: merge (h +: t) t2
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    merge : [a] -> [a] -> [a]
```

And here's a version using `cases`. The patterns are separated by commas:

``` unison
merge2 : [a] -> [a] -> [a]
merge2 = cases
  [], ys -> ys
  xs, [] -> xs
  h +: t, h2 +: t2 ->
    if h <= h2 then h  +: merge2 t (h2 +: t2)
    else            h2 +: merge2 (h +: t) t2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      merge2 : [a] -> [a] -> [a]
        (also named merge)
```

Notice that Unison detects this as an alias of `merge`, and if we view `merge`

``` ucm
scratch/main> view merge

  merge : [a] -> [a] -> [a]
  merge = cases
    [], ys -> ys
    xs, [] -> xs
    h +: t, h2 +: t2 ->
      if h <= h2 then h +: merge t (h2 +: t2)
      else h2 +: merge (h +: t) t2
```

it again shows the definition using the multi-argument `cases` syntax opportunistically, even though the code was originally written without that syntax.

Here's another example:

``` unison
structural type B = T | F

blah : B -> B -> Text
blah = cases
  T, x -> "hi"
  x, y -> "bye"

blorf = cases
  x, T -> x
  x, y -> y

> blah T F
> blah F F
> blorf T F
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type B
      blah  : B -> B -> Text
      blorf : B -> B -> B

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    12 | > blah T F
           ⧩
           "hi"

    13 | > blah F F
           ⧩
           "bye"

    14 | > blorf T F
           ⧩
           F
```

## Patterns with multiple guards

``` unison
merge3 : [a] -> [a] -> [a]
merge3 = cases
  [], ys -> ys
  xs, [] -> xs
  h +: t, h2 +: t2 | h <= h2   -> h  +: merge3 t (h2 +: t2)
                   | otherwise -> h2 +: merge3 (h +: t) t2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      merge3 : [a] -> [a] -> [a]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    merge3 : [a] -> [a] -> [a]

scratch/main> view merge3

  merge3 : [a] -> [a] -> [a]
  merge3 = cases
    [], ys           -> ys
    xs, []           -> xs
    h +: t, h2 +: t2 
      | h <= h2   -> h +: merge3 t (h2 +: t2)
      | otherwise -> h2 +: merge3 (h +: t) t2
```

This is the same definition written with multiple patterns and not using the `cases` syntax; notice it is considered an alias of `merge3` above.

``` unison
merge4 : [a] -> [a] -> [a]
merge4 a b = match (a,b) with
  [], ys -> ys
  xs, [] -> xs
  h +: t, h2 +: t2 | h <= h2   -> h  +: merge4 t (h2 +: t2)
  h +: t, h2 +: t2 | otherwise -> h2 +: merge4 (h +: t) t2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      merge4 : [a] -> [a] -> [a]
        (also named merge3)
```
