# The `text.find` command

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

The `text.find` (or `grep`) command can be used to search for text or numeric literals appearing anywhere in your project. Just supply one or more tokens to search for. Unlike regular grep over the text of your code, this ignores local variables and function names that happen to match your search tokens (use `dependents` or `find` for that purpose). It's only searching for text or numeric literals that match.

``` ucm
scratch/main> help grep

  text.find (or grep)
  `text.find token1 "99" token2` finds terms with literals (text
  or numeric) containing `token1`, `99`, and `token2`.

  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.

  Use `text.find.all` to include search of `lib`.
```

``` ucm
scratch/main> help text.find.all

  text.find.all (or grep.all)
  `text.find.all token1 "99" token2` finds terms with literals
  (text or numeric) containing `token1`, `99`, and `token2`.

  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.

  Use `text.find` to exclude `lib` from search.
```

Here's an example:

``` unison
foo =
  _ = "an interesting constant"
  1
bar = match "well hi there" with
  "ooga" -> 99
  "booga" -> 23
  _ -> 0
baz = ["an", "quaffle", "tres"]
qux =
  quaffle = 99
  quaffle + 1

lib.foo = [Any 46, Any "hi", Any "zoink"]
lib.bar = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar     : Nat
      baz     : [Text]
      foo     : Nat
      lib.bar : Nat
      lib.foo : [Any]
      qux     : Nat
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> grep hi

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0

scratch/main> grep "hi"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> text.find.all hi

  🔎

  These definitions from the current namespace have matches:

    1. bar
    2. lib.foo

  Tip: Try `edit 1` or `edit 1-2` to bring these into your
       scratch file.

scratch/main> view "1-5"

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0

  lib.foo : [Any]
  lib.foo = [Any 46, Any "hi", Any "zoink"]

scratch/main> grep oog

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0
```

``` ucm
scratch/main> grep quaffle

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. baz

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> view "1-5"

  baz : [Text]
  baz = ["an", "quaffle", "tres"]

scratch/main> text.find "interesting const"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. foo

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> view "1-5"

  foo : Nat
  foo =
    _ = "an interesting constant"
    1

scratch/main> text.find "99" "23"

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. bar

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> view 1

  bar : Nat
  bar = match "well hi there" with
    "ooga"  -> 99
    "booga" -> 23
    _       -> 0
```

Now some failed searches:

``` ucm :error
scratch/main> grep lsdkfjlskdjfsd

  😶 I couldn't find any matches.

  Tip: `text.find.all` will search `lib` as well.
```

Notice it gives the tip about `text.find.all`. But not here:

``` ucm :error
scratch/main> grep.all lsdkfjlskdjfsd

  😶 I couldn't find any matches.
```
