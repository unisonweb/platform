``` ucm :hide
scratch/main> builtins.merge
```

Some tests of pattern behavior.

``` unison
p1 = join [literal "blue", literal "frog"]

> Pattern.run (many p1) "bluefrogbluegoat"
> Pattern.run (many.corrected p1) "bluefrogbluegoat"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      p1 : Pattern Text

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    3 | > Pattern.run (many p1) "bluefrogbluegoat"
          ⧩
          Some ([], "goat")

    4 | > Pattern.run (many.corrected p1) "bluefrogbluegoat"
          ⧩
          Some ([], "bluegoat")
```
