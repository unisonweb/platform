Regression test for https://github.com/unisonweb/unison/pull/2819

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
hangExample : Boolean
hangExample =
  ("a long piece of text to hang the line" == "")
    && ("a long piece of text to hang the line" == "")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      hangExample : Boolean
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    hangExample : Boolean

scratch/main> view hangExample

  hangExample : Boolean
  hangExample =
    "a long piece of text to hang the line" == ""
      && "a long piece of text to hang the line" == ""
```
