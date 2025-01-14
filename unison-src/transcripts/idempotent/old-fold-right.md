``` ucm :hide
scratch/main> builtins.merge
```

``` unison
oldRight: (b ->{e} a ->{e} b) -> [a] ->{e} [b]
oldRight f la = bug "out"

pecan: '{} [Text]
pecan = 'let
  la = [1, 2, 3]
  f: Text -> Nat -> Text
  f = bug "out"

  oldRight f la
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      oldRight : (b ->{e} a ->{e} b) -> [a] ->{e} [b]
      pecan    : '[Text]
```
