``` unison
bonk : forall a. a -> a
bonk x =
  zonk : forall a. a -> a
  zonk z = z
  honk : a
  honk = x
  x
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bonk : a -> a

```
