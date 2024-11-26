# View commands

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
a.thing = "a"
b.thing = "b"
```

``` ucm :hide
scratch/main> add
```

``` ucm
-- Should suffix-search and find values in sub-namespaces

scratch/main> view thing

  a.thing : Text
  a.thing = "a"

  b.thing : Text
  b.thing = "b"

-- Should support absolute paths

scratch/main> view .b.thing

  .b.thing : Text
  .b.thing = "b"
```

TODO: swap this back to a 'ucm' block when view.global is re-implemented

``` 
-- view.global should search globally and be absolutely qualified
scratch/other> view.global thing
-- Should support branch relative paths
scratch/other> view /main:a.thing
```
