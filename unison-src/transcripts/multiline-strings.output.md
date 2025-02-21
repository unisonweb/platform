``` unison
string =
  """
  a
  b

  c
  """
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      string : ##Text

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    string : ##Text

scratch/main> edit string

  ☝️
  
  I added 1 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
``` unison:added-by-ucm scratch.u
string : ##Text
string =
  """
  a
  b
  
  c
  """
```

