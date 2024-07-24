### Transcript parser operations

The transcript parser is meant to parse `ucm` and `unison` blocks.

``` unison
x = 1
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    x : Nat

```
``` unison
---
title: :scratch.u
---
z

```

``` ucm
scratch/main> delete foo

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    foo

```
``` ucm
scratch/main> delete lineToken.call

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    lineToken.call

```
However handling of blocks of other languages should be supported.

``` python
some python code
```

``` c_cpp
some C++ code
```

``` c9search
some cloud9 code
```

