# projects merge

```ucm
.> builtins.merge

  Done.

```
```unison
zonk = 0
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      zonk : Nat

```
```ucm
  ☝️  The namespace .foo is empty.

.foo> add

  ⍟ I've added these definitions:
  
    zonk : Nat

.> project.create foo

  🎉 I've created the project foo.

.> merge foo foo/main

  Here's what's changed in foo/main after the merge:
  
  Added definitions:
  
    1. zonk : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

```
```unison
bonk = 2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bonk : Nat

```
```ucm
foo/main> add

  ⍟ I've added these definitions:
  
    bonk : Nat

```
```ucm
.> project.create bar

  🎉 I've created the project bar.

bar/main> merge foo/main

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. bonk : Nat
    2. zonk : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

bar/main> branch /topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
xonk = 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      xonk : Nat

```
```ucm
bar/main> add

  ⍟ I've added these definitions:
  
    xonk : Nat

bar/topic> merge /main

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. xonk : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

  ☝️  The namespace .bar is empty.

.bar> merge foo/main

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. bonk : Nat
    2. zonk : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

```
