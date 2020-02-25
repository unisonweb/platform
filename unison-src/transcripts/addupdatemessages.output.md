# Adds and updates

Let's set up some definitions to start:

```unison
x = 1
y = 2

type X = One Nat
type Y = Two Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type X
      type Y
      x : Nat
      y : Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Expected: `x` and `y`, `X`, and `Y` exist as above. UCM tells you this.

```ucm
  ☝️  The namespace .scratch is empty.

.scratch> add

  ⍟ I've added these definitions:
  
    type X
    type Y
    x : Nat
    y : Nat

```
Let's add an alias for `1` and `One`:

```unison
z = 1

type Z = One Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Z
        (also named X)
      z : Nat
        (also named x)
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Expected: `z` is now `1`. UCM tells you that this definition is also called `x`.
Also, `Z` is an alias for `X`.

```ucm
.scratch> add

  ⍟ I've added these definitions:
  
    type Z
      (also named X)
    z : Nat
      (also named x)

```
Let's update something that has an alias (to a value that doesn't have a name already):

```unison
x = 3
type X = Three Nat Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type X
        (The old definition is also named Z. I'll update this
        name too.)
      x : Nat
        (The old definition is also named z. I'll update this
        name too.)
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Expected: `x` is now `3` and `X` has constructor `Three`. UCM tells you the old definitions were also called `z` and `Z` and these names have also been updated.

```ucm
.scratch> update

  ⍟ I've updated these names to your new definition:
  
    type X
      (The old definition was also named Z. I updated this name
      too.)
    x : .builtin.Nat
      (The old definition was also named z. I updated this name
      too.)

```
Update it to something that already exists with a different name:

```unison
x = 2
type X = Two Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type X
        (The old definition is also named Z. I'll update this
        name too.)
        (The new definition is already named Y as well.)
      x : Nat
        (The old definition is also named z. I'll update this
        name too.)
        (The new definition is already named y as well.)
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
Expected: `x` is now `2` and `X` is `Two`. UCM says the old definition was also named `z/Z`, and was also updated. And it says the new definition is also named `y/Y`. 

```ucm
.scratch> update

  ⍟ I've updated these names to your new definition:
  
    type X
      (The old definition was also named Z. I updated this name
      too.)
      (The new definition is already named Y as well.)
    x : .builtin.Nat
      (The old definition was also named z. I updated this name
      too.)
      (The new definition is already named y as well.)

```
