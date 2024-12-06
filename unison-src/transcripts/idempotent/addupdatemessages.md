# Adds and updates

Let's set up some definitions to start:

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
x = 1
y = 2

structural type X = One Nat
structural type Y = Two Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type X
      structural type Y
      x : Nat
      y : Nat
```

Expected: `x` and `y`, `X`, and `Y` exist as above. UCM tells you this.

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type X
    structural type Y
    x : Nat
    y : Nat
```

Let's add an alias for `1` and `One`:

``` unison
z = 1

structural type Z = One Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Z
        (also named X)
      z : Nat
        (also named x)
```

Expected: `z` is now `1`. UCM tells you that this definition is also called `x`.
Also, `Z` is an alias for `X`.

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Z
      (also named X)
    z : Nat
      (also named x)
```

Let's update something that has an alias (to a value that doesn't have a name already):

``` unison
x = 3
structural type X = Three Nat Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type X
        (The old definition is also named Z.)
      x : Nat
        (The old definition is also named z.)
```

Expected: `x` is now `3` and `X` has constructor `Three`. UCM tells you the old definitions were also called `z` and `Z` and these names have also been updated.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

Update it to something that already exists with a different name:

``` unison
x = 2
structural type X = Two Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type X
        (also named Y)
      x : Nat
        (also named y)
```

Expected: `x` is now `2` and `X` is `Two`. UCM says the old definition was also named `z/Z`, and was also updated. And it says the new definition is also named `y/Y`.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
