# Regression test for updates which conflict with an existing data constructor

https://github.com/unisonweb/unison/issues/2786

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

First we add a sum-type to the codebase.

``` unison
structural type X = x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type X
        (also named lib.builtins.Unit)
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type X
      (also named lib.builtins.Unit)
```

Now we update the type, changing the name of the constructors, *but*, we simultaneously
add a new top-level term with the same name as the old constructor.

``` unison
structural type X = y | z

X.x : Text
X.x = "some text that's not in the codebase"

dependsOnX = Text.size X.x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      X.x        : Text
      dependsOnX : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type X
        (The old definition is also named lib.builtins.Unit.)
```

This update should succeed since the conflicted constructor
is removed in the same update that the new term is being added.

``` ucm
scratch/main> update.old

  ⍟ I've added these definitions:

    X.x        : Text
    dependsOnX : Nat

  ⍟ I've updated these names to your new definition:

    structural type X
      (The old definition was also named lib.builtins.Unit.)
```
