``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo
```

Bug: this no-op update should (of course) succeed.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
