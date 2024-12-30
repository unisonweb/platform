``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
unique type Baz = Qux Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Baz
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Baz
    type Foo
```

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view Foo

  type Foo = Bar Nat Nat

scratch/main> view Baz

  type Baz = Qux Foo

scratch/main> find.verbose

  1. -- #1uosg6rv85ql7rbohtfvqqacgjl5pp2faj0t3k3dkrtn0t3jqdh2m2om8earv0jh8m8j86vv6bv1h17jl8a2lfa857pm6n27hnisi1g
     type Baz
     
  2. -- #1uosg6rv85ql7rbohtfvqqacgjl5pp2faj0t3k3dkrtn0t3jqdh2m2om8earv0jh8m8j86vv6bv1h17jl8a2lfa857pm6n27hnisi1g#0
     Baz.Qux : Foo -> Baz
     
  3. -- #hlhjq1lf1cvfevkvb9d441kkubn0f6s43gvrd4gcff0r739vomehjnov4b3qe8506fb5bm8m5ba0sol9mbljgkk3gb2qt2u02v6i2vo
     type Foo
     
  4. -- #hlhjq1lf1cvfevkvb9d441kkubn0f6s43gvrd4gcff0r739vomehjnov4b3qe8506fb5bm8m5ba0sol9mbljgkk3gb2qt2u02v6i2vo#0
     Foo.Bar : Nat -> Nat -> Foo
     
```
