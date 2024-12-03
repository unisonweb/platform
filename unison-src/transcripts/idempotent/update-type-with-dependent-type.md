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

  1. -- #34msh9satlfog576493eo9pkjn6aj7d8fj6jfheglvgr5s39iptb81649bpkad1lqraheqb8em9ms551k01oternhknc4m7jicgtk08
     type Baz
     
  2. -- #34msh9satlfog576493eo9pkjn6aj7d8fj6jfheglvgr5s39iptb81649bpkad1lqraheqb8em9ms551k01oternhknc4m7jicgtk08#0
     Baz.Qux : Foo -> Baz
     
  3. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g
     type Foo
     
  4. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g#0
     Foo.Bar : Nat -> Nat -> Foo
     
```
