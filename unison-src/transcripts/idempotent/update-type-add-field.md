``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

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

  Done.

scratch/main> view Foo

  type Foo = Bar Nat Nat

scratch/main> find.verbose

  1. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g
     type Foo
     
  2. -- #8fk6k0j208th1ia4vnjtoc5fomd6le540prec255svg71bcfga9dofrvoq1d7v6010d6b6em4q51p8st5c5juhrev72cnnel8ko3o1g#0
     Foo.Bar : Nat -> Nat -> Foo
     
```
