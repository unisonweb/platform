``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

structural type A.B = OneAlias Foo
structural type A = B.TheOtherAlias Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type A
      structural type A.B
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type A
    structural type A.B
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

``` ucm :error
scratch/main> update

  The type A.B is an alias of A. I'm not able to perform an
  update when a type exists nested under an alias of itself.
  Please separate them or delete one copy, and then try updating
  again.
```
