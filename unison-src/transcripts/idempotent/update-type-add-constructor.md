``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo
  = Bar Nat
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
unique type Foo
  = Bar Nat
  | Baz Nat Nat
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

  type Foo = Bar Nat | Baz Nat Nat

scratch/main> find.verbose

  1. -- #2sffq4apsq1cts53njcunj63fa8ohov4eqn77q14s77ajicajh4g28sq5s5ai33f2k6oh6o67aarnlpu7u7s4la07ag2er33epalsog
     type Foo
     
  2. -- #2sffq4apsq1cts53njcunj63fa8ohov4eqn77q14s77ajicajh4g28sq5s5ai33f2k6oh6o67aarnlpu7u7s4la07ag2er33epalsog#0
     Foo.Bar : Nat -> Foo
     
  3. -- #2sffq4apsq1cts53njcunj63fa8ohov4eqn77q14s77ajicajh4g28sq5s5ai33f2k6oh6o67aarnlpu7u7s4la07ag2er33epalsog#1
     Foo.Baz : Nat -> Nat -> Foo
     
```
