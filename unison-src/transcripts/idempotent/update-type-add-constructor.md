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

  type Foo = Baz Nat Nat | Bar Nat

scratch/main> find.verbose

  1. -- #id3p6do8f7ssln9npa3gs3c2i8uors25ffts92pr4nsh9k9bn3no50e4d1b053c2d0vei64pbtcpdld9gk6drsvptnpfqr6tp8v4qh0
     type Foo
     
  2. -- #id3p6do8f7ssln9npa3gs3c2i8uors25ffts92pr4nsh9k9bn3no50e4d1b053c2d0vei64pbtcpdld9gk6drsvptnpfqr6tp8v4qh0#1
     Foo.Bar : Nat -> Foo
     
  3. -- #id3p6do8f7ssln9npa3gs3c2i8uors25ffts92pr4nsh9k9bn3no50e4d1b053c2d0vei64pbtcpdld9gk6drsvptnpfqr6tp8v4qh0#0
     Foo.Baz : Nat -> Nat -> Foo
     
```
