# Propagating type edits

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

We introduce a type `Foo` with a function dependent `fooToInt`.

``` unison
unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      fooToInt : Foo -> Int
```

And then we add it.

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    fooToInt : Foo -> Int

scratch/main> find.verbose

  1. -- #j743idicb1sf7udts85812agaml4rkfi3iss6lstvmvgufibd40blq5qtmoh9ndrtkvkaqkurn7npgc61ob8j2louj04j8slkppsl90
     type Foo
     
  2. -- #j743idicb1sf7udts85812agaml4rkfi3iss6lstvmvgufibd40blq5qtmoh9ndrtkvkaqkurn7npgc61ob8j2louj04j8slkppsl90#0
     Foo.Foo : Foo
     
  3. -- #sd7apvqbpk3vl2aassq4gcckovohqrs05ne1g9ol0fb6gd227bp388osj7bg40kttt2o9f1kit9avlb94ep8q1ho3g284ursrplb4l0
     fooToInt : Foo -> Int
     

scratch/main> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42
```

Then if we change the type `Foo`...

``` unison
unique type Foo = Foo | Bar
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

and update the codebase to use the new type `Foo`...

``` ucm
scratch/main> update.old

  ⍟ I've updated these names to your new definition:

    type Foo
```

... it should automatically propagate the type to `fooToInt`.

``` ucm
scratch/main> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42
```

### Preserving user type variables

We make a term that has a dependency on another term and also a non-redundant
user-provided type signature.

``` unison
preserve.someTerm : Optional foo -> Optional foo
preserve.someTerm x = x

preserve.otherTerm : Optional baz -> Optional baz
preserve.otherTerm y = someTerm y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      preserve.otherTerm : Optional baz -> Optional baz
      preserve.someTerm  : Optional foo -> Optional foo
```

Add that to the codebase:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    preserve.otherTerm : Optional baz -> Optional baz
    preserve.someTerm  : Optional foo -> Optional foo
```

Let's now edit the dependency:

``` unison
preserve.someTerm : Optional x -> Optional x
preserve.someTerm _ = None
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      preserve.someTerm : Optional x -> Optional x
```

Update...

``` ucm
scratch/main> update.old

  ⍟ I've updated these names to your new definition:

    preserve.someTerm : Optional x -> Optional x
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

``` ucm
scratch/main> view preserve.someTerm

  preserve.someTerm : Optional x -> Optional x
  preserve.someTerm _ = None

scratch/main> view preserve.otherTerm

  preserve.otherTerm : Optional baz -> Optional baz
  preserve.otherTerm y = someTerm y
```
