# `edit.dependents`

The `edit.dependents` command is like `edit`, but it adds a definition and all of its transitive dependents to the file
(being careful not to add anything that's already there).

``` unison
type Foo = Foo Nat Nat
type Bar = { bar : Foo }

baz : Bar -> Bar
baz x = x
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Bar
      type Foo
      Bar.bar        : Bar -> Foo
      Bar.bar.modify : (Foo ->{g} Foo) -> Bar ->{g} Bar
      Bar.bar.set    : Foo -> Bar -> Bar
      baz            : Bar -> Bar

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Bar
    type Foo
    Bar.bar        : Bar -> Foo
    Bar.bar.modify : (Foo ->{g} Foo) -> Bar ->{g} Bar
    Bar.bar.set    : Foo -> Bar -> Bar
    baz            : Bar -> Bar

```
Let's populate our scratch file with `Bar` (and its auto-generated accessors), then `edit.dependents` its dependency
`Foo`, which should add `Foo` and `baz`.

``` unison
type Bar = { bar : Nat }
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Bar
      Bar.bar        : Bar -> Nat
      Bar.bar.modify : (Nat ->{g} Nat) -> Bar ->{g} Bar
      Bar.bar.set    : Nat -> Bar -> Bar

```
``` ucm
scratch/main> edit.dependents Foo

  Loading branch...

  Identifying dependents...

  Loading dependents...

  ☝️
  
  I added 2 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
``` unison :added-by-ucm scratch.u
type Foo = Foo Nat Nat

baz : Bar -> Bar
baz x = x
```

