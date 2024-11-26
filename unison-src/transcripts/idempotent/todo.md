# Nothing to do

When there's nothing to do, `todo` says this:

``` ucm
scratch/main> todo

  You have no pending todo items. Good work! ✅
```

# Dependents of `todo`

The `todo` command shows local (outside `lib`) terms that directly call `todo`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo : Nat
foo = todo "implement foo"

bar : Nat
bar = foo + foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar : Nat
    foo : Nat

scratch/main> todo

  These terms call `todo`:

    1. foo
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Direct dependencies without names

The `todo` command shows hashes of direct dependencies of local (outside `lib`) definitions that don't have names in
the current namespace.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo.bar = 15
baz = foo.bar + foo.bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      baz     : Nat
      foo.bar : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    baz     : Nat
    foo.bar : Nat

scratch/main> delete.namespace.force foo

  Done.

  ⚠️

  Of the things I deleted, the following are still used in the
  following definitions. They now contain un-named references.

  Dependency   Referenced In
  bar          1. baz

scratch/main> todo

  These terms do not have any names in the current namespace:

    1. #1jujb8oelv
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Conflicted names

The `todo` command shows conflicted names.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
foo = 16
bar = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar : Nat
    foo : Nat

scratch/main> debug.alias.term.force foo bar

  Done.

scratch/main> todo

  ❓

  The term bar has conflicting definitions:

    1. bar#14ibahkll6
    2. bar#cq22mm4sca

  Tip: Use `move.term` or `delete.term` to resolve the
       conflicts.
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Definitions in lib

The `todo` command complains about terms and types directly in `lib`.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
lib.foo = 16
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    lib.foo : Nat

scratch/main> todo

  There's a type or term at the top level of the `lib`
  namespace, where I only expect to find subnamespaces
  representing library dependencies. Please move or remove it.
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Constructor aliases

The `todo` command complains about constructor aliases.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = One
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

scratch/main> alias.term Foo.One Foo.Two

  Done.

scratch/main> todo

  The type Foo has a constructor with multiple names.

    1. Foo.One
    2. Foo.Two

  Please delete all but one name for each constructor.
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Missing constructor names

The `todo` command complains about missing constructor names.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Bar
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

scratch/main> delete.term Foo.Bar

  Done.

scratch/main> todo

  These types have some constructors with missing names.

    1. Foo

  You can use `view 1` and
  `alias.term <hash> <TypeName>.<ConstructorName>` to give names
  to each unnamed constructor.
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Nested decl aliases

The `todo` command complains about nested decl aliases.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
structural type Foo a = One a | Two a a
structural type Foo.inner.Bar a = Uno a | Dos a a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Foo a
      structural type Foo.inner.Bar a
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo a
    structural type Foo.inner.Bar a

scratch/main> todo

  These types are aliases, but one is nested under the other.
  Please separate them or delete one copy.

    1. Foo
    2. Foo.inner.Bar
```

``` ucm :hide
scratch/main> delete.project scratch
```

# Stray constructors

The `todo` command complains about stray constructors.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Bar
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

scratch/main> alias.term Foo.Bar Baz

  Done.

scratch/main> todo

  These constructors are not nested beneath their corresponding
  type names:

    1. Baz

  For each one, please either use `move` to move if, or if it's
  an extra copy, you can simply `delete` it.
```

``` ucm :hide
scratch/main> delete.project scratch
```
