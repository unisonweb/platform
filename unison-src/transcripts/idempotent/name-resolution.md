# Example 1

We have a namespace type named `Namespace.Foo` and a file type named `File.Foo`. A reference to the type `Foo` is
ambiguous. A reference to `Namespace.Foo` or `File.Foo` work fine.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
type Namespace.Foo = Bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Namespace.Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Namespace.Foo
```

``` unison :error
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.


    ❓
    
    I couldn't resolve any of these symbols:
    
        2 | type UsesFoo = UsesFoo Foo
    
    
    Symbol   Suggestions
             
    Foo      File.Foo
             Namespace.Foo
```

``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Namespace.Foo File.Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type File.Foo
      type UsesFoo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 2

We have a namespace type named `Foo` and a file type named `File.Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the namespace type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
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
```

``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type File.Foo
      type UsesFoo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type File.Foo
    type UsesFoo

scratch/main> view UsesFoo

  type UsesFoo = UsesFoo Foo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 3

We have a namespace type named `Namespace.Foo` and a file type named `Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the file type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
type Namespace.Foo = Bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Namespace.Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Namespace.Foo
```

``` unison
type Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      type UsesFoo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    type UsesFoo

scratch/main> view UsesFoo

  type UsesFoo = UsesFoo Foo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Text`. A reference to the term `foo` is ambiguous,
but resolves to `ns.foo` via TDNR.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ns.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ns.foo : Nat
```

``` unison
file.foo : Text
file.foo = "foo"

bar : Text
bar = foo ++ "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar      : Text
      file.foo : Text
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Text`. A reference to the term `foo` is ambiguous,
but resolves to `file.foo` via TDNR.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ns.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ns.foo : Nat
```

``` unison
file.foo : Text
file.foo = "foo"

bar : Nat
bar = foo + 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar      : Nat
      file.foo : Text
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Nat`. A reference to the term `foo` is ambiguous.
A reference to `ns.foo` or `file.foo` work fine.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ns.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ns.foo : Nat
```

``` unison :error
file.foo : Nat
file.foo = 43

bar : Nat
bar = foo + 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what foo refers to here:

      5 | bar = foo + 10

  The name foo is ambiguous. Its type should be: Nat

  I found some terms in scope that have matching names and
  types. Maybe you meant one of these:

  file.foo : Nat
  ns.foo : Nat
```

``` unison
file.foo : Nat
file.foo = 43

bar : Nat
bar = file.foo + ns.foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar      : Nat
      file.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar      : Nat
    file.foo : Nat

scratch/main> view bar

  bar : Nat
  bar =
    use Nat +
    file.foo + ns.foo
```

``` ucm
scratch/main> project.delete scratch
```
