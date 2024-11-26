A `namespace foo` directive is optional, and may only appear at the top of a file.

It affects the contents of the file as follows:

1.  All bindings like `x.y.z` are prefixed with the namespace; note that when this file is saved, the feedback mentions
    the full bindings' names.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
namespace foo

baz : Nat
baz = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.baz : Nat
```

2.  Free variables whose names exactly match bindings in the file are rewritten to refer to the prefixed binder instead.
    That is, a term like `factorial = ... factorial ...` is rewritten to `foo.factorial = ... foo.factorial ...`.

``` unison
namespace foo

factorial : Int -> Int
factorial = cases
    +0 -> +1
    n -> n * factorial (n - +1)

longer.evil.factorial : Int -> Int
longer.evil.factorial n = n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.factorial             : Int -> Int
      foo.longer.evil.factorial : Int -> Int
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo.factorial             : Int -> Int
    foo.longer.evil.factorial : Int -> Int

scratch/main> view factorial

  foo.factorial : Int -> Int
  foo.factorial = cases
    +0 -> +1
    n  -> n Int.* foo.factorial (n Int.- +1)

  foo.longer.evil.factorial : Int -> Int
  foo.longer.evil.factorial n = n
```

Note that in the above example, we do not want the existence of a `namespace foo` directive to determine whether the
reference to the name `factorial` within the body of `factorial` is a recursive reference (good, behavior without
namespace directive, exact-name-match-wins semantics) or an ambiguous reference (bad, as would be the case if the
bindings were expanded to `foo.factorial` and `foo.longer.evil.factorial`, but the variables left alone).

Here are a few more examples demonstrating that type names, constructor names, generated record accessor names, and
type links are all properly handled.

``` unison
type longer.foo.Foo = Bar
type longer.foo.Baz = { qux : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type longer.foo.Baz
      type longer.foo.Foo
      longer.foo.Baz.qux        : Baz -> Nat
      longer.foo.Baz.qux.modify : (Nat ->{g} Nat)
                                  -> Baz
                                  ->{g} Baz
      longer.foo.Baz.qux.set    : Nat -> Baz -> Baz
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type longer.foo.Baz
    type longer.foo.Foo
    longer.foo.Baz.qux        : Baz -> Nat
    longer.foo.Baz.qux.modify : (Nat ->{g} Nat) -> Baz ->{g} Baz
    longer.foo.Baz.qux.set    : Nat -> Baz -> Baz
```

``` unison
namespace foo

type Foo = Bar
type Baz = { qux : Nat }

type RefersToFoo = RefersToFoo Foo

refersToBar = cases
  Foo.Bar -> 17

refersToQux baz =
  Baz.qux baz + Baz.qux baz

hasTypeLink =
  {{ {type Foo} }}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type foo.Baz
      type foo.Foo
      type foo.RefersToFoo
      foo.Baz.qux        : foo.Baz -> Nat
      foo.Baz.qux.modify : (Nat ->{g} Nat)
                           -> foo.Baz
                           ->{g} foo.Baz
      foo.Baz.qux.set    : Nat -> foo.Baz -> foo.Baz
      foo.hasTypeLink    : Doc2
      foo.refersToBar    : foo.Foo -> Nat
      foo.refersToQux    : foo.Baz -> Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type foo.Baz
    type foo.Foo
    type foo.RefersToFoo
    foo.Baz.qux        : foo.Baz -> Nat
    foo.Baz.qux.modify : (Nat ->{g} Nat)
                         -> foo.Baz
                         ->{g} foo.Baz
    foo.Baz.qux.set    : Nat -> foo.Baz -> foo.Baz
    foo.hasTypeLink    : Doc2
    foo.refersToBar    : foo.Foo -> Nat
    foo.refersToQux    : foo.Baz -> Nat

scratch/main> view RefersToFoo refersToBar refersToQux hasTypeLink

  type foo.RefersToFoo = RefersToFoo foo.Foo

  foo.hasTypeLink : Doc2
  foo.hasTypeLink = {{ {type foo.Foo} }}

  foo.refersToBar : foo.Foo -> Nat
  foo.refersToBar = cases foo.Foo.Bar -> 17

  foo.refersToQux : foo.Baz -> Nat
  foo.refersToQux baz =
    use Nat +
    use foo.Baz qux
    qux baz + qux baz

scratch/main> todo

  You have no pending todo items. Good work! ✅
```
