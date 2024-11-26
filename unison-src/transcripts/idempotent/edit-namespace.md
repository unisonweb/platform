``` ucm :hide
project/main> builtins.mergeio lib.builtin
```

``` unison
{{ ping doc }}
nested.cycle.ping n = n Nat.+ pong n

{{ pong doc }}
nested.cycle.pong n = n Nat.+ ping n

toplevel = "hi"

simple.x = 10
simple.y = 20

-- Shouldn't edit things in lib
lib.project.ignoreMe = 30

-- Shouldn't render record accessors
unique type Foo = { bar : Nat, baz : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      Foo.bar               : Foo -> Nat
      Foo.bar.modify        : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set           : Nat -> Foo -> Foo
      Foo.baz               : Foo -> Nat
      Foo.baz.modify        : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.baz.set           : Nat -> Foo -> Foo
      lib.project.ignoreMe  : Nat
      nested.cycle.ping     : Nat -> Nat
      nested.cycle.ping.doc : Doc2
      nested.cycle.pong     : Nat -> Nat
      nested.cycle.pong.doc : Doc2
      simple.x              : Nat
      simple.y              : Nat
      toplevel              : Text
```

``` ucm
project/main> add

  ⍟ I've added these definitions:

    type Foo
    Foo.bar               : Foo -> Nat
    Foo.bar.modify        : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set           : Nat -> Foo -> Foo
    Foo.baz               : Foo -> Nat
    Foo.baz.modify        : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.baz.set           : Nat -> Foo -> Foo
    lib.project.ignoreMe  : Nat
    nested.cycle.ping     : Nat -> Nat
    nested.cycle.ping.doc : Doc2
    nested.cycle.pong     : Nat -> Nat
    nested.cycle.pong.doc : Doc2
    simple.x              : Nat
    simple.y              : Nat
    toplevel              : Text
```

`edit.namespace` edits the whole namespace (minus the top-level `lib`).

``` ucm
project/main> edit.namespace

  ☝️

  I added 8 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
type Foo = { bar : Nat, baz : Nat }

nested.cycle.ping : Nat -> Nat
nested.cycle.ping n =
  use Nat +
  n + nested.cycle.pong n

nested.cycle.ping.doc : Doc2
nested.cycle.ping.doc = {{ ping doc }}

nested.cycle.pong : Nat -> Nat
nested.cycle.pong n =
  use Nat +
  n + nested.cycle.ping n

nested.cycle.pong.doc : Doc2
nested.cycle.pong.doc = {{ pong doc }}

simple.x : Nat
simple.x = 10

simple.y : Nat
simple.y = 20

toplevel : Text
toplevel = "hi"
```

`edit.namespace` can also accept explicit paths

``` ucm
project/main> edit.namespace nested simple

  ☝️

  I added 6 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
nested.cycle.ping : Nat -> Nat
nested.cycle.ping n =
  use Nat +
  n + nested.cycle.pong n

nested.cycle.ping.doc : Doc2
nested.cycle.ping.doc = {{ ping doc }}

nested.cycle.pong : Nat -> Nat
nested.cycle.pong n =
  use Nat +
  n + nested.cycle.ping n

nested.cycle.pong.doc : Doc2
nested.cycle.pong.doc = {{ pong doc }}

simple.x : Nat
simple.x = 10

simple.y : Nat
simple.y = 20
```
