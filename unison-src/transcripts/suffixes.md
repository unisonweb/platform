# Suffix-based resolution of names

```ucm:hide
scratch/main> builtins.merge
```

Any unique name suffix can be used to refer to a definition. For instance:

```unison:hide
-- No imports needed even though FQN is `builtin.{Int,Nat}`
foo.bar.a : Int
foo.bar.a = +99

-- No imports needed even though FQN is `builtin.Optional.{None,Some}`
optional.isNone = cases
  None -> true
  Some _ -> false
```

This also affects commands like find. Notice lack of qualified names in output:

```ucm
scratch/main> add
scratch/main> find take
```

The `view` and `display` commands also benefit from this:

```ucm
scratch/main> view List.drop
scratch/main> display bar.a
```

In the signature, we don't see `base.Nat`, just `Nat`. The full declaration name is still shown for each search result though.

Type-based search also benefits from this, we can just say `Nat` rather than `.base.Nat`:

```ucm
scratch/main> find : Nat -> [a] -> [a]
```

## Preferring names not in `lib.*.lib.*`

Suffix-based resolution prefers names that are not in an indirect dependency.

```unison
cool.abra.cadabra = "my project"
lib.distributed.abra.cadabra = "direct dependency 1"
lib.distributed.baz.qux = "direct dependency 2"
lib.distributed.lib.baz.qux = "indirect dependency"
```

```ucm
scratch/main> add
```

```unison:error
> abra.cadabra
```

```unison
> baz.qux
```

```ucm
scratch/main> view abra.cadabra
scratch/main> view baz.qux
```

Note that we can always still view indirect dependencies by using more name segments:

```ucm
scratch/main> view distributed.abra.cadabra
scratch/main> names distributed.lib.baz.qux
```

## Corner cases

If a definition is given in a scratch file, its suffixes shadow existing definitions that exist in the codebase with the same suffixes. For example:

```unison:hide
unique type A = Thing1 Nat | thing2 Nat

foo.a = 23
bar = 100
```

```ucm
scratch/main> add
```

```unison
unique type B = Thing1 Text | thing2 Text | Thing3 Text

zoink.a = "hi"

-- verifying that the `a` here references `zoink.a`
foo.baz.qux.bar : Text
foo.baz.qux.bar = a

-- verifying that the `bar` is resolving to `foo.baz.qux.bar`
-- and that `Thing1` references `B.Thing1` from the current file
fn = cases
  Thing1 msg -> msg Text.++ bar
  thing2 msg -> msg Text.++ bar
  _ -> todo "hmm"
```
