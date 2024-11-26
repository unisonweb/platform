# `edit.dependents`

The `edit.dependents` command is like `edit`, but it adds a definition and all of its transitive dependents to the file
(being careful not to add anything that's already there).

``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

``` unison
type Foo = Foo Nat Nat
type Bar = { bar : Foo }

baz : Bar -> Bar
baz x = x
```

``` ucm
scratch/main> add
```

Let's populate our scratch file with `Bar` (and its auto-generated accessors), then `edit.dependents` its dependency
`Foo`, which should add `Foo` and `baz`.

``` unison
type Bar = { bar : Nat }
```

``` ucm
scratch/main> edit.dependents Foo
```

``` ucm :hide
scratch/main> project.delete scratch
```
