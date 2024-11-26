This transcript shows how the pretty-printer picks names for a hash when multiple are available. The algorithm is:

1.  Names that are "name-only" come before names that are hash qualified. So `List.map` comes before `List.map#2384a` and also `aaaa#xyz`.
2.  Shorter names (in terms of segment count) come before longer ones, for instance `base.List.map` comes before `somelibrary.external.base.List.map`.
3.  Otherwise if there are multiple names with a minimal number of segments, compare the names alphabetically.

``` ucm :hide
scratch/main> builtins.merge lib.builtins

scratch/biasing> builtins.merge lib.builtins
```

``` unison :hide
a.a = a.b + 1
a.b = 0 + 1
a.aaa.but.more.segments = 0 + 1
```

Will add `a` and `b` to the codebase and give `b` a longer (in terms of segment length alias), and show that it isn't used when viewing `a`:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    a.a                     : Nat
    a.aaa.but.more.segments : Nat
    a.b                     : Nat

scratch/main> view a.a

  a.a : Nat
  a.a =
    use Nat +
    b + 1
```

Next let's introduce a conflicting symbol and show that its hash qualified name isn't used when it has an unconflicted name:

``` unison :hide
a2.a = a2.b + 1
a2.b = 0 + 1
a2.aaa.but.more.segments = 0 + 1
a2.c = 1
a2.d = a2.c + 10
a2.long.name.but.shortest.suffixification = 1

a3.a = a3.b + 1
a3.b = 0 + 1
a3.aaa.but.more.segments = 0 + 1
a3.c = 2
a3.d = a3.c + 10
a3.long.name.but.shortest.suffixification = 1
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    a2.a                                      : Nat
      (also named a.a)
    a2.aaa.but.more.segments                  : Nat
      (also named a.b and a.aaa.but.more.segments)
    a2.b                                      : Nat
      (also named a.b and a.aaa.but.more.segments)
    a2.c                                      : Nat
    a2.d                                      : Nat
    a2.long.name.but.shortest.suffixification : Nat
    a3.a                                      : Nat
      (also named a.a)
    a3.aaa.but.more.segments                  : Nat
      (also named a.b and a.aaa.but.more.segments)
    a3.b                                      : Nat
      (also named a.b and a.aaa.but.more.segments)
    a3.c                                      : Nat
    a3.d                                      : Nat
    a3.long.name.but.shortest.suffixification : Nat

scratch/main> debug.alias.term.force a2.c a3.c

  Done.

scratch/main> debug.alias.term.force a2.d a3.d

  Done.
```

At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized.
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope,
`a2.c` is chosen because although the suffixified version has fewer segments, its fully-qualified name has the fewest segments.

``` ucm
scratch/main> view a b c d

  a.a : Nat
  a.a =
    use Nat +
    b + 1

  a.b : Nat
  a.b =
    use Nat +
    0 + 1

  a2.c : Nat
  a2.c = 1

  a2.d : Nat
  a2.d =
    use Nat +
    a2.c + 10

  a3.c#dcgdua2lj6 : Nat
  a3.c#dcgdua2lj6 = 2

  a3.d#9ivhgvhthc : Nat
  a3.d#9ivhgvhthc =
    use Nat +
    c#dcgdua2lj6 + 10
```

## Name biasing

``` unison
deeply.nested.term =
  a + 1

deeply.nested.num = 10

a = 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a                  : Nat
      deeply.nested.num  : Nat
      deeply.nested.term : Nat
```

``` ucm
scratch/biasing> add

  ⍟ I've added these definitions:

    a                  : Nat
    deeply.nested.num  : Nat
    deeply.nested.term : Nat

-- Despite being saved with name `a`,

-- the pretty printer should prefer the suffixified 'deeply.nested.num name' over the shallow 'a'.

-- It's closer to the term being printed.

scratch/biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    num + 1
```

Add another term with `num` suffix to force longer suffixification of `deeply.nested.num`

``` unison
other.num = 20
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      other.num : Nat
```

``` ucm
scratch/biasing> add

  ⍟ I've added these definitions:

    other.num : Nat

-- nested.num should be preferred over the shorter name `a` due to biasing

-- because `deeply.nested.num` is nearby to the term being viewed.

scratch/biasing> view deeply.nested.term

  deeply.nested.term : Nat
  deeply.nested.term =
    use Nat +
    nested.num + 1
```
