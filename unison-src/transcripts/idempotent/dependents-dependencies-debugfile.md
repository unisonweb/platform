``` ucm :hide
scratch/main> builtins.merge
```

### `debug.file`

I can use `debug.file` to see the hashes of the last typechecked file.

Given this .u file:

``` unison :hide
structural type outside.A = A Nat outside.B
structural type outside.B = B Int
outside.c = 3
outside.d = c < (p + 1)

structural type inside.M = M outside.A
inside.p = c
inside.q x = x + p * p
inside.r = d
```

``` ucm
scratch/main> debug.file

  type inside.M#h37a56c5ep
  type outside.A#6l6krl7n4l
  type outside.B#eo6rj0lj1b
  inside.p#htoo5rnb54
  inside.q#1mqcoh3tnk
  inside.r#nkgohbke6n
  outside.c#f3lgjvjqoo
  outside.d#ukd7tu6kds
```

This will help me make progress in some situations when UCM is being deficient or broken.

### `dependents` / `dependencies`

But wait, there's more.  I can check the dependencies and dependents of a definition:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type inside.M
    structural type outside.A
    structural type outside.B
    inside.p  : Nat
    inside.q  : Nat -> Nat
    inside.r  : Boolean
    outside.c : Nat
    outside.d : Boolean

scratch/main> dependents q

  q has no dependents.

scratch/main> dependencies q

  Dependencies of: q

    Types:

    1. builtin.Nat

    Terms:

    2. builtin.Nat.*
    3. builtin.Nat.+
    4. inside.p

  Tip: Try `view 4` to see the source of any numbered item in
       the above list.

scratch/main> dependencies B

  Dependencies of: type B, B

    Types:

    1. builtin.Int
    2. outside.B

  Tip: Try `view 2` to see the source of any numbered item in
       the above list.

scratch/main> dependencies d

  Dependencies of: d

    Types:

    1. builtin.Boolean

    Terms:

    2. builtin.Nat.+
    3. builtin.Universal.<
    4. inside.p
    5. outside.c

  Tip: Try `view 5` to see the source of any numbered item in
       the above list.

scratch/main> dependents d

  Dependents of: d

    Terms:

    1. inside.r

  Tip: Try `view 1` to see the source of any numbered item in
       the above list.
```

We don't have an index for dependents of constructors, but iirc if you ask for that, it will show you dependents of the structural type that provided the constructor.
