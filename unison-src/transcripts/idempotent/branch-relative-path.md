``` unison
foo = 5
foo.bar = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo     : ##Nat
      foo.bar : ##Nat
```

``` ucm
p0/main> add

  ⍟ I've added these definitions:

    foo     : ##Nat
    foo.bar : ##Nat
```

``` unison
bonk = 5
donk.bonk = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bonk      : ##Nat
        (also named foo)
      donk.bonk : ##Nat
        (also named foo.bar)
```

``` ucm
p1/main> add

  ⍟ I've added these definitions:

    bonk      : ##Nat
    donk.bonk : ##Nat

p1/main> fork "p0/main:" zzz

  Done.

p1/main> find zzz

  1. zzz.foo : ##Nat
  2. zzz.foo.bar : ##Nat

p1/main> fork "p0/main:foo" yyy

  Done.

p1/main> find yyy

  1. yyy.bar : ##Nat

p0/main> fork "p1/main:" "p0/main:p1"

  Done.

p0/main> ls p1

  1. bonk  (##Nat)
  2. donk/ (1 term)
  3. yyy/  (1 term)
  4. zzz/  (2 terms)

p0/main> ls p1.zzz

  1. foo  (##Nat)
  2. foo/ (1 term)

p0/main> ls p1.yyy

  1. bar (##Nat)
```
