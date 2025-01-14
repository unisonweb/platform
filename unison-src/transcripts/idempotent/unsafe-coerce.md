``` ucm :hide
scratch/main> builtins.merge
```

``` unison
f : '{} Nat
f _ = 5

fc : '{IO, Exception} Nat
fc = unsafe.coerceAbilities f

main : '{IO, Exception} [Result]
main _ =
  n = !fc
  if n == 5 then [Ok ""] else [Fail ""]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      f    : 'Nat
      fc   : '{IO, Exception} Nat
      main : '{IO, Exception} [Result]
```

``` ucm
scratch/main> find unsafe.coerceAbilities

  1. builtin.unsafe.coerceAbilities : (a ->{e1} b) -> a -> b

scratch/main> add

  ⍟ I've added these definitions:

    f    : 'Nat
    fc   : '{IO, Exception} Nat
    main : '{IO, Exception} [Result]

scratch/main> io.test main

    New test results:

    1. main   ◉ 

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
