# An assortment of regression tests that exercise various interesting cases within the runtime.

``` unison
negativeCaseMatch = match -10 with
  +1 -> "bad"
  -10 -> "good"
  +3 -> "bad"
  _ -> "bad"
> negativeCaseMatch

funcWithMoreThanTwoUnboxedArgs : Nat -> Nat -> Nat -> Nat
funcWithMoreThanTwoUnboxedArgs x y z =
  x + y + z

> funcWithMoreThanTwoUnboxedArgs 1 2 3

funcWithMixedArgTypes : Nat -> Text -> Nat -> Text
funcWithMixedArgTypes x y z =
  Nat.toText x ++ y ++ Nat.toText z

> funcWithMixedArgTypes 1 "hello" 2

unboxedAndBoxedArgsInSequences = ([1, 2, 3], ["x", "y", "z"])
> unboxedAndBoxedArgsInSequences

casting = (Nat.toInt 100,
           Float.toRepresentation 3.14,
           Float.fromRepresentation 4614253070214989087,
           Int.fromRepresentation 100,
           Int.toRepresentation +10,
           Int.toRepresentation -10)
> casting
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      casting                        : ( Int,
                                         Nat,
                                         Float,
                                         Int,
                                         Nat,
                                         Nat)
      funcWithMixedArgTypes          : Nat
                                       -> Text
                                       -> Nat
                                       -> Text
      funcWithMoreThanTwoUnboxedArgs : Nat -> Nat -> Nat -> Nat
      negativeCaseMatch              : Text
      unboxedAndBoxedArgsInSequences : ([Nat], [Text])
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    6 | > negativeCaseMatch
          ⧩
          "good"
  
    12 | > funcWithMoreThanTwoUnboxedArgs 1 2 3
           ⧩
           6
  
    18 | > funcWithMixedArgTypes 1 "hello" 2
           ⧩
           "1hello2"
  
    21 | > unboxedAndBoxedArgsInSequences
           ⧩
           ([1, 2, 3], ["x", "y", "z"])
  
    29 | > casting
           ⧩
           ( +100
           , 4614253070214989087
           , 3.14
           , +100
           , 10
           , 18446744073709551606
           )

```
