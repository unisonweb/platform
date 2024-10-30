# An assortment of regression tests that exercise various interesting cases within the runtime.

```ucm:hide
scratch/main> builtins.merge lib.builtins
```


```unison
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
