``` ucm :hide
scratch/main> builtins.merge
```

Some random ability stuff to ensure things work.

``` unison :hide

unique ability A where
  one : Nat ->{A} Nat
  two : Nat -> Nat ->{A} Nat
  three : Nat -> Nat -> Nat ->{A} Nat
  four : Nat ->{A} (Nat -> Nat -> Nat -> Nat)

ha : Request {A} r -> r
ha = cases
  { x } -> x
  { one i -> c } -> handle c (i+1) with ha
  { two i j -> c } -> handle c (i+j) with ha
  { three i j k -> c } -> handle c (i+j+k) with ha
  { four i -> c } -> handle c (j k l -> i+j+k+l) with ha
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ability A
    ha : Request {A} r -> r
```
