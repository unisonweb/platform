```ucm:hide
scratch/main> builtins.merge
```

# Basics
## non-exhaustive patterns
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
```

```unison:error
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some A) -> ()
  (B, None) -> ()
```

## redundant patterns
```unison:error
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
  B -> ()
  C -> ()
  _ -> ()
```

```unison:error
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some _) -> ()
  (B, None) -> ()
  (A, Some A) -> ()
```

# Uninhabited patterns

match is complete without covering uninhabited patterns
```unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
```

uninhabited patterns are reported as redundant
```unison:error
unique type V =

test0 : V -> ()
test0 = cases
  _ -> ()
```

```unison:error
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some _ -> ()
```

# Guards

## Incomplete patterns due to guards should be reported
```unison:error
test : () -> ()
test = cases
  () | false -> ()
```

```unison:error
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
```

## Complete patterns with guards should be accepted
```unison:error
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
    | otherwise -> 0
```

# Pattern instantiation depth

Uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns.
```unison:error
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
```

```unison:error
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some (Some A) -> ()
```

# Literals

## Non-exhaustive

Nat
```unison:error
test : Nat -> ()
test = cases
  0 -> ()
```

Boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
```

## Exhaustive

Nat
```unison
test : Nat -> ()
test = cases
  0 -> ()
  _ -> ()
```

Boolean
```unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
```

# Redundant

Nat
```unison:error
test : Nat -> ()
test = cases
  0 -> ()
  0 -> ()
  _ -> ()
```

Boolean
```unison:error
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
  _ -> ()
```

# Sequences

## Exhaustive
```unison
test : [()] -> ()
test = cases
  [] -> ()
  x +: xs -> ()
```

## Non-exhaustive
```unison:error
test : [()] -> ()
test = cases
  [] -> ()
```

```unison:error
test : [()] -> ()
test = cases
  x +: xs -> ()
```

```unison:error
test : [()] -> ()
test = cases
  xs :+ x -> ()
```

```unison:error
test : [()] -> ()
test = cases
  x0 +: (x1 +: xs) -> ()
  [] -> ()
```

```unison:error
test : [()] -> ()
test = cases
  [] -> ()
  x0 +: [] -> ()
```

## Uninhabited

`Cons` is not expected since `V` is uninhabited
```unison
unique type V =

test : [V] -> ()
test = cases
  [] -> ()
```

## Length restrictions can equate cons and nil patterns

Here the first pattern matches lists of length two or greater, the
second pattern matches lists of length 0. The third case matches when the
final element is `false`, while the fourth pattern matches when the
first element is `true`. However, the only possible list length at
the third or fourth clause is 1, so the first and final element must
be equal. Thus, the pattern match is exhaustive.
```unison
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ false -> ()
  true +: xs -> ()
```

This is the same idea as above but shows that fourth match is redundant.
```unison:error
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ true -> ()
  true +: xs -> ()
  _ -> ()
```

This is another similar example. The first pattern matches lists of
length 5 or greater. The second matches lists of length 4 or greater where the
first and third element are true. The third matches lists of length 4
or greater where the final 4 elements are `true, false, true, false`.
The list must be exactly of length 4 to arrive at the second or third
clause, so the third pattern is redundant.
```unison:error
test : [Boolean] -> ()
test = cases
  [a, b, c, d, f] ++ xs -> ()
  [true, _, true, _] ++ _ -> ()
  _ ++ [true, false, true, false] -> ()
  _ -> ()
```

# bugfix: Sufficient data decl map

```unison
unique type T = A

unit2t : Unit -> T
unit2t = cases
  () -> A
```

```ucm
scratch/main> add
```

Pattern coverage checking needs the data decl map to contain all
transitive type dependencies of the scrutinee type. We do this
before typechecking begins in a roundabout way: fetching all
transitive type dependencies of references that appear in the expression.

This test ensures that we have fetched the `T` type although there is
no data decl reference to `T` in `witht`.
```unison
witht : Unit
witht = match unit2t () with
  x -> ()
```

```unison
unique type V =

evil : Unit -> V
evil = bug ""
```

```ucm
scratch/main> add
```

```unison:error
withV : Unit
withV = match evil () with
  x -> ()
```

```unison
unique type SomeType = A
```

```ucm
scratch/main> add
```

```unison
unique type R = R SomeType

get x = match x with
  R y -> y
```

```unison
unique type R = { someType : SomeType }
```

# Ability handlers

## Exhaustive ability handlers are accepted

```unison
structural ability Abort where
  abort : {Abort} a


result : '{e, Abort} a -> {e} a
result f = handle !f with cases
       { x } -> x
       { abort -> _ } -> bug "aborted"
```

```unison
structural ability Abort where
  abort : {Abort} a

unique type T = A | B

result : '{e, Abort} T -> {e} ()
result f = handle !f with cases
       { A } -> ()
       { B } -> ()
       { abort -> _ } -> bug "aborted"
```

```unison
structural ability Abort where
  abort : {Abort} a

result : '{e, Abort} V -> {e} V
result f =
  impl : Request {Abort} V -> V
  impl = cases
       { abort -> _ } -> bug "aborted"
  handle !f with impl
```

```unison
structural ability Abort where
  abort : {Abort} a

structural ability Stream a where
  emit : a -> {Stream a} Unit

handleMulti : '{Stream a, Abort} r -> (Optional r, [a])
handleMulti c =
  impl xs = cases
    { r } -> (Some r, xs)
    { emit x -> resume } -> handle !resume with impl (xs :+ x)
    { abort -> _ } -> (None, xs)
  handle !c with impl []
```

## Non-exhaustive ability handlers are rejected

```unison:error
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e} a
result f = handle !f with cases
       { abort -> _ } -> bug "aborted"
```

```unison:error
structural ability Abort where
  abort : {Abort} a

unique type T = A | B

result : '{e, Abort} T -> {e} ()
result f = handle !f with cases
       { A } -> ()
       { abort -> _ } -> bug "aborted"
```

```unison:error
unique ability Give a where
  give : a -> {Give a} Unit

unique type T = A | B

result : '{e, Give T} r -> {e} r
result f = handle !f with cases
       { x } -> x
       { give A -> resume } -> result resume
```

```unison:error
structural ability Abort where
  abort : {Abort} a

structural ability Stream a where
  emit : a -> {Stream a} Unit

handleMulti : '{Stream a, Abort} r -> (Optional r, [a])
handleMulti c =
  impl : [a] -> Request {Stream a, Abort} r -> (Optional r, [a])
  impl xs = cases
    { r } -> (Some r, xs)
    { emit x -> resume } -> handle !resume with impl (xs :+ x)
  handle !c with impl []
```

## Redundant handler cases are rejected

```unison:error
unique ability Give a where
  give : a -> {Give a} Unit

unique type T = A | B

result : '{e, Give T} r -> {e} r
result f = handle !f with cases
       { x } -> x
       { give _ -> resume } -> result resume
       { give A -> resume } -> result resume
```

## Exhaustive ability reinterpretations are accepted

```unison
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e, Abort} a
result f = handle !f with cases
       { x } -> x
       { abort -> _ } -> abort
       { abortWithMessage msg -> _ } -> abortWithMessage ("aborting: " ++ msg)
```

```unison
structural ability Abort a where
  abort : {Abort a} r
  abortWithMessage : a -> {Abort a} r

result : '{e, Abort V} a -> {e, Abort V} a
result f =
  impl : Request {Abort V} r -> {Abort V} r
  impl = cases
       { x } -> x
       { abort -> _ } -> abort
  handle !f with impl
```

## Non-exhaustive ability reinterpretations are rejected

```unison:error
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e, Abort} a
result f = handle !f with cases
       { x } -> x
       { abortWithMessage msg -> _ } -> abortWithMessage ("aborting: " ++ msg)
```

## Hacky workaround for uninhabited abilities

Although all of the constructors of an ability might be uninhabited,
the typechecker requires at least one be specified so that it can
determine that the ability should be discharged. So, the default
pattern match coverage checking behavior of prohibiting covering any
of the cases is problematic. Instead, the pattern match coverage
checker will require that at least one constructor be given, even if
they are all uninhabited.

The messages here aren't the best, but I don't think uninhabited
abilities will come up and get handlers written for them often.

```unison:error
unique ability Give a where
  give : a -> {Give a} Unit
  give2 : a -> {Give a} Unit

result : '{e, Give V} r -> {e} r
result f =
  impl : Request {Give V} r -> {} r
  impl = cases
       { x } -> x
  handle !f with impl
```

```unison
unique ability Give a where
  give : a -> {Give a} Unit
  give2 : a -> {Give a} Unit

result : '{e, Give V} r -> {e} r
result f =
  impl : Request {Give V} r -> {} r
  impl = cases
       { x } -> x
       { give _ -> resume } -> bug "impossible"
  handle !f with impl
```

```unison
unique ability Give a where
  give : a -> {Give a} Unit
  give2 : a -> {Give a} Unit

result : '{e, Give V} r -> {e} r
result f =
  impl : Request {Give V} r -> {} r
  impl = cases
       { x } -> x
       { give2 _ -> resume } -> bug "impossible"
  handle !f with impl
```

```unison:error
unique ability Give a where
  give : a -> {Give a} Unit
  give2 : a -> {Give a} Unit

result : '{e, Give V} r -> {e} r
result f =
  impl : Request {Give V} r -> {} r
  impl = cases
       { x } -> x
       { give _ -> resume } -> bug "impossible"
       { give2 _ -> resume } -> bug "impossible"
  handle !f with impl
```

```unison:error
unique ability GiveA a where
  giveA : a -> {GiveA a} Unit
  giveA2 : a -> {GiveA a} Unit

unique ability GiveB a where
  giveB : a -> {GiveB a} Unit
  giveB2 : a -> {GiveB a} Unit

result : '{e, GiveA V, GiveB V} r -> {e} r
result f =
  impl : Request {GiveA V, GiveB V} r -> {} r
  impl = cases
       { x } -> x
       { giveA _ -> _ } -> bug "impossible"
       { giveA2 _ -> _ } -> bug "impossible"
       { giveB _ -> _ } -> bug "impossible"
       { giveB2 _ -> _ } -> bug "impossible"
  handle !f with impl
```

```unison
unique ability GiveA a where
  giveA : a -> {GiveA a} Unit
  giveA2 : a -> {GiveA a} Unit

unique ability GiveB a where
  giveB : a -> {GiveB a} Unit
  giveB2 : a -> {GiveB a} Unit

result : '{e, GiveA V, GiveB V} r -> {e} r
result f =
  impl : Request {GiveA V, GiveB V} r -> {} r
  impl = cases
       { x } -> x
       { giveA2 _ -> _ } -> bug "impossible"
       { giveB _ -> _ } -> bug "impossible"
  handle !f with impl
```
