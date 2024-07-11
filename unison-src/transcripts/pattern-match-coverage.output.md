# Basics

## non-exhaustive patterns

``` unison
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   A -> ()
    
  
  Patterns not matched:
  
    * B
    * C

```
``` unison
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some A) -> ()
  (B, None) -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   (A, Some _) -> ()
        6 |   (A, None) -> ()
        7 |   (B, Some A) -> ()
        8 |   (B, None) -> ()
    
  
  Patterns not matched:
   * (B, Some B)

```
## redundant patterns

``` unison
unique type T = A | B | C

test : T -> ()
test = cases
  A -> ()
  B -> ()
  C -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        8 |   _ -> ()
    

```
``` unison
unique type T = A | B

test : (T, Optional T) -> ()
test = cases
  (A, Some _) -> ()
  (A, None) -> ()
  (B, Some _) -> ()
  (B, None) -> ()
  (A, Some A) -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        9 |   (A, Some A) -> ()
    

```
# Uninhabited patterns

match is complete without covering uninhabited patterns

``` unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type V
      test : Optional (Optional V) -> ()

```
uninhabited patterns are reported as redundant

``` unison
unique type V =

test0 : V -> ()
test0 = cases
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        5 |   _ -> ()
    

```
``` unison
unique type V =

test : Optional (Optional V) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        7 |   Some _ -> ()
    

```
# Guards

## Incomplete patterns due to guards should be reported

``` unison
test : () -> ()
test = cases
  () | false -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   () | false -> ()
    
  
  Patterns not matched:
   * ()

```
``` unison
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   None -> 0
        4 |   Some x
        5 |     | isEven x -> x
    
  
  Patterns not matched:
   * Some _

```
## Complete patterns with guards should be accepted

``` unison
test : Optional Nat -> Nat
test = cases
  None -> 0
  Some x
    | isEven x -> x
    | otherwise -> 0
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Optional Nat -> Nat

```
# Pattern instantiation depth

Uncovered patterns are only instantiated as deeply as necessary to
distinguish them from existing patterns.

``` unison
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   None -> ()
        6 |   Some None -> ()
    
  
  Patterns not matched:
   * Some (Some _)

```
``` unison
unique type T = A | B | C

test : Optional (Optional T) -> ()
test = cases
  None -> ()
  Some None -> ()
  Some (Some A) -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        4 | test = cases
        5 |   None -> ()
        6 |   Some None -> ()
        7 |   Some (Some A) -> ()
    
  
  Patterns not matched:
  
    * Some (Some B)
    * Some (Some C)

```
# Literals

## Non-exhaustive

Nat

``` unison
test : Nat -> ()
test = cases
  0 -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   0 -> ()
    
  
  Patterns not matched:
   * _

```
Boolean

``` unison
test : Boolean -> ()
test = cases
  true -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   true -> ()
    
  
  Patterns not matched:
   * false

```
## Exhaustive

Nat

``` unison
test : Nat -> ()
test = cases
  0 -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Nat -> ()

```
Boolean

``` unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : Boolean -> ()

```
# Redundant

Nat

``` unison
test : Nat -> ()
test = cases
  0 -> ()
  0 -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        4 |   0 -> ()
    

```
Boolean

``` unison
test : Boolean -> ()
test = cases
  true -> ()
  false -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        5 |   _ -> ()
    

```
# Sequences

## Exhaustive

``` unison
test : [()] -> ()
test = cases
  [] -> ()
  x +: xs -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : [()] -> ()

```
## Non-exhaustive

``` unison
test : [()] -> ()
test = cases
  [] -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   [] -> ()
    
  
  Patterns not matched:
   * (() +: _)

```
``` unison
test : [()] -> ()
test = cases
  x +: xs -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   x +: xs -> ()
    
  
  Patterns not matched:
   * []

```
``` unison
test : [()] -> ()
test = cases
  xs :+ x -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   xs :+ x -> ()
    
  
  Patterns not matched:
   * []

```
``` unison
test : [()] -> ()
test = cases
  x0 +: (x1 +: xs) -> ()
  [] -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   x0 +: (x1 +: xs) -> ()
        4 |   [] -> ()
    
  
  Patterns not matched:
   * (() +: [])

```
``` unison
test : [()] -> ()
test = cases
  [] -> ()
  x0 +: [] -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        2 | test = cases
        3 |   [] -> ()
        4 |   x0 +: [] -> ()
    
  
  Patterns not matched:
   * (() +: (() +: _))

```
## Uninhabited

`Cons` is not expected since `V` is uninhabited

``` unison
unique type V =

test : [V] -> ()
test = cases
  [] -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type V
      test : [V] -> ()

```
## Length restrictions can equate cons and nil patterns

Here the first pattern matches lists of length two or greater, the
second pattern matches lists of length 0. The third case matches when the
final element is `false`, while the fourth pattern matches when the
first element is `true`. However, the only possible list length at
the third or fourth clause is 1, so the first and final element must
be equal. Thus, the pattern match is exhaustive.

``` unison
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ false -> ()
  true +: xs -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      test : [Boolean] -> ()

```
This is the same idea as above but shows that fourth match is redundant.

``` unison
test : [Boolean] -> ()
test = cases
  [a, b] ++ xs -> ()
  [] -> ()
  xs :+ true -> ()
  true +: xs -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        6 |   true +: xs -> ()
    

```
This is another similar example. The first pattern matches lists of
length 5 or greater. The second matches lists of length 4 or greater where the
first and third element are true. The third matches lists of length 4
or greater where the final 4 elements are `true, false, true, false`.
The list must be exactly of length 4 to arrive at the second or third
clause, so the third pattern is redundant.

``` unison
test : [Boolean] -> ()
test = cases
  [a, b, c, d, f] ++ xs -> ()
  [true, _, true, _] ++ _ -> ()
  _ ++ [true, false, true, false] -> ()
  _ -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        5 |   _ ++ [true, false, true, false] -> ()
    

```
# bugfix: Sufficient data decl map

``` unison
unique type T = A

unit2t : Unit -> T
unit2t = cases
  () -> A
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type T
      unit2t : 'T

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type T
    unit2t : 'T

```
Pattern coverage checking needs the data decl map to contain all
transitive type dependencies of the scrutinee type. We do this
before typechecking begins in a roundabout way: fetching all
transitive type dependencies of references that appear in the expression.

This test ensures that we have fetched the `T` type although there is
no data decl reference to `T` in `witht`.

``` unison
witht : Unit
witht = match unit2t () with
  x -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      witht : ()

```
``` unison
unique type V =

evil : Unit -> V
evil = bug ""
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type V
      evil : 'V

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type V
    evil : 'V

```
``` unison
withV : Unit
withV = match evil () with
  x -> ()
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        3 |   x -> ()
    

```
``` unison
unique type SomeType = A
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type SomeType

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type SomeType

```
``` unison
unique type R = R SomeType

get x = match x with
  R y -> y
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type R
      get : R -> SomeType

```
``` unison
unique type R = { someType : SomeType }
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type R
      R.someType        : R -> SomeType
      R.someType.modify : (SomeType ->{g} SomeType) -> R ->{g} R
      R.someType.set    : SomeType -> R -> R

```
# Ability handlers

## Exhaustive ability handlers are accepted

``` unison
structural ability Abort where
  abort : {Abort} a


result : '{e, Abort} a -> {e} a
result f = handle !f with cases
       { x } -> x
       { abort -> _ } -> bug "aborted"
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort
      result : '{e, Abort} a ->{e} a

```
``` unison
structural ability Abort where
  abort : {Abort} a

unique type T = A | B

result : '{e, Abort} T -> {e} ()
result f = handle !f with cases
       { A } -> ()
       { B } -> ()
       { abort -> _ } -> bug "aborted"
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort
      result : '{e, Abort} T ->{e} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type T

```
``` unison
structural ability Abort where
  abort : {Abort} a

result : '{e, Abort} V -> {e} V
result f =
  impl : Request {Abort} V -> V
  impl = cases
       { abort -> _ } -> bug "aborted"
  handle !f with impl
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort
      result : '{e, Abort} V ->{e} V

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort
      structural ability Stream a
      handleMulti : '{Abort, Stream a} r -> (Optional r, [a])

```
## Non-exhaustive ability handlers are rejected

``` unison
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e} a
result f = handle !f with cases
       { abort -> _ } -> bug "aborted"
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        7 | result f = handle !f with cases
        8 |        { abort -> _ } -> bug "aborted"
    
  
  Patterns not matched:
  
    * { _ }
    * { abortWithMessage _ -> _ }

```
``` unison
structural ability Abort where
  abort : {Abort} a

unique type T = A | B

result : '{e, Abort} T -> {e} ()
result f = handle !f with cases
       { A } -> ()
       { abort -> _ } -> bug "aborted"
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        7 | result f = handle !f with cases
        8 |        { A } -> ()
        9 |        { abort -> _ } -> bug "aborted"
    
  
  Patterns not matched:
   * { B }

```
``` unison
unique ability Give a where
  give : a -> {Give a} Unit

unique type T = A | B

result : '{e, Give T} r -> {e} r
result f = handle !f with cases
       { x } -> x
       { give A -> resume } -> result resume
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        7 | result f = handle !f with cases
        8 |        { x } -> x
        9 |        { give A -> resume } -> result resume
    
  
  Patterns not matched:
   * { give B -> _ }

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
       10 |   impl xs = cases
       11 |     { r } -> (Some r, xs)
       12 |     { emit x -> resume } -> handle !resume with impl (xs :+ x)
    
  
  Patterns not matched:
   * { abort -> _ }

```
## Redundant handler cases are rejected

``` unison
unique ability Give a where
  give : a -> {Give a} Unit

unique type T = A | B

result : '{e, Give T} r -> {e} r
result f = handle !f with cases
       { x } -> x
       { give _ -> resume } -> result resume
       { give A -> resume } -> result resume
```

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
       10 |        { give A -> resume } -> result resume
    

```
## Exhaustive ability reinterpretations are accepted

``` unison
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e, Abort} a
result f = handle !f with cases
       { x } -> x
       { abort -> _ } -> abort
       { abortWithMessage msg -> _ } -> abortWithMessage ("aborting: " ++ msg)
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort
      result : '{e, Abort} a ->{e, Abort} a

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Abort a
      result : '{e, Abort V} a ->{e, Abort V} a

```
## Non-exhaustive ability reinterpretations are rejected

``` unison
structural ability Abort where
  abort : {Abort} a
  abortWithMessage : Text -> {Abort} a


result : '{e, Abort} a -> {e, Abort} a
result f = handle !f with cases
       { x } -> x
       { abortWithMessage msg -> _ } -> abortWithMessage ("aborting: " ++ msg)
```

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        7 | result f = handle !f with cases
        8 |        { x } -> x
        9 |        { abortWithMessage msg -> _ } -> abortWithMessage ("aborting: " ++ msg)
    
  
  Patterns not matched:
   * { abort -> _ }

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

``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  Pattern match doesn't cover all possible cases:
        8 |   impl = cases
        9 |        { x } -> x
    
  
  Patterns not matched:
  
    * { give _ -> _ }
    * { give2 _ -> _ }

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability Give a
      result : '{e, Give V} r ->{e} r

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability Give a
      result : '{e, Give V} r ->{e} r

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
       11 |        { give2 _ -> resume } -> bug "impossible"
    

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
       15 |        { giveA2 _ -> _ } -> bug "impossible"
    

```
``` unison
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

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability GiveA a
      ability GiveB a
      result : '{e, GiveA V, GiveB V} r ->{e} r

```
