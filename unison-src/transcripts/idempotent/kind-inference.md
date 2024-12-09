``` ucm :hide
scratch/main> builtins.merge
```

## A type param cannot have conflicting kind constraints within a single decl

conflicting constraints on the kind of `a` in a product

``` unison :error
unique type T a = T a (a Nat)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        1 | unique type T a = T a (a Nat)
    
    a doesn't expect an argument; however, it is applied to Nat.
```

conflicting constraints on the kind of `a` in a sum

``` unison :error
unique type T a
  = Star a
  | StarStar (a Nat)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        2 |   = Star a
    
    The arrow type (->) expects arguments of kind Type; however,
    it is applied to a which has kind: Type -> Type.
```

## Kinds are inferred by decl component

Successfully infer `a` in `Ping a` to be of kind `* -> *` by
inspecting its component-mate `Pong`.

``` unison
unique type Ping a = Ping Pong
unique type Pong = Pong (Ping Optional)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Ping a
      type Pong
```

Catch the conflict on the kind of `a` in `Ping a`. `Ping` restricts
`a` to `*`, whereas `Pong` restricts `a` to `* -> *`.

``` unison :error
unique type Ping a = Ping a Pong
unique type Pong = Pong (Ping Optional)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        1 | unique type Ping a = Ping a Pong
    
    The arrow type (->) expects arguments of kind Type; however,
    it is applied to a which has kind: Type -> Type.
```

Successful example between mutually recursive type and ability

``` unison
unique type Ping a = Ping (a Nat -> {Pong Nat} ())
unique ability Pong a where
  pong : Ping Optional -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Ping a
      ability Pong a
```

Catch conflict between mutually recursive type and ability

``` unison :error
unique type Ping a = Ping (a -> {Pong Nat} ())
unique ability Pong a where
  pong : Ping Optional -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        3 |   pong : Ping Optional -> ()
    
    Ping expects an argument of kind: Type; however, it is
    applied to Optional which has kind: Type -> Type.
```

Consistent instantiation of `T`'s `a` parameter in `S`

``` unison
unique type T a = T a

unique type S = S (T Nat)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type S
      type T a
```

Delay kind defaulting until all components are processed. Here `S`
constrains the kind of `T`'s `a` parameter, although `S` is not in
the same component as `T`.

``` unison
unique type T a = T

unique type S = S (T Optional)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type S
      type T a
```

Catch invalid instantiation of `T`'s `a` parameter in `S`

``` unison :error
unique type T a = T a

unique type S = S (T Optional)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        3 | unique type S = S (T Optional)
    
    T expects an argument of kind: Type; however, it is applied
    to Optional which has kind: Type -> Type.
```

## Checking annotations

Catch kind error in type annotation

``` unison :error
test : Nat Nat
test = 0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        1 | test : Nat Nat
    
    Nat doesn't expect an argument; however, it is applied to
    Nat.
```

Catch kind error in annotation example 2

``` unison :error
test : Optional -> ()
test _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        1 | test : Optional -> ()
    
    The arrow type (->) expects arguments of kind Type; however,
    it is applied to Optional which has kind: Type -> Type.
```

Catch kind error in annotation example 3

``` unison :error
unique type T a = T (a Nat)

test : T Nat -> ()
test _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        3 | test : T Nat -> ()
    
    T expects an argument of kind: Type -> Type; however, it is
    applied to Nat which has kind: Type.
```

Catch kind error in scoped type variable annotation

``` unison :error
unique type StarStar a = StarStar (a Nat)
unique type Star a = Star a

test : StarStar a -> ()
test _ =
  buggo : Star a
  buggo = bug ""
  ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        6 |   buggo : Star a
    
    Star expects an argument of kind: Type; however, it is
    applied to a which has kind: Type -> Type.
```

## Effect/type mismatch

Effects appearing where types are expected

``` unison :error
unique ability Foo where
  foo : ()

test : Foo -> ()
test _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        4 | test : Foo -> ()
    
    The arrow type (->) expects arguments of kind Type; however,
    it is applied to Foo which has kind: Ability.
```

Types appearing where effects are expected

``` unison :error
test : {Nat} ()
test _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Kind mismatch arising from
        1 | test : {Nat} ()
    
    An ability list must consist solely of abilities; however,
    this list contains Nat which has kind Type. Abilities are of
    kind Ability.
```

## Cyclic kinds

``` unison :error
unique type T a = T (a a)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Cannot construct infinite kind
        1 | unique type T a = T (a a)
    
    The above application constrains the kind of a to be
    infinite, generated by the constraint k = k -> Type where k
    is the kind of a.
```

``` unison :error
unique type T a b = T (a b) (b a)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Cannot construct infinite kind
        1 | unique type T a b = T (a b) (b a)
    
    The above application constrains the kind of b to be
    infinite, generated by the constraint
    k = (k -> Type) -> Type where k is the kind of b.
```

``` unison :error
unique type Ping a = Ping (a Pong)
unique type Pong a = Pong (a Ping)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  Cannot construct infinite kind
        1 | unique type Ping a = Ping (a Pong)
    
    The above application constrains the kind of a to be
    infinite, generated by the constraint
    k = (((k -> Type) -> Type) -> Type) -> Type where k is the
    kind of a.
```
