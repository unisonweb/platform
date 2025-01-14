Brief document discussing Unison's algebraic effects.

  - The type `a ->{IO} b` type is a function from `a` to `b`, which requires the `IO` ability. The `{}` should be thought of as being attached to the `->`.
  - The `{}` syntax can contain any number of comma separated types, like `a ->{IO, Abort, State Nat} b`. We call the `{}` list the "required abilities" of the function.
  - Within an abilities list, type variables like `{e1, e2}` can be instantiated to sets of abilities, so we should think of the `{}` as just taking the union of all the sets contained therein. `IO` within `{IO}` is really the singleton set.
  - Unison's typechecker prevents calling a function whose required abilities aren't available in the currrent expression. We say that at each subexpression of the program, there's an *ambient* set of abilities available, and when calling a function `f : a ->{e1,e2} b`, the ambient abilities must be at least as big as as `{e1, e2}` (according to the subtyping judgement). Verifying that these requested abilities are available is called an "ability check".
  - The ambient abilities at a subterm is defined to be equal to the required abilities on the type of the *nearest enclosing lambda*. For instance, within the body of a lambda of type `a ->{Remote} b`, `{Remote}` is the ambient set.
  - Okay the above isn't quite right because `handle` blocks prepend new abilities to the ambient based on the abilities that the handler eliminates. So a handler `h : Request {IO} a -> b` will grant access to `IO` within the `body` of `handle h in body`. So the ambient set is really the required abilities on the type of the nearest enclosing lambda, plus the abilities eliminated by enclosing handlers.

Here are a few examples:

``` haskell
foo : Text ->{} ()
foo name = IO.printLine ("Hello, " ++ name)
```

Triggers an ability check failure, since the nearest enclosing lambda requires `{}`, the empty set of abilities. Therefore the body of that lambda doesn't have access to `IO`.

``` haskell
foo2 : Text ->{IO} Text ->{} ()
foo2 name1 name = IO.printLine ("Hello, " ++ name)
```

This also triggers an ability check failure. The inner lambda still requires only `{}` and we don't get access to abilities required by outer lambdas. This would be unsound (you could partially apply the function, then obtain a function with a smaller abilities requirement than what it actually used).

This would work:

``` haskell
foo2 : Text ->{IO} Text ->{} ()
foo2 name1 =
  IO.printLine ("Hello, " ++ name1)
  name -> ()
```

Notice that we get access to `IO` after just the first argument is supplied. The lambda we return though can't use `IO`.

TODO: handle blocks

## Type annotations and ability inference

The type of the nearest enclosing lambda and therefore the ambient set can't always be known in advance, if the user hasn't provided type annotations. In this case, we invent an existential type parameter for the ambient set and allow the existential to be refined by the normal ability checks.

I realized it's not sound to do Frank-style effect generalization after typechecking and have a different proposal instead. For instance, suppose we have the function:

``` haskell
map : (a -> b) -> [a] -> [b]
```

Which we typecheck and then afterwards generalize to:

``` haskell
map : (a ->{e} b) -> [a] ->{e} [b]
```

Except, what if that function `a ->{e} b` were actually being passed (within the body of `map`) to some other function that was expecting an `a ->{} b`? We can't just generalize this willy nilly, we actually need to typecheck with the enriched type.

So I propose the following:

  - The type `a -> b` means `a ->{e} b` for some existential `e` to be inferrered by Unison. It doesn't mean `forall e . a ->{e} b` or `a ->{} b`.
  - And as before:
      - The type `a ->{} b` means a function with no required abilities, AKA a pure function
      - The type `a ->{e} b` means a function with exactly `e` as its required abilities

So, the `map` function, assuming it were implemented in an ability-polymorphic way, would get the signature:

``` haskell
map : (a ->{e} b) -> [a] ->{e} [b]
```

This would be the type it would get if inferred, or if the user provided the signature `(a -> b) -> [a] -> [b]` to the function, it would note this elaborated type for the user (and possibly link to some docs about what this means).

This is sound and should work fine. It has the benefit of being highly nonmagical. I think it could also good for teaching about abilities: one can write "simple" type signatures and have them be elaborated automatically, which builds some familiarity. A downside is that the user will see more ability type variables. But maybe that's a feature, not a bug.

A couple usability improvements can elide ability type variables in various cases:

  - When displaying a type signature, we can elide any ability type variables that are mentioned just once by the type (as in `forall e . Nat ->{e} Nat`). If the variable is mentioned more than once in the signature, we include it, since it's adding useful information about what the function does and how it works. A principle here is that it's okay to eliminate informtaion from an arrow `a ->{e} b` and show that as `a -> b` if the user can use that as an `a ->{e} b` for any choice of `e`, including `{}`.
  - Another possible usability thing that's maybe more questionable, eliminate any empty `{}` that aren't to the left of an `->`. So for instance `Nat ->{} Nat ->{} Text` would display as just `Nat -> Nat -> Text`, but like `(a ->{} b) -> blah` would still display as `(a ->{} b) -> blah` since the `{}` appear to the left of an `->`.

### Question

Given the above, wow do we decide when a type signature is redundant, for purposes of determining whether to store that signature along with the type?
