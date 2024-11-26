# Destructuring binds

``` ucm :hide
scratch/main> builtins.merge
```

Here's a couple examples:

``` unison
ex0 : Nat -> Nat
ex0 n =
  (a, _, (c,d)) = ("uno", "dos", (n, 7))
  c + d

ex1 : (a,b,(Nat,Nat)) -> Nat
ex1 tup =
  (a, b, (c,d)) = tup
  c + d
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ex0 : Nat -> Nat
      ex1 : (a, b, (Nat, Nat)) -> Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ex0 : Nat -> Nat
    ex1 : (a, b, (Nat, Nat)) -> Nat

scratch/main> view ex0 ex1

  ex0 : Nat -> Nat
  ex0 n =
    use Nat +
    (a, _, (c, d)) = ("uno", "dos", (n, 7))
    c + d

  ex1 : (a, b, (Nat, Nat)) -> Nat
  ex1 = cases (a, b, (c, d)) -> c Nat.+ d
```

Notice that `ex0` is printed using the `cases` syntax (but `ex1` is not). The pretty-printer currently prefers the `cases` syntax if definition can be printed using either destructuring bind or `cases`.

A destructuring bind is just syntax for a single branch pattern match. Notice that Unison detects this function as an alias of `ex1`:

``` unison
ex2 : (a,b,(Nat,Nat)) -> Nat
ex2 tup = match tup with
  (a, b, (c,d)) -> c + d
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ex2 : (a, b, (Nat, Nat)) -> Nat
        (also named ex1)
```

## Corner cases

Destructuring binds can't be recursive: the left-hand side bound variables aren't available on the right hand side. For instance, this doesn't typecheck:

``` unison :error
ex4 =
  (a,b) = (a Nat.+ b, 19)
  "Doesn't typecheck"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what a refers to here:

      2 |   (a,b) = (a Nat.+ b, 19)

  I think its type should be:

      Nat

  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
    * You have a typo in the name
```

Even though the parser accepts any pattern on the LHS of a bind, it looks pretty weird to see things like `12 = x`, so we avoid showing a destructuring bind when the LHS is a "literal" pattern (like `42` or "hi"). Again these examples wouldn't compile with coverage checking.

``` unison
ex5 : 'Text
ex5 _ = match 99 + 1 with
  12 -> "Hi"
  _ -> "Bye"

ex5a : 'Text
ex5a _ = match (99 + 1, "hi") with
  (x, "hi") -> "Not printed as a destructuring bind."
  _ -> "impossible"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ex5  : 'Text
      ex5a : 'Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ex5  : 'Text
    ex5a : 'Text

scratch/main> view ex5 ex5a

  ex5 : 'Text
  ex5 _ = match 99 Nat.+ 1 with
    12 -> "Hi"
    _  -> "Bye"

  ex5a : 'Text
  ex5a _ = match (99 Nat.+ 1, "hi") with
    (x, "hi") -> "Not printed as a destructuring bind."
    _         -> "impossible"
```

Notice how it prints both an ordinary match.

Also, for clarity, the pretty-printer shows a single-branch match if the match shadows free variables of the scrutinee, for example:

``` unison :hide
ex6 x = match x with
  (x, y) -> x Nat.+ y
```

For clarity, the pretty-printer leaves this alone, even though in theory it could be written `(x,y) = x; x + y`:

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ex6 : (Nat, Nat) -> Nat

scratch/main> view ex6

  ex6 : (Nat, Nat) -> Nat
  ex6 = cases (x, y) -> x Nat.+ y
```
