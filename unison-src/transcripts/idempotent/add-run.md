# add.run

## Basic usage

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

is2even : 'Boolean
is2even = '(even 2)
```

it errors if there isn't a previous run

``` ucm :error
scratch/main> add.run foo

  ⚠️

  There is no previous evaluation to save. Use `run` to evaluate
  something before attempting to save it.
```

``` ucm
scratch/main> run is2even

  true
```

it errors if the desired result name conflicts with a name in the
unison file

``` ucm :error
scratch/main> add.run is2even

  ⚠️

  Cannot save the last run result into `is2even` because that
  name conflicts with a name in the scratch file.
```

otherwise, the result is successfully persisted

``` ucm
scratch/main> add.run foo.bar.baz

  ⍟ I've added these definitions:

    foo.bar.baz : Boolean
```

``` ucm
scratch/main> view foo.bar.baz

  foo.bar.baz : Boolean
  foo.bar.baz = true
```

## It resolves references within the unison file

``` unison
z b = b Nat.+ 12
y a b = a Nat.+ b Nat.+ z 10




main : '{IO, Exception} (Nat -> Nat -> Nat)
main _ = y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      main : '{IO, Exception} (Nat -> Nat -> Nat)
      y    : Nat -> Nat -> Nat
      z    : Nat -> Nat
```

``` ucm
scratch/main> run main

  a b -> a Nat.+ b Nat.+ z 10

scratch/main> add.run result

  ⍟ I've added these definitions:

    result : Nat -> Nat -> Nat
    z      : Nat -> Nat
```

## It resolves references within the codebase

``` unison
inc : Nat -> Nat
inc x = x + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      inc : Nat -> Nat
```

``` ucm
scratch/main> add inc

  ⍟ I've added these definitions:

    inc : Nat -> Nat
```

``` unison :hide
main : '(Nat -> Nat)
main _ x = inc x
```

``` ucm
scratch/main> run main

  inc

scratch/main> add.run natfoo

  ⍟ I've added these definitions:

    natfoo : Nat -> Nat

scratch/main> view natfoo

  natfoo : Nat -> Nat
  natfoo = inc
```

## It captures scratch file dependencies at run time

``` unison
x = 1
y = x + x
main = 'y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      main : 'Nat
      x    : Nat
      y    : Nat
```

``` ucm
scratch/main> run main

  2
```

``` unison :hide
x = 50
```

this saves 2 to xres, rather than 100

``` ucm
scratch/main> add.run xres

  ⍟ I've added these definitions:

    xres : Nat

scratch/main> view xres

  xres : Nat
  xres = 2
```

## It fails with a message if add cannot complete cleanly

``` unison :hide
main = '5
```

``` ucm :error
scratch/main> run main

  5

scratch/main> add.run xres

  x These definitions failed:

    Reason
    needs update   xres   : Nat

    Tip: Use `help filestatus` to learn more.
```

## It works with absolute names

``` unison :hide
main = '5
```

``` ucm
scratch/main> run main

  5

scratch/main> add.run .an.absolute.name

  ⍟ I've added these definitions:

    .an.absolute.name : Nat

scratch/main> view .an.absolute.name

  .an.absolute.name : Nat
  .an.absolute.name = 5
```
