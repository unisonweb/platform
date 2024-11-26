# Tests for `move`

``` ucm :hide
scratch/main> builtins.merge
```

## Happy Path - namespace, term, and type

Create a term, type, and namespace with history

``` unison
Foo = 2
unique type Foo = Foo
Foo.termInA = 1
unique type Foo.T = T
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      type Foo.T
      Foo         : Nat
      Foo.termInA : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    type Foo.T
    Foo         : Nat
    Foo.termInA : Nat
```

``` unison
Foo.termInA = 2
unique type Foo.T = T1 | T2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo.T
      Foo.termInA : Nat
        (also named Foo)
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Should be able to move the term, type, and namespace, including its types, terms, and sub-namespaces.

``` ucm
scratch/main> move Foo Bar

  Done.

scratch/main> ls

  1. Bar      (Nat)
  2. Bar      (type)
  3. Bar/     (4 terms, 1 type)
  4. builtin/ (469 terms, 74 types)

scratch/main> ls Bar

  1. Foo     (Bar)
  2. T       (type)
  3. T/      (2 terms)
  4. termInA (Nat)

scratch/main> history Bar

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #o7vuviel4c

    + Adds / updates:
    
      T T.T1 T.T2 termInA
    
    - Deletes:
    
      T.T

  □ 2. #c5cggiaumo (start of history)
```

## Happy Path - Just term

``` unison
bonk = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bonk : Nat
```

``` ucm
z/main> builtins.merge

  Done.

z/main> add

  ⍟ I've added these definitions:

    bonk : Nat

z/main> move bonk zonk

  Done.

z/main> ls

  1. builtin/ (469 terms, 74 types)
  2. zonk     (Nat)
```

## Happy Path - Just namespace

``` unison
bonk.zonk = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bonk.zonk : Nat
        (also named zonk)
```

``` ucm
a/main> builtins.merge

  Done.

a/main> add

  ⍟ I've added these definitions:

    bonk.zonk : Nat

a/main> move bonk zonk

  Done.

a/main> ls

  1. builtin/ (469 terms, 74 types)
  2. zonk/    (1 term)

a/main> view zonk.zonk

  zonk.zonk : Nat
  zonk.zonk = 5
```

## Sad Path - No term, type, or namespace named src

``` ucm :error
scratch/main> move doesntexist foo

  ⚠️

  There is no term, type, or namespace at doesntexist.
```
