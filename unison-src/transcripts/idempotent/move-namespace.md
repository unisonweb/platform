# Tests for `move.namespace`

## Moving the Root

I should be able to move the root into a sub-namespace

``` unison :hide
foo = 1
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo : ##Nat

-- Should request confirmation

scratch/main> move.namespace . .root.at.path

  ⚠️

  Moves which affect the root branch cannot be undone, are you sure?
  Re-run the same command to proceed.

scratch/main> move.namespace . .root.at.path

  Done.

scratch/main> ls

  1. root/ (1 term)

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #g97lh1m2v7 (start of history)
```

``` ucm
scratch/main> ls .root.at.path

  1. foo (##Nat)

scratch/main> history .root.at.path

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #08a6hgi6s4 (start of history)
```

I should be able to move a sub namespace *over* the root.

``` ucm
-- Should request confirmation

scratch/main> move.namespace .root.at.path .

  ⚠️

  Moves which affect the root branch cannot be undone, are you sure?
  Re-run the same command to proceed.

scratch/main> move.namespace .root.at.path .

  Done.

scratch/main> ls

  1. foo (##Nat)

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #08a6hgi6s4 (start of history)
```

``` ucm :error
-- should be empty

scratch/main> ls .root.at.path

  nothing to show

scratch/main> history .root.at.path

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #sg60bvjo91 (start of history)
```

``` ucm :hide
scratch/happy> builtins.merge lib.builtins
```

## Happy path

Create a namespace and add some history to it

``` unison
a.termInA = 1
unique type a.T = T
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type a.T
      a.termInA : Nat
```

``` ucm
scratch/happy> add

  ⍟ I've added these definitions:

    type a.T
    a.termInA : Nat
```

``` unison
a.termInA = 2
unique type a.T = T1 | T2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type a.T
      a.termInA : Nat
```

``` ucm
scratch/happy> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Should be able to move the namespace, including its types, terms, and sub-namespaces.

``` ucm
scratch/happy> move.namespace a b

  Done.

scratch/happy> ls b

  1. T       (type)
  2. T/      (2 terms)
  3. termInA (Nat)

scratch/happy> history b

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #ugqniosnp0

    + Adds / updates:
    
      T T.T1 T.T2 termInA
    
    - Deletes:
    
      T.T

  □ 2. #a7r726o5ut (start of history)
```

## Namespace history

``` ucm :hide
scratch/history> builtins.merge lib.builtins
```

Create some namespaces and add some history to them

``` unison
a.termInA = 1
b.termInB = 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a.termInA : Nat
      b.termInB : Nat
```

``` ucm
scratch/history> add

  ⍟ I've added these definitions:

    a.termInA : Nat
    b.termInB : Nat
```

``` unison
a.termInA = 2
b.termInB = 11
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      a.termInA : Nat
      b.termInB : Nat
```

``` ucm
scratch/history> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Deleting a namespace should not leave behind any history,
if we move another to that location we expect the history to simply be the history
of the moved namespace.

``` ucm
scratch/history> delete.namespace b

  Done.

scratch/history> move.namespace a b

  Done.

-- Should be the history from 'a'

scratch/history> history b

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #j0cjjqepb3

    + Adds / updates:
    
      termInA

  □ 2. #m8smmmgjso (start of history)

-- Should be empty

scratch/history> history a

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #sg60bvjo91 (start of history)
```

## Moving over an existing branch

``` ucm :hide
scratch/existing> builtins.merge lib.builtins
```

Create some namespace and add some history to them

``` unison
a.termInA = 1
b.termInB = 10
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      a.termInA : Nat
      b.termInB : Nat
```

``` ucm
scratch/existing> add

  ⍟ I've added these definitions:

    a.termInA : Nat
    b.termInB : Nat
```

``` unison
a.termInA = 2
b.termInB = 11
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      a.termInA : Nat
      b.termInB : Nat
```

``` ucm
scratch/existing> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/existing> move.namespace a b

  ⚠️

  A branch existed at the destination: b so I over-wrote it.

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

  Done.
```
