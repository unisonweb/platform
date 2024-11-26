# Undo

Undo should pop a node off of the history of the current branch.

``` unison :hide
x = 1
```

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.

scratch/main> add

  ⍟ I've added these definitions:

    x : Nat

scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> alias.term x y

  Done.

scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)
  3. y    (Nat)

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #nmem6r6no1

    + Adds / updates:
    
      y
    
    = Copies:
    
      Original name New name(s)
      x             y

  ⊙ 2. #3rqf1hbev7

    + Adds / updates:
    
      x

  □ 3. #ms9lggs2rg (start of history)

scratch/main> undo

  Here are the changes I undid

  Name changes:

    Original  Changes
    1. x      2. y (added)

scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #3rqf1hbev7

    + Adds / updates:
    
      x

  □ 2. #ms9lggs2rg (start of history)
```

-----

It should not be affected by changes on other branches.

``` unison :hide
x = 1
```

``` ucm
scratch/branch1> builtins.merge lib.builtins

  Done.

scratch/branch1> add

  ⍟ I've added these definitions:

    x : Nat

scratch/branch1> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/branch1> alias.term x y

  Done.

scratch/branch1> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)
  3. y    (Nat)

scratch/branch1> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #nmem6r6no1

    + Adds / updates:
    
      y
    
    = Copies:
    
      Original name New name(s)
      x             y

  ⊙ 2. #3rqf1hbev7

    + Adds / updates:
    
      x

  □ 3. #ms9lggs2rg (start of history)

-- Make some changes on an unrelated branch

scratch/branch2> builtins.merge lib.builtins

  Done.

scratch/branch2> delete.namespace lib

  Done.

scratch/branch1> undo

  Here are the changes I undid

  Name changes:

    Original  Changes
    1. x      2. y (added)

scratch/branch1> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/branch1> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #3rqf1hbev7

    + Adds / updates:
    
      x

  □ 2. #ms9lggs2rg (start of history)
```

-----

Undo should be a no-op on a newly created branch

``` ucm :error
scratch/main> branch.create-empty new

  Done. I've created an empty branch scratch/new.

  Tip: Use `merge /somebranch` to initialize this branch.

scratch/new> undo

  ⚠️

  Nothing more to undo.
```
