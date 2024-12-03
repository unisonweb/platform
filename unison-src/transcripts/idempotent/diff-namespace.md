``` ucm :hide
scratch/b1> builtins.merge lib.builtins

scratch/b2> builtins.merge lib.builtins

scratch/nsx> builtins.merge lib.builtins

scratch/main> builtins.merge lib.builtins

scratch/ns1> builtins.merge lib.builtins
```

``` unison :hide
x = 23
fslkdjflskdjflksjdf = 663
```

``` ucm
scratch/b1> add

  ⍟ I've added these definitions:

    fslkdjflskdjflksjdf : Nat
    x                   : Nat
```

``` unison :hide
x = 23
fslkdjflskdjflksjdf = 23
abc = 23
```

``` ucm
scratch/b2> add

  ⍟ I've added these definitions:

    abc                 : Nat
    fslkdjflskdjflksjdf : Nat
    x                   : Nat

scratch/b1> debug.alias.term.force .x .fslkdjflskdjflksjdf

  Done.
```

``` ucm
scratch/main> diff.namespace /b1: /b2:

  Resolved name conflicts:

    1. ┌ fslkdjflskdjflksjdf#sekb3fdsvb : Nat
    2. └ fslkdjflskdjflksjdf#u520d1t9kc : Nat
       ↓
    3. fslkdjflskdjflksjdf#u520d1t9kc : Nat

  Name changes:

    Original                             Changes
    4. x                              ┐  5. abc (added)
    6. fslkdjflskdjflksjdf#u520d1t9kc ┘  7. fslkdjflskdjflksjdf (added)
                                         8. fslkdjflskdjflksjdf#u520d1t9kc (removed)
```

Things we want to test:

  - Diffing identical namespaces
  - Adds, removes, updates
      - Adds with multiple names
  - Moved and copied definitions
      - Moves that have more that 1 initial or final name
  - ... terms and types
  - New patches, modified patches, deleted patches, moved patches
  - With and without propagated updates

``` unison :hide
fromJust = 1
b = 2
bdependent = b
c = 3
helloWorld = "Hello, world!"

structural type A a = A ()
structural ability X a1 a2 where x : ()
```

``` ucm
scratch/ns1> add

  ⍟ I've added these definitions:

    structural type A a
    structural ability X a1 a2
    b          : Nat
    bdependent : Nat
    c          : Nat
    fromJust   : Nat
    helloWorld : Text

scratch/ns1> alias.term fromJust fromJust'

  Done.

scratch/ns1> alias.term helloWorld helloWorld2

  Done.

scratch/ns1> branch /ns2

  Done. I've created the ns2 branch based off of ns1.

  Tip: To merge your work back into the ns1 branch, first
       `switch /ns1` then `merge /ns2`.
```

Here's what we've done so far:

``` ucm :error
scratch/main> diff.namespace .nothing /ns1:

  ⚠️

  The namespace scratch/main:.nothing is empty. Was there a typo?
```

``` ucm :error
scratch/main> diff.namespace /ns1: /ns2:

  The namespaces are identical.
```

``` unison :hide
junk = "asldkfjasldkfj"
```

``` ucm
scratch/ns1> add

  ⍟ I've added these definitions:

    junk : Text

scratch/ns1> debug.alias.term.force junk fromJust

  Done.

scratch/ns1> delete.term junk

  Done.
```

``` unison :hide
fromJust = 99
b = 999999999
d = 4
e = 5
f = 6
unique type Y a b = Y a b
```

``` ucm
scratch/ns2> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> diff.namespace /ns1: /ns2:

  Resolved name conflicts:

    1.  ┌ fromJust#gjmq673r1v : Nat
    2.  └ fromJust#rnbo52q2sh : Text
        ↓
    3.  fromJust#6gn1k53ie0 : Nat

  Updates:

    4.  b : Nat
        ↓
    5.  b : Nat
    
    6.  bdependent : Nat
        ↓
    7.  bdependent : Nat

  Added definitions:

    8.  type Y a b
    9.  Y.Y : a -> b -> Y a b
    10. d   : Nat
    11. e   : Nat
    12. f   : Nat

  Name changes:

    Original                   Changes
    13. fromJust'           ┐  14. fromJust#gjmq673r1v (removed)
    15. fromJust#gjmq673r1v ┘  

scratch/ns2> alias.term d d'

  Done.

scratch/ns2> alias.type A A'

  Done.

scratch/ns2> alias.term A.A A'.A

  Done.

scratch/ns2> alias.type X X'

  Done.

scratch/ns2> alias.term X.x X'.x

  Done.

scratch/main> diff.namespace /ns1: /ns2:

  Resolved name conflicts:

    1.  ┌ fromJust#gjmq673r1v : Nat
    2.  └ fromJust#rnbo52q2sh : Text
        ↓
    3.  fromJust#6gn1k53ie0 : Nat

  Updates:

    4.  b : Nat
        ↓
    5.  b : Nat
    
    6.  bdependent : Nat
        ↓
    7.  bdependent : Nat

  Added definitions:

    8.  type Y a b
    9.  Y.Y  : a -> b -> Y a b
    10. ┌ d  : Nat
    11. └ d' : Nat
    12. e    : Nat
    13. f    : Nat

  Name changes:

    Original                   Changes
    14. A                      15. A' (added)
    
    16. X                      17. X' (added)
    
    18. A.A                    19. A'.A (added)
    
    20. fromJust'           ┐  21. fromJust#gjmq673r1v (removed)
    22. fromJust#gjmq673r1v ┘  
    
    23. X.x                    24. X'.x (added)

scratch/ns1> alias.type X X2

  Done.

scratch/ns1> alias.term X.x X2.x

  Done.

scratch/ns2> alias.type A' A''

  Done.

scratch/ns2> alias.term A'.A A''.A

  Done.

scratch/ns2> branch /ns3

  Done. I've created the ns3 branch based off of ns2.

  Tip: To merge your work back into the ns2 branch, first
       `switch /ns2` then `merge /ns3`.

scratch/ns2> alias.term fromJust' yoohoo

  Done.

scratch/ns2> delete.term.verbose fromJust'

  Name changes:

    Original        Changes
    1. fromJust' ┐  2. fromJust' (removed)
    3. yoohoo    ┘  

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> diff.namespace /ns3: /ns2:

  Name changes:

    Original        Changes
    1. fromJust'    2. yoohoo (added)
                    3. fromJust' (removed)
```

``` unison :hide
bdependent = "banana"
```

``` ucm
scratch/ns3> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> diff.namespace /ns2: /ns3:

  Updates:

    1. bdependent : Nat
       ↓
    2. bdependent : Text

  Name changes:

    Original     Changes
    3. yoohoo    4. fromJust' (added)
                 5. yoohoo (removed)
```

## Two different auto-propagated changes creating a name conflict

Currently, the auto-propagated name-conflicted definitions are not explicitly
shown, only their also-conflicted dependency is shown.

``` unison :hide
a = 333
b = a + 1

forconflicts = 777
```

``` ucm
scratch/nsx> add

  ⍟ I've added these definitions:

    a            : Nat
    b            : Nat
    forconflicts : Nat

scratch/nsx> branch /nsy

  Done. I've created the nsy branch based off of nsx.

  Tip: To merge your work back into the nsx branch, first
       `switch /nsx` then `merge /nsy`.

scratch/nsx> branch /nsz

  Done. I've created the nsz branch based off of nsx.

  Tip: To merge your work back into the nsx branch, first
       `switch /nsx` then `merge /nsz`.
```

``` unison :hide
a = 444
```

``` ucm
scratch/nsy> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

``` unison :hide
a = 555
```

``` ucm
scratch/nsz> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/nsy> branch /nsw

  Done. I've created the nsw branch based off of nsy.

  Tip: To merge your work back into the nsy branch, first
       `switch /nsy` then `merge /nsw`.

scratch/nsw> debug.alias.term.force .forconflicts .a

  Done.

scratch/nsw> debug.alias.term.force .forconflicts .b

  Done.
```

``` ucm
scratch/main> diff.namespace /nsx: /nsw:

  New name conflicts:

    1. a#uiiiv8a86s : Nat
       ↓
    2. ┌ a#mdl4vqtu00 : Nat
    3. └ a#r3msrbpp1v : Nat
    
    4. b#lhigeb1let : Nat
       ↓
    5. ┌ b#r3msrbpp1v : Nat
    6. └ b#unkqhuu66p : Nat

  Name changes:

    Original           Changes
    7. forconflicts    8. a#r3msrbpp1v (added)
                       9. b#r3msrbpp1v (added)

scratch/nsw> view a

  a#mdl4vqtu00 : Nat
  a#mdl4vqtu00 = 444

  a#r3msrbpp1v : Nat
  a#r3msrbpp1v = 777

scratch/nsw> view b

  b#r3msrbpp1v : Nat
  b#r3msrbpp1v = 777

  b#unkqhuu66p : Nat
  b#unkqhuu66p =
    use Nat +
    a#mdl4vqtu00 + 1
```

## Should be able to diff a namespace hash from history.

``` unison
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      x : Nat
```

``` ucm
scratch/hashdiff> add

  ⍟ I've added these definitions:

    x : ##Nat
```

``` unison
y = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      y : ##Nat
```

``` ucm
scratch/hashdiff> add

  ⍟ I've added these definitions:

    y : ##Nat

scratch/hashdiff> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ 1. #ru1hnjofdj

    + Adds / updates:
    
      y

  □ 2. #i52j9fd57b (start of history)

scratch/hashdiff> diff.namespace 2 1

  Added definitions:

    1. y : ##Nat
```

## 

Updates:  -- 1 to 1

New name conflicts: -- updates where RHS has multiple hashes (excluding when RHS=LHS)

1.  foo\#jk19sm5bf8 : Nat - do we want to force a hashqualified? Arya thinks so
    ↓
2.  ┌ foo\#0ja1qfpej6 : Nat
3.  └ foo\#jk19sm5bf8 : Nat

Resolved name conflicts: -- updates where LHS had multiple hashes and RHS has one

4.  ┌ bar\#0ja1qfpej6 : Nat
5.  └ bar\#jk19sm5bf8 : Nat
    ↓
6.  bar\#jk19sm5bf8 : Nat

## Display issues to fixup

  - \[d\] Do we want to surface new edit conflicts in patches?
  - \[t\] two different auto-propagated changes creating a name conflict should show
    up somewhere besides the auto-propagate count
  - \[t\] Things look screwy when the type signature doesn't fit and has to get broken
    up into multiple lines. Maybe just disallow that?
  - \[d\] Delete blank line in between copies / renames entries if all entries are 1 to 1
    see todo in the code
  - \[x\] incorrectly calculated bracket alignment on hashqualified "Name changes"  (delete.output.md)
  - \[x\] just handle deletion of isPropagated in propagate function, leave HandleInput alone (assuming this does the trick)
  - \[x\] might want unqualified names to be qualified sometimes:
  - \[x\] if a name is updated to a not-yet-named reference, it's shown as both an update and an add
  - \[x\] similarly, if a conflicted name is resolved by deleting the last name to
    a reference, I (arya) suspect it will show up as a Remove
  - \[d\] Maybe group and/or add headings to the types, constructors, terms
  - \[x\] add tagging of propagated updates to test propagated updates output
  - \[x\] missing old names in deletion ppe (delete.output.md)  (superseded by \#1143)
  - \[x\] delete.term has some bonkers output
  - \[x\] Make a decision about how we want to show constructors in the diff
  - \[x\] 12.patch patch needs a space
  - \[x\] This looks like garbage
  - \[x\] Extra 2 blank lines at the end of the add section
  - \[x\] Fix alignment issues with buildTable, convert to column3M (to be written)
  - \[x\] adding an alias is showing up as an Add and a Copy; should just show as Copy
  - \[x\] removing one of multiple aliases appears in removes + moves + copies section
  - \[x\] some overlapping cases between Moves and Copies^
  - \[x\] Maybe don't list the type signature twice for aliases?
