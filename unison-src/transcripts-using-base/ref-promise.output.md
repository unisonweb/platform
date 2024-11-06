# tests for Promise and CAS on Refs

Ref support a CAS operation that can be used as a building block to
change state atomically without locks.

``` unison
casTest: '{io2.IO} [Result]
casTest = do
  test = do
    ref = IO.ref 0
    ticket = Ref.readForCas ref
    v1 = Ref.cas ref ticket 5
    check "CAS is successful is there were no conflicting writes" v1
    Ref.write ref 10
    v2 = Ref.cas ref ticket 15
    check "CAS fails when there was an intervening write" (not v2)

  runTest test
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      casTest : '{IO} [Result]

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    casTest : '{IO} [Result]

scratch/main> io.test casTest

    New test results:
  
    1. casTest   ◉ CAS fails when there was an intervening write
  
    2. casTest   ✗ CAS is successful is there were no conflicting writes
  
  🚫 1 test(s) failing, ✅ 1 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

```



🛑

The transcript failed due to an error in the stanza above. The error is:


    New test results:
  
    1. casTest   ◉ CAS fails when there was an intervening write
  
    2. casTest   ✗ CAS is successful is there were no conflicting writes
  
  🚫 1 test(s) failing, ✅ 1 test(s) passing
  
  Tip: Use view 1 to view the source of a test.

