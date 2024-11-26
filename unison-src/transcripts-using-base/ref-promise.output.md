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

``` ucm :added-by-ucm
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

    1. casTest   ◉ CAS is successful is there were no conflicting writes
                 ◉ CAS fails when there was an intervening write

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Promise is a simple one-shot awaitable condition.

``` unison
promiseSequentialTest : '{IO} [Result]
promiseSequentialTest = do
  test = do
    use Nat eq
    use Promise read write
    p = !Promise.new
    write p 0 |> void
    v1 = read p
    check "Should read a value that's been written" (eq v1 0)
    write p 1 |> void
    v2 = read p
    check "Promise can only be written to once" (eq v2 0)

  runTest test

promiseConcurrentTest : '{IO} [Result]
promiseConcurrentTest = do
  use Nat eq
  test = do
    p = !Promise.new
    _ = forkComp '(Promise.write p 5)
    v = Promise.read p
    check "Reads awaits for completion of the Promise" (eq v 5)

  runTest test
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      promiseConcurrentTest : '{IO} [Result]
      promiseSequentialTest : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    promiseConcurrentTest : '{IO} [Result]
    promiseSequentialTest : '{IO} [Result]

scratch/main> io.test promiseSequentialTest

    New test results:

    1. promiseSequentialTest   ◉ Should read a value that's been written
                               ◉ Promise can only be written to once

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> io.test promiseConcurrentTest

    New test results:

    1. promiseConcurrentTest   ◉ Reads awaits for completion of the Promise

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

CAS can be used to write an atomic update function.

``` unison
atomicUpdate : Ref {IO} a -> (a -> a) ->{IO} ()
atomicUpdate ref f =
  ticket = Ref.readForCas ref
  value = f (Ticket.read ticket)
  if Ref.cas ref ticket value then () else atomicUpdate ref f
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      atomicUpdate : Ref {IO} a -> (a -> a) ->{IO} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    atomicUpdate : Ref {IO} a -> (a -> a) ->{IO} ()
```

Promise can be used to write an operation that spawns N concurrent
tasks and collects their results

``` unison
spawnN : Nat -> '{IO} a ->{IO} [a]
spawnN n fa =
  use Nat eq drop
  go i acc =
    if eq i 0
    then acc
    else
      value = !Promise.new
      _ = forkComp do Promise.write value !fa
      go (drop i 1) (acc :+ value)

  map Promise.read (go n [])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      spawnN : Nat -> '{IO} a ->{IO} [a]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    spawnN : Nat -> '{IO} a ->{IO} [a]
```

We can use these primitives to write a more interesting example, where
multiple threads repeatedly update an atomic counter, we check that
the value of the counter is correct after all threads are done.

``` unison
fullTest : '{IO} [Result]
fullTest = do
  use Nat * + eq drop

  numThreads = 100
  iterations = 100
  expected = numThreads * iterations

  test = do
    state = IO.ref 0
    thread n =
      if eq n 0
      then ()
      else
        atomicUpdate state (v -> v + 1)
        thread (drop n 1)
    void (spawnN numThreads '(thread iterations))
    result = Ref.read state
    check "The state of the counter is consistent "(eq result expected)

  runTest test
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      fullTest : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    fullTest : '{IO} [Result]

scratch/main> io.test fullTest

    New test results:

    1. fullTest   ◉ The state of the counter is consistent 

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
