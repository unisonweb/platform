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
scratch/main> add
scratch/main> io.test casTest
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

``` ucm
scratch/main> add
scratch/main> io.test promiseSequentialTest
scratch/main> io.test promiseConcurrentTest
```

CAS can be used to write an atomic update function.

``` unison
atomicUpdate : Ref {IO} a -> (a -> a) ->{IO} ()
atomicUpdate ref f =
  ticket = Ref.readForCas ref
  value = f (Ticket.read ticket)
  if Ref.cas ref ticket value then () else atomicUpdate ref f
```

``` ucm
scratch/main> add
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
``` ucm
scratch/main> add
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

``` ucm
scratch/main> add
scratch/main> io.test fullTest
```
