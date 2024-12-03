Lets just make sure we can start a thread

``` unison
otherThread : '{io2.IO}()
otherThread = 'let
  watch "I'm the other Thread" ()

testBasicFork : '{io2.IO} [Result]
testBasicFork = 'let
  test = 'let
    watch "I'm the parent thread" ()
    threadId = .builtin.io2.IO.forkComp otherThread
    emit (Ok "created thread")

  runTest test

```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      otherThread   : '{IO} ()
      testBasicFork : '{IO} [Result]
```

See if we can get another thread to stuff a value into a MVar

``` ucm :hide
scratch/main> add

scratch/main> io.test testBasicFork
```

``` unison
thread1 : Nat -> MVar Nat -> '{io2.IO}()
thread1 x mv = 'let
  go = 'let
    put mv (increment x)

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()


testBasicMultiThreadMVar : '{io2.IO} [Result]
testBasicMultiThreadMVar = 'let
  test = 'let
    mv = !newEmpty
    void (forkComp (thread1 10 mv))
    next = take mv
    expectU "other thread should have incremented" 11 next

  runTest test


```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      testBasicMultiThreadMVar : '{IO} [Result]
      thread1                  : Nat -> MVar Nat -> '{IO} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    testBasicMultiThreadMVar : '{IO} [Result]
    thread1                  : Nat -> MVar Nat -> '{IO} ()

scratch/main> io.test testBasicMultiThreadMVar

    New test results:

    1. testBasicMultiThreadMVar   ◉ other thread should have incremented

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

``` unison
sendingThread: Nat -> MVar Nat -> '{io2.IO}()
sendingThread toSend mv = 'let
  go = 'let
    put mv (increment toSend)

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()


receivingThread: MVar Nat -> MVar Text -> '{io2.IO}()
receivingThread recv send = 'let
  go = 'let
    recvd = take recv
    put send (toText recvd)

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()

testTwoThreads: '{io2.IO}[Result]
testTwoThreads = 'let
  test = 'let
    send = !MVar.newEmpty
    recv = !MVar.newEmpty

    void (forkComp (sendingThread 6 send))
    void (forkComp (receivingThread send recv))

    recvd = take recv

    expectU "" "7" recvd

  runTest test

```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      receivingThread : MVar Nat -> MVar Text -> '{IO} ()
      sendingThread   : Nat -> MVar Nat -> '{IO} ()
        (also named thread1)
      testTwoThreads  : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    receivingThread : MVar Nat -> MVar Text -> '{IO} ()
    sendingThread   : Nat -> MVar Nat -> '{IO} ()
      (also named thread1)
    testTwoThreads  : '{IO} [Result]

scratch/main> io.test testTwoThreads

    New test results:

    1. testTwoThreads   ◉ 

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
