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

``` ucm
scratch/main> add
scratch/main> io.test testBasicMultiThreadMVar
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

``` ucm
scratch/main> add
scratch/main> io.test testTwoThreads
```
