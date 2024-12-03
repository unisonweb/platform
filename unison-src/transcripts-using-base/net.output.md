``` unison :hide
serverSocket = compose2 reraise IO.serverSocket.impl
socketPort = compose reraise socketPort.impl
listen = compose reraise listen.impl
closeSocket = compose reraise closeSocket.impl
clientSocket = compose2 reraise IO.clientSocket.impl
socketSend = compose2 reraise socketSend.impl
socketReceive = compose2 reraise socketReceive.impl
socketAccept = compose reraise socketAccept.impl
```

``` ucm :hide
scratch/main> add
```

# Tests for network related builtins

### Creating server sockets

This section tests functions in the IO builtin related to binding to
TCP server socket, as to be able to accept incoming TCP connections.

``` 
    builtin.io2.IO.serverSocket : Optional Text -> Text ->{io2.IO} Either Failure io2.Socket
```

This function takes two parameters, The first is the Hostname. If None
is provided, We will attempt to bind to 0.0.0.0 (All ipv4
addresses). We currently only support IPV4 (we should fix this\!)
The second is the name of the port to bind to. This can be
a decimal representation of a port number between 1-65535. This can be
a named port like "ssh" (for port 22) or "kermit" (for port 1649),
This mapping of names to port numbers is maintained by the [nsswitch
service](https://en.wikipedia.org/wiki/Name_Service_Switch), typically
stored in `/etc/services` and queried with the `getent` tool:

``` 
# map number to name
$ getent services 22
ssh                   22/tcp

# map name to number
$ getent services finger
finger                79/tcp

# get a list of all known names
$ getent services | head
tcpmux                1/tcp
echo                  7/tcp
echo                  7/udp
discard               9/tcp sink null
discard               9/udp sink null
systat                11/tcp users
daytime               13/tcp
daytime               13/udp
netstat               15/tcp
qotd                  17/tcp quote
```

Below shows different examples of how we might specify the server coordinates.

``` unison
testExplicitHost : '{io2.IO} [Result]
testExplicitHost _ =
  test = 'let
    sock = serverSocket (Some "127.0.0.1") "1028"
    emit (Ok "successfully created socket")
    port = socketPort sock
    putBytes (stdHandle StdOut) (toUtf8 (toText port))
    expectU "should have bound to port 1028" 1028 port

  runTest test

testDefaultHost : '{io2.IO} [Result]
testDefaultHost _ =
  test = 'let
    sock = serverSocket None "1028"
    emit (Ok "successfully created socket")
    port = socketPort sock
    putBytes (stdHandle StdOut) (toUtf8 (toText port))
    expectU "should have bound to port 1028" 1028 port

  runTest test

testDefaultPort : '{io2.IO} [Result]
testDefaultPort _ =
  test = 'let
    sock = serverSocket None "0"
    emit (Ok "successfully created socket")
    port = socketPort sock
    putBytes (stdHandle StdOut) (toUtf8 (toText port))

    check "port should be > 1024" (1024 < port)
    check "port should be < 65536" (65536 > port)

  runTest test
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      testDefaultHost  : '{IO} [Result]
      testDefaultPort  : '{IO} [Result]
      testExplicitHost : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    testDefaultHost  : '{IO} [Result]
    testDefaultPort  : '{IO} [Result]
    testExplicitHost : '{IO} [Result]

scratch/main> io.test testDefaultPort

    New test results:

    1. testDefaultPort   ◉ successfully created socket
                         ◉ port should be > 1024
                         ◉ port should be < 65536

  ✅ 3 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

This example demonstrates connecting a TCP client socket to a TCP server socket. A thread is started for both client and server. The server socket asks for any availalbe port (by passing "0" as the port number). The server thread then queries for the actual assigned port number, and puts that into an MVar which the client thread can read. The client thread then reads a string from the server and reports it back to the main thread via a different MVar.

``` unison

serverThread: MVar Nat -> Text -> '{io2.IO}()
serverThread portVar toSend = 'let
  go : '{io2.IO, Exception}()
  go = 'let
    sock = serverSocket (Some "127.0.0.1") "0"
    port = socketPort sock
    put portVar port
    listen sock
    sock' = socketAccept sock
    socketSend sock' (toUtf8 toSend)
    closeSocket sock'

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()

clientThread : MVar Nat -> MVar Text -> '{io2.IO}()
clientThread portVar resultVar = 'let
  go = 'let
    port = take portVar
    sock = clientSocket "127.0.0.1" (Nat.toText port)
    msg = fromUtf8 (socketReceive sock 100)
    put resultVar msg

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()

testTcpConnect : '{io2.IO}[Result]
testTcpConnect = 'let
  test = 'let
    portVar = !MVar.newEmpty
    resultVar = !MVar.newEmpty

    toSend = "12345"

    void (forkComp (serverThread portVar toSend))
    void (forkComp (clientThread portVar resultVar))

    received = take resultVar

    expectU "should have reaped what we've sown" toSend received

  runTest test

```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      clientThread   : MVar Nat -> MVar Text -> '{IO} ()
      serverThread   : MVar Nat -> Text -> '{IO} ()
      testTcpConnect : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    clientThread   : MVar Nat -> MVar Text -> '{IO} ()
    serverThread   : MVar Nat -> Text -> '{IO} ()
    testTcpConnect : '{IO} [Result]

scratch/main> io.test testTcpConnect

    New test results:

    1. testTcpConnect   ◉ should have reaped what we've sown

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
