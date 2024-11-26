# Tests for TLS builtins

``` unison :hide
-- generated with:
-- openssl req -newkey rsa:2048 -subj '/CN=test.unison.cloud/O=Unison/C=US' -nodes -keyout key.pem -x509 -days 3650 -out cert.pem

self_signed_key_pem="-----BEGIN PRIVATE KEY-----\nMIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQDtV0Lqk9i5DKJG\ne5zwDFuxHxSxhOygCuq6Jl4xonsjl4hdvXxUUiuqxGGbv4x9HSvavpHwyriGiIRQ\noIjanWiNK9Jp6VDYWOvErnTG/+Rfm1vCoUKQvn8lDrD9knSPUoTz3Cz7JS8FE/rr\nFR3IRyXa0gpXmvIwX16SeCS/Lb/Le9o1HJh9DrkxVyoFq3zlX1OE0AVV0a014IDB\nNprqLITwiVzyDPQkP8rbJF9WPI5afzW8+3+v5UanIFknOOPaJl8pf3gmqI5g8fxk\n/SSMlPgnLd1Fi7h90gBygdpJw3do3/ZA1IOvmFQ+LXE1xtqU1Ay3f3At3DiJgTxP\n8mwBYdtdAgMBAAECggEBAMo85QRF3xIvtcchZeUWYrtWpKdvgMIPC1x7fSAGN69o\nXAakg+DF8/ebRyET435o8QmAAZOQ6hOZGEYrxPGj14cTpEQjT4RKoPwDO/al7c+Z\n7mK2TqZP7L+C+UXZGgFWa3vwTVPjp2FIWTMf1zTli1geSjnECkM1wLxGK+nL7fZQ\nesHXPkJJG5AqzA84bJ/fY5OQ/dfcCxnHEv5XpHPq6VFgXg7jtcNbr1R9EBiQfreN\nU7Hd38R77jYjL1fT71HwEUQ0cwavfxTu0jZFXJxEC7CC1J65QXUguZXLf9vwgSB0\nm0gZgeJlQ905bDJrxUcqCFxdROy/SndP6qFnJSCsfwECgYEA+2cld/WCieUGstJd\njsIrJ6f/e+uuOSTnGTtnsBX6KoiHdcg3sVVVK18xI9El9V+YX9SjN37XeGFe/Wzu\ngE3M4A3Jqz7cgdNj/PaKjqQwJWNbcJnL5ku6eQvcAIpc5gAZxXVCPIbY1ZpeYcsh\nMwr3cOEpQu8UVFBbn/OeJ1r07dECgYEA8a5J3Ls5PSxXq8NDrkAxt3vUJIWLGQQJ\nbV2aGDI2XP2N+vh2WML9rlFeyyBOeRxK9TsErVOaEeOcQZV97//fzIGxCU+SXyiC\nnVMXT2U1mzOu5qPfzLO5Ga4sunxqKDman6NM2IPw2NPA7zMWNQMEIHAerwYZzjm5\nB5tFcMA8e80CgYBgF8rwkTz2LD5lN5dfK8SHAeXbnfgYC4zxzg0R9zSJ8WmlkYQI\nGk/VpisIP7c8lO+PIZ3JZohBkSZXw71d+V7n/R0qgXqTfRNo62uGnidxAws+fOq8\n+hEql2feJQThPQScvvc0X26eJsUQqC3mbripwsacuPmSSKzc9Kds741TIQKBgQCd\nXnG2CytATAliTKlbY218HmOKzHJAfcJttk9KhhekAW5cB0F4lq98vHtPJOA0OFoO\nyLlI63EdSOpMQj1Y83IUxjYy699Rmx1BuAMrral0P/kZMYfe0QAsWp/BZpXxT2EB\npeG58l/3sBqnJsrFBgu/24H/UaeoAyoaa96Rhntb2QKBgQCSEkcUnzTvoUyMFN14\n8NttxOUZiSsCmgoXk6Rk2QKyCPsJocGS4BffGt3kOMcotz/0YsvM1TBBLB7vIaAy\nE1eWLBxK4yYeS8dKXwiCZn170yaJyjoBwZC1RgqQiKa5Y22Di7KjJoMa4Da8Tk4z\nFbE5dBApbLhvNTyQ7BHZxlfmdg==\n-----END PRIVATE KEY-----"

self_signed_cert_pem2 = "-----BEGIN CERTIFICATE-----\nMIIDVTCCAj2gAwIBAgIUdMNT5sYMfDJYH48Rh8LrlN+5wwgwDQYJKoZIhvcNAQEL\nBQAwOjEaMBgGA1UEAwwRdGVzdC51bmlzb24uY2xvdWQxDzANBgNVBAoMBlVuaXNv\nbjELMAkGA1UEBhMCVVMwHhcNMjIwMTI0MjAxNzQ2WhcNMzIwMTIyMjAxNzQ2WjA6\nMRowGAYDVQQDDBF0ZXN0LnVuaXNvbi5jbG91ZDEPMA0GA1UECgwGVW5pc29uMQsw\nCQYDVQQGEwJVUzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAO1XQuqT\n2LkMokZ7nPAMW7EfFLGE7KAK6romXjGieyOXiF29fFRSK6rEYZu/jH0dK9q+kfDK\nuIaIhFCgiNqdaI0r0mnpUNhY68SudMb/5F+bW8KhQpC+fyUOsP2SdI9ShPPcLPsl\nLwUT+usVHchHJdrSClea8jBfXpJ4JL8tv8t72jUcmH0OuTFXKgWrfOVfU4TQBVXR\nrTXggME2muoshPCJXPIM9CQ/ytskX1Y8jlp/Nbz7f6/lRqcgWSc449omXyl/eCao\njmDx/GT9JIyU+Cct3UWLuH3SAHKB2knDd2jf9kDUg6+YVD4tcTXG2pTUDLd/cC3c\nOImBPE/ybAFh210CAwEAAaNTMFEwHQYDVR0OBBYEFIfwxpuqtqxfCpaJGW32jH2J\nNbnYMB8GA1UdIwQYMBaAFIfwxpuqtqxfCpaJGW32jH2JNbnYMA8GA1UdEwEB/wQF\nMAMBAf8wDQYJKoZIhvcNAQELBQADggEBAKh7EDo5XjSd6J190WGH3V8v49J0Sh8M\nP7APe1eL8eTkW1Vh7/QCOhRpkSnyCz2OxJjjeFVAsCO3aLxlRM6wQZQKXu45iM2U\niPmv7ECS5xUn7LqRZd/JG1P6jvRPtBC1+oqA+NNDe27wzQp3rWyDG3pWZga8jJfW\nq+2xQ+s6GfzszxYZ/8MLn4zaUSymnOA+70yQ8czXkSO7MT2jJ7QDX8jxuJPZZARW\nuXeAYPRqD+b4MjdBATEtxgPTDWEi8gtfHFGUgInFhD4hOu+D3NLiE6lfR5brUqpQ\nZ4v8prCI8OjGSUx1dIJhqQHB5O0vdaxO0hkVdfqDVE93UrGBPwBRDlo=\n-----END CERTIFICATE-----"

not_a_cert = "-----BEGIN SCHERMIFICATE-----\n-----END SCHERMIFICATE-----"
```

``` ucm :hide
scratch/main> add
```

# Using an alternative certificate store

First lets make sure we can load our cert and private key

``` unison
this_should_work=match (decodeCert.impl (toUtf8 self_signed_cert_pem2) with
  Left (Failure _ t _) -> [Fail t]
  Right _ -> [Ok "succesfully decoded self_signed_pem"]

this_should_not_work=match (decodeCert.impl (toUtf8 not_a_cert) with
  Left _ -> [Ok "failed"]
  Right _ -> [Fail "um, that was a schmificate"]

what_should_work _ = this_should_work ++ this_should_not_work
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      this_should_not_work : [Result]
      this_should_work     : [Result]
      what_should_work     : ∀ _. _ -> [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    this_should_not_work : [Result]
    this_should_work     : [Result]
    what_should_work     : ∀ _. _ -> [Result]

scratch/main> io.test what_should_work

    New test results:

    1. what_should_work   ◉ succesfully decoded self_signed_pem
                          ◉ failed

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Test handshaking a client/server a local TCP connection using our
self-signed cert.

We'll create a server and a client, and start threads for each.

The server will report the port it is bound to via a passed MVar which
the client can read.

``` unison
serverThread: MVar Nat -> Text -> '{io2.IO}()
serverThread portVar toSend = 'let
  go: '{io2.IO, Exception}()
  go = 'let
       -- load our self signed cert
    cert = decodeCert (toUtf8 self_signed_cert_pem2)

       -- assume there is exactly one key decoded from our Bytes
    key = match (decodePrivateKey (toUtf8 self_signed_key_pem)) with
      k +: _ -> k
      [] -> bug "oh no"

       -- create a default configuration using our credentials (certificate chain and key)
    tlsconfig = Tls.ServerConfig.default [cert] key

       -- Open a TCP server port:
    -- we pass the special port "0" to mean "please find us an open port"
    sock = serverSocket (Some "127.0.0.1") "0"

       -- find out what port we got
    port = socketPort sock

       -- report the port back so that the client knows where to connect
    put portVar port

       -- start listening to the socket so that it starts accepting connections
    listen sock

       -- accept a single connection on this socket
    sock' = socketAccept sock

       -- attach TLS to our TCP connection
    tls = newServer tlsconfig sock'

       -- try to handshake the TLS connection with the client
    reraise (handshake.impl tls)

       -- send our message over our tls channel
    send tls (toUtf8 toSend)
    terminate tls
    closeSocket sock'

  match (toEither go) with
    Left (Failure _ t _) -> watch ("error in server: " ++ t) ()
    _ -> watch "server finished" ()

testClient : Optional SignedCert -> Text -> MVar Nat -> '{io2.IO, Exception} Text
testClient cert hostname portVar _ =
  -- create a client that will expect a cert from the given hostname (CN)
  defaultClient = (Tls.ClientConfig.default hostname Bytes.empty)

  -- if we were passed a certificate to trust, it is the only certificate we trust
  tlsconfig = match cert with
    None        -> defaultClient
    Some (cert) -> defaultClient |> ClientConfig.certificates.set [cert]

  -- wait to find out what port the server started on
  port = take portVar

  -- create a tcp connection with the server

  sock = clientSocket "127.0.0.1" (Nat.toText port)

  -- attach the TLS client to the TCP socket
  tls = newClient tlsconfig sock

  -- verify that the server presents us with a certificate chain for
  -- test.unison.cloud originating with a certificate we trust, and
  -- that the server can use a compatible TLS version and cipher
  reraise (handshake.impl tls)

  -- receive a message from the server
  fromUtf8 (receive tls)

testConnectSelfSigned : '{io2.IO}[Result]
testConnectSelfSigned _ =
  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    tid = forkComp (serverThread portVar toSend)

    -- Client
    cert = decodeCert (toUtf8 self_signed_cert_pem2)
    received = !(testClient (Some cert) "test.unison.cloud" portVar)

    _ = kill.impl tid

    expectU "should have reaped what we've sown" toSend received


  runTest test

-- this client will trust whatever certs the system trusts
-- for signing certs. This should NOT trust the server
-- serving the self-signed cert, so both the client and
-- the server should have a failure during the handshake
testCAReject : '{io2.IO}[Result]
testCAReject _ =
  checkError : Either Failure a -> Result
  checkError = cases
    Right _ -> Fail "expected a handshake exception"
    Left (Failure _ t _) ->
      if contains "UnknownCa" t && contains "HandshakeFailed" t then Ok "correctly rejected self-signed cert" else
        Fail ("expected UnknownCa, got: " ++ t)

  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    tid = forkComp (serverThread portVar toSend)

    -- Client
    testClient None "test.unison.cloud" portVar |> toEither |> checkError |> emit

    kill.impl tid

  runTest test

-- this client will ask for a different hostname, and
-- therefore should fail during the handshake when the
-- server presents an cert with unexpected hostname
testCNReject : '{io2.IO}[Result]
testCNReject _ =
  checkError : Either Failure a -> Result
  checkError = cases
    Right _ -> Fail "expected a handshake exception"
    Left (Failure _ t _) -> if contains "NameMismatch" t && contains "HandshakeFailed" t then Ok "correctly rejected self-signed cert" else
        Fail ("expected UnknownCa, got: " ++ t)

  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    tid = forkComp (serverThread portVar toSend)


    -- Client
    testClient None "wrong.host.name" portVar |> toEither |> checkError |> emit

    kill.impl tid

  runTest test
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      serverThread          : MVar Nat -> Text -> '{IO} ()
      testCAReject          : '{IO} [Result]
      testCNReject          : '{IO} [Result]
      testClient            : Optional SignedCert
                              -> Text
                              -> MVar Nat
                              -> '{IO, Exception} Text
      testConnectSelfSigned : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    serverThread          : MVar Nat -> Text -> '{IO} ()
    testCAReject          : '{IO} [Result]
    testCNReject          : '{IO} [Result]
    testClient            : Optional SignedCert
                            -> Text
                            -> MVar Nat
                            -> '{IO, Exception} Text
    testConnectSelfSigned : '{IO} [Result]

scratch/main> io.test testConnectSelfSigned

    New test results:

    1. testConnectSelfSigned   ◉ should have reaped what we've sown

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> io.test testCAReject

    New test results:

    1. testCAReject   ◉ correctly rejected self-signed cert

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> io.test testCNReject

    New test results:

    1. testCNReject   ◉ correctly rejected self-signed cert

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
