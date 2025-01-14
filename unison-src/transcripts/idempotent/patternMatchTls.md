``` ucm :hide
scratch/main> builtins.merge
```

We had bugs in the calling conventions for both send and terminate which would
cause pattern matching on the resulting (Right ()) would cause a runtime error.

``` unison
use builtin.io2.Tls newClient send handshake terminate

frank: '{IO} ()
frank = do
  socket = assertRight (clientSocket.impl "example.com" "443")
  config = ClientConfig.default "example.com" 0xs
  tls = assertRight (newClient.impl config socket)
  () = assertRight (handshake.impl tls)
  () = assertRight (send.impl tls 0xs)
  () = assertRight (terminate.impl tls)
  ()

assertRight : Either a b -> b
assertRight = cases
  Right x -> x
  Left _ -> bug "expected a right but got a left"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      assertRight : Either a b -> b
      frank       : '{IO} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    assertRight : Either a b -> b
    frank       : '{IO} ()

scratch/main> run frank

  ()
```
