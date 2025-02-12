A simple transcript to test the use of exceptions that bubble to the top level.

``` ucm :hide
scratch/main> builtins.merge
```

FYI, here are the `Exception` and `Failure` types:

``` ucm
scratch/main> view Exception Failure

  structural ability builtin.Exception where
    raise : Failure ->{Exception} x

  type builtin.io2.Failure = Failure Type Text Any
```

Here's a sample program just to verify that the typechecker allows `run` to throw exceptions:

``` unison
use builtin IO Exception Test.Result

main : '{IO, Exception} ()
main _ = ()

mytest : '{IO, Exception} [Test.Result]
mytest _ = [Ok "Great"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      main   : '{IO, Exception} ()
      mytest : '{IO, Exception} [Result]
```

``` ucm
scratch/main> run main

  ()

scratch/main> add

  ⍟ I've added these definitions:

    main   : '{IO, Exception} ()
    mytest : '{IO, Exception} [Result]

scratch/main> io.test mytest

    New test results:

    1. mytest   ◉ Great

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Now a test to show the handling of uncaught exceptions:

``` unison
main2 = '(error "oh noes!" ())

error : Text -> a ->{Exception} x
error msg a =
  builtin.Exception.raise (Failure (typeLink RuntimeError) msg (Any a))

unique type RuntimeError =
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type RuntimeError
      error : Text -> a ->{Exception} x
      main2 : '{Exception} r
```

``` ucm :error
scratch/main> run main2

  💔💥

  The program halted with an unhandled exception:

    Failure (typeLink RuntimeError) "oh noes!" (Any ())

  Stack trace:
    ##raise
```
