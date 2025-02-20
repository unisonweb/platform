``` ucm :hide
fresh/main> builtins.merge
```

This passes, because the IO sandbox doesn't seem to apply to `test>` watch expressions.

``` unison
test> foo.test =
  x = 192
  Debug.trace "x" x
  [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.test : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    2 |   x = 192
    
    ✅ Passed Passed
```

``` ucm
fresh/main> add

  ⍟ I've added these definitions:

    foo.test : [Result]
```

The `test` command succeeds, because the result of `foo.test` is already cached, which skips the IO sandbox.

``` ucm
fresh/main> test

  Cached test results (`help testcache` to learn more)

    1. foo.test   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

This test which is essentially the same will fail, because it is never run with a `test>` watch expression to get the result into the test cache.

``` unison
bar.test : [Test.Result]
bar.test =
  x = 42
  Debug.trace "x" x
  [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar.test : [Result]
```

``` ucm
fresh/main> add

  ⍟ I've added these definitions:

    bar.test : [Result]
```

``` ucm :error
fresh/main> test

    
    Cached test results (`help testcache` to learn more)
    
      1. foo.test   ◉ Passed
    
    ✅ 1 test(s) passing
    
    ✅  



  Error while evaluating test `bar.test`

  💔💥

  I've encountered a call to builtin.bug with the following
  value:

    "pure code can't perform I/O"

  Stack trace:
    builtin.bug
    #oit1slb1i3
```
