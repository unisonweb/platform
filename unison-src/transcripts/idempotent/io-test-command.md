``` ucm :hide
scratch/main> builtins.merge
```

The `io.test` command should run all of the tests within the current namespace, excluding libs.

``` unison :hide
-- We manually specify types so we don't need to pull in base to run IO and such
ioAndExceptionTest : '{IO, Exception} [Result]
ioAndExceptionTest = do
  [Ok "Success"]

ioTest : '{IO} [Result]
ioTest = do
  [Ok "Success"]

lib.ioAndExceptionTestInLib  : '{IO, Exception} [Result]
lib.ioAndExceptionTestInLib  = do
  [Ok "Success"]
```

``` ucm :hide
scratch/main> add
```

Run a IO tests one by one

``` ucm
scratch/main> io.test ioAndExceptionTest

    New test results:

    1. ioAndExceptionTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> io.test ioTest

    New test results:

    1. ioTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`io.test` doesn't cache results

``` ucm
scratch/main> io.test ioAndExceptionTest

    New test results:

    1. ioAndExceptionTest   ◉ Success

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`io.test.all` will run all matching tests except those in the `lib` namespace.

``` ucm
scratch/main> io.test.all





    New test results:

    1. ioAndExceptionTest   ◉ Success
    2. ioTest               ◉ Success

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
