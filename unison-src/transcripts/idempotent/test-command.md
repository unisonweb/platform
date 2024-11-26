Merge builtins so we get enough names for the testing stuff.

``` ucm :hide
scratch/main> builtins.merge
```

The `test` command should run all of the tests in the current directory.

``` unison
test1 : [Result]
test1 = [Ok "test1"]

foo.test2 : [Result]
foo.test2 = [Ok "test2"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.test2 : [Result]
      test1     : [Result]
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> test

  ✅  





    New test results:

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

Tests should be cached if unchanged.

``` ucm
scratch/main> test

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` won't descend into the `lib` namespace, but `test.all` will.

``` unison
lib.dep.testInLib : [Result]
lib.dep.testInLib = [Ok "testInLib"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      lib.dep.testInLib : [Result]
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> test

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2
    2. test1       ◉ test1

  ✅ 2 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> test.all

    
    Cached test results (`help testcache` to learn more)
    
      1. foo.test2   ◉ test2
      2. test1       ◉ test1
    
    ✅ 2 test(s) passing
    
    ✅  



    New test results:

    1. lib.dep.testInLib   ◉ testInLib

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` WILL run tests within `lib` if specified explicitly.

``` ucm
scratch/main> test lib.dep

  Cached test results (`help testcache` to learn more)

    1. lib.dep.testInLib   ◉ testInLib

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

`test` can be given a relative path, in which case it will only run tests found somewhere in that namespace.

``` ucm
scratch/main> test foo

  Cached test results (`help testcache` to learn more)

    1. foo.test2   ◉ test2

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
