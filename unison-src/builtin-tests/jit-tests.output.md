The runtime tests are hosted at https://share.unison-lang.org/@unison/runtime-tests/

If you want to add or update tests, you can create a branch of that project, and update the `runtime_tests_version` line in `jit-tests.sh` and `CI.yaml`

Before merging the PR on Github, we'll merge your branch on Share and restore `runtime_tests_version` to /main or maybe a release.

``` ucm :hide :error
scratch/main> this is a hack to trigger an error, in order to swallow any error on the next line.

scratch/main> we delete the project to avoid any merge conflicts or complaints from ucm.

scratch/main> delete.project runtime-tests
```

``` ucm :hide
scratch/main> clone @unison/runtime-tests/releases/0.0.3 runtime-tests/selected
```

``` ucm
runtime-tests/selected> run.native tests

  ()

runtime-tests/selected> run.native tests.jit.only

  ()
```

Per Dan:
It's testing a flaw in how we were sending code from a scratch file to the native runtime, when that happened multiple times.
Related to the verifiable refs and recursive functions.

``` unison
foo = do
  go : Nat ->{Exception} ()
  go = cases
    0 -> ()
    n -> go (decrement n)
  go 1000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo : '{Exception} ()
```

``` ucm
scratch/main> run.native foo

  ()

scratch/main> run.native foo

  ()
```

This can also only be tested by separately running this test, because
it is exercising the protocol that ucm uses to talk to the jit during
an exception.

``` ucm :error
runtime-tests/selected> run.native testBug

  💔💥

  I've encountered a call to builtin.bug with the following
  value:

    "testing"
```
