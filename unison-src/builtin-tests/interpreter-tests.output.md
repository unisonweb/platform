The runtime tests are hosted at https://share.unison-lang.org/@unison/runtime-tests/

If you want to add or update tests, you can create a branch of that project, and update the `runtime_tests_version` line in `jit-tests.sh` and `CI.yaml`

Before merging the PR on Github, we'll merge your branch on Share and restore `runtime_tests_version` to /main or maybe a release.

``` ucm :hide:error
scratch/main> this is a hack to trigger an error, in order to swallow any error on the next line.

scratch/main> we delete the project to avoid any merge conflicts or complaints from ucm.

scratch/main> delete.project runtime-tests
```

``` ucm :hide
scratch/main> clone @unison/runtime-tests/releases/0.0.1 runtime-tests/selected
```

``` ucm
runtime-tests/selected> run tests

  ()

runtime-tests/selected> run tests.interpreter.only

  ()
```
