These are commands that will likely be useful during development.

__General:__ `./scripts/test.sh` compiles and builds the Haskell code and runs all tests. Recommended that you run this before pushing any code to a branch that others might be working on.

_Disclaimer_ If you have trouble getting started, please get in touch via [Slack](https://unison-lang.org/community) so we can help.  If you have any fixes to the process, please send us a PR!

## Running Unison

To get cracking with Unison:

1. [Install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install).
2. Build the project with `stack build`. This builds all executables.
3. (Optional) Run `./dev-ui-install.hs` to fetch the latest release of the codebase UI. If you don't care about running the codebase UI locally you can ignore this step.
4. After building do `stack exec unison` to will initialize a codebase in your home directory (in `~/.unison`). This only needs to be done once. (Alternatively, you can use `stack exec -- unison -C <other dir> to create a codebase in <other dir>`
5. `stack exec unison` starts Unison and watches for `.u` file changes in the current directory. If you want to run it in a different directory, just add `unison` to your `PATH`, after finding it with `stack exec which unison`.

On startup, Unison prints a url for the codebase UI. If you did step 3 above, then visiting that URL in a browser will give you a nice interface to your codebase.

## Autoformatting your code with Ormolu

We use 0.5.0.1 of Ormolu and CI will fail if your code isn't properly formatted. 

```
ghcup install ghc 9.2.7 # if not already installed
ghcup install cabal # if not already installed
cabal unpack ormolu-0.5.0.1
cd ormolu-0.5.0.1
cabal install -w ghc-9.2.7
```

You can then add the following to `.git/hooks/pre-commit` to make sure all your commits get formatted:

```
#!/bin/bash

set -e

if [[ -z "${SKIP_FORMATTING}" ]]; then
    ormolu -i $(git diff --cached --name-only | grep '\.hs$')
    git add $(git diff --cached --name-only)
fi
```

If you've got an existing PR that somehow hasn't been formatted correctly, you can install the correct version of Ormolu locally, then do:

```
ormolu -i $(git ls-files | grep '\.hs$')
```

Also note that you can always wrap a comment around some code you don't want Ormolu to touch, using:

```
{- ORMOLU_DISABLE -}
dontFormatMe = do blah
                    blah
                  blah
{- ORMOLU_ENABLE -}
```

## Running Tests

* `stack test --fast` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Most test suites support selecting a specific test to run by passing a prefix as a test argument:

* `stack test unison-parser-typechecker --fast --test-arguments my-test-prefix` builds and runs most test suites, see below for exceptions to this (e.g. transcript tests).

Some tests are executables instead:

* `stack exec transcripts` runs the transcripts-related integration tests, found in `unison-src/transcripts`. You can add more tests to this directory.
* `stack exec transcripts -- prefix-of-filename` runs only transcript tests with a matching filename prefix.
* `stack exec integration-tests` runs the additional integration tests for cli. These tests are not triggered by `tests` or `trancscripts`.
* `stack exec unison -- transcript unison-src/transcripts-round-trip/main.md` runs the pretty-printing round trip tests
* `stack exec unison -- transcript unison-src/transcripts-manual/benchmarks.md` runs the benchmark suite. Output goes in unison-src/transcripts-manual/benchmarks/output.txt.

### Building everything at once, including tests and benchmarks, but without running them:
Do:

    stack build --fast --test --bench --no-run-tests --no-run-benchmarks

### What if you want a profiled build?

Do:

    stack build --profile unison-parser-typechecker

Again you can leave off the flag. To run an executable with profiling enabled, do:

    stack exec -- <executable-name> +RTS -p

That will generate a `<executable-name>.prof` plain text file with profiling data. [More info on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).

## Building with cabal

Unison can also be built/installed with cabal. You'll need the same ghc
used by `stack.yaml` to successfully build its dependencies.
The provided project file is also in contrib/ so you'll need to specify
its location on the command line.

* To build all projects use

    `cabal v2-build --project-file=contrib/cabal.project all`

* Tests can be run with e.g.

    `cabal v2-test --project-file=contrib/cabal.project all`

* The executable can be installed with

    `cabal v2-install --project-file=contrib/cabal.project unison`

* The install directory can be modified with the option `--installdir: ...`

* Take in account that if you want to load the project in haskell-language-server using cabal instead stack you will need:
  * Copy or link `./contrib/cabal.project` to `./cabal.project`
  * Delete or rename the existing `./hie.yaml`. The default behaviour without `hie.yaml` works with cabal.

## Building on Windows

### I get an error about unison/sql/something

This codebase uses symlinks as a workaround for some inconveniences in the `here` package. Support for symlinks in Windows is relatively new, and isn't enabled by default. As a result, your cloned copy of the code probably won't build.

First you'll need to enable "Developer Mode" in your Windows settings.

	See https://consumer.huawei.com/en/support/content/en-us15594140/

Then you'll need to enable symlink support in your `git` configuration, e.g.

    `git config core.symlinks true`

And then ask `git` to fix up your symlinks with `git checkout .`

More context at: https://stackoverflow.com/a/59761201/310162


### I get an error about `removeDirectoryRecursive`/`removeContentsRecursive`/`removePathRecursive`/`permission denied (Access is denied.)`

Stack doesn't work deterministically in Windows due to mismatched expectations about how file deletion works. If you get this error, you can just retry the build and it will probably make more progress than the last time.
