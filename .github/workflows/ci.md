The new CI workflow builds `ucm`, generates racket source, and generates `unison-runtime` (aka `ucr`), saving them all as build artifacts.

At a high level, the CI process is:
1. On all platforms, build `unisonweb/unison` Haskell program and run tests; save the resulting binaries as build artifacts
2. On Ubuntu, generate and save the Racket sources as a build artifact
3. On all platforms, build the `unison-runtime` Racket program save the resulting binaries as build artifacts.

### `env` vars at the top of `CI.yaml`:

These variables pin some dependency versions, set up some directories to cache, etc. Please see the `env` section in [ci.yaml](./ci.yaml) for specifics.

### Cached directories:

One reason for this change is to reduce the CI time for commits that only change docs, or yaml or other uninteresting things.

#### `.stack`
Caches build dependencies needed by unison packages.

- The **cache key** includes the os, the stackage resolver, `stack.yaml`, and any `package.yaml`.

This currently will re-save on success or failure, but only on a cache miss (source changed).If we find we want to re-save even on a cache key miss (e.g. due to `stack` weirdness), we can change the condition.

#### `.stack-work`
Caches build outputs for unison packages themselves.

- The **cache key** includes the os, the stackage resolver, `stack.yaml`, and any `package.yaml`.

This currently will re-save on success or failure, but only on a cache miss (source changed).If we find we want to re-save even on a cache key miss (e.g. due to `stack` weirdness), we can change the condition.

#### `ucm_local_bin`
A built `ucm` is cached in `ucm_local_bin` after a successful build and Haskell tests pass.
- The **cache key** includes the os, `stack.yaml`, any `package.yaml`, and any `.hs` file.
- On an exact cache hit, these steps are skipped, otherwise they are run:
	- restore `.stack`
	- restore `.stack-work`
	- install `stack`
	- build `ucm` dependencies
	- build `ucm`
	- `unison-cli` tests
	- `unison-core` tests
	- `unison-parser-typechecker` tests
	- `unison-sqlite` tests
	- `unison-syntax` tests
	- `unison-util-bytes` tests
	- `unison-util-cache` tests
	- `unison-util-relation` tests
	- `cli-integration-tests`
	- verification of `stack ghci` startup
	- `interpreter-tests.md`

#### `unison_src_test_results`
A bit is cached in `unison_src_test_results` after non-Haskell tests in the `unison` repo pass.
- The **cache key** includes os, `stack.yaml`, any `package.yaml`, any `.hs` file, and any file in `unison-src/`
- On an exact cache hit, these steps are skipped, otherwise they are run:
    - `round-trip-tests`
    - `transcripts`
    - `unison-src/builtin-tests/interpreter-tests.md`
- If all steps suceed, the `unison_src_test_results` bit is saved.

#### `base-codebase`
This stores the result of `base.md`, which can be reused later to save the cost of a `pull`.
No steps are skipped on a cache hit; however, a second `pull` will mostly be a no-op.

#### `jit_src_scheme`
JIT sources are cached in `jit_src_scheme` if the `generate-jit-source` job completes.
- The **cache key** includes the version of Racket, and the release version of `@unison/internal`.
- If the cache contains `{data-info, boot-generated, simple-wrappers, builtin-generated, compound-wrappers}.ss`, then these steps are skipped, otherwise they are run:
	- "create transcript" to produce pull `@unison/internal` and run `generateSchemeBoot`.
	- download `ucm artifact` saved in the previous step
	- set `ucm` permissions
	- checkout `unison` repo, which includes some static scheme and racket files.
	- run the previously generated transcript
- If all steps succeed, the `jit_src_scheme` cache is saved.

#### `jit_dist`
JIT binaries are cached in `jit_dist` if the `build-jit-binary` job completes.
- The **cache key** includes the version of Racket, and the release version of `@unison/internal`.
- On an exact cache hit, these steps are skipped, otherwise they are run:
	- Restore Racket dependencies
	- setup Racket
	- restore apt cache (Linux only)
	- download jit source from previous job
	- use `raco` to build jit binary
	- download `ucm` artifact from previous job
	- set `ucm` permissions
	- restore `base` codebase saved in previous job
	- jit integration test
- If all of these steps succeed, the `jit_dist` cache is saved.
