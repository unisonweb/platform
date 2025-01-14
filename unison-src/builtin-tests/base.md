When this file is modified, CI will create a new codebase and re-run this;
otherwise it may reuse a previously cached codebase.

Thus, make sure the contents of this file define the contents of the cache
(e.g. don't pull `latest`.)

``` ucm
scratch/main> pull @unison/base/releases/2.5.0 .base
scratch/main> builtins.mergeio
scratch/main> undo
```
