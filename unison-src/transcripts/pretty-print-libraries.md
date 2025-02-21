This transcript is to detect changes in the pretty-printer for a few major public libraries.

We clone releases and not dev branches to avoid external changes, and also to reduce the time needed to clone the libraries.

```ucm
scratch/main> clone @unison/base/releases/3.19.0
@unison/base/releases/3.19.0> edit.namespace
```

```ucm
scratch/main> clone @unison/http/releases/3.3.2
@unison/http/releases/3.3.2> edit.namespace
```
