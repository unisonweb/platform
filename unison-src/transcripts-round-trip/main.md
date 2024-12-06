This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added  to either `reparses-with-same-hash.u` or `reparses.u` as regression tests.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
scratch/a1> builtins.mergeio lib.builtins
scratch/a2> builtins.mergeio lib.builtins
```

``` ucm :hide
scratch/a1> load unison-src/transcripts-round-trip/reparses-with-same-hash.u
scratch/a1> add
```

``` unison
x = ()
```

``` ucm :hide
scratch/a1> find
```

So we can see the pretty-printed output:

``` ucm
scratch/a1> edit.new 1-1000
```

``` ucm :hide
scratch/a1> delete.namespace.force lib.builtins
```

``` ucm :hide
scratch/a2> load
```

``` ucm :hide
scratch/a2> add
scratch/a2> delete.namespace.force lib.builtins
```

This diff should be empty if the two namespaces are equivalent. If it's nonempty, the diff will show us the hashes that differ.

``` ucm :error
scratch/main> diff.namespace /a1: /a2:
```

Now check that definitions in 'reparses.u' at least parse on round trip:

``` ucm :hide
scratch/a3> builtins.mergeio lib.builtins
scratch/a3> load unison-src/transcripts-round-trip/reparses.u
scratch/a3> add
```

This just makes 'roundtrip.u' the latest scratch file.

``` unison :hide
x = ()
```

``` ucm :hide
scratch/a3> find
```

``` ucm
scratch/a3> edit.new 1-5000
```

``` ucm :hide
scratch/a3_new> builtins.mergeio lib.builtins
scratch/a3_new> load
scratch/a3_new> add
scratch/a3> delete.namespace.force lib.builtins
scratch/a3_new> delete.namespace.force lib.builtins
```

These are currently all expected to have different hashes on round trip.

``` ucm
scratch/main> diff.namespace /a3_new: /a3:
```

## Other regression tests not covered by above

### Builtins should appear commented out in the edit.new command

Regression test for https://github.com/unisonweb/unison/pull/3548

``` ucm
scratch/regressions> alias.term ##Nat.+ plus
scratch/regressions> edit.new plus
scratch/regressions> load
```
