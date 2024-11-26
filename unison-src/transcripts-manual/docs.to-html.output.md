``` ucm
test-html-docs/main> builtins.mergeio lib.builtins

  Done.
```

``` unison
{{A doc directly in the namespace.}}
some.ns.direct = 1

{{A doc pretty deeply nested in the namespace.}}
some.ns.pretty.deeply.nested = 2

{{A doc outside the namespace.}}
some.outside = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      some.ns.direct                   : Nat
      some.ns.direct.doc               : Doc2
      some.ns.pretty.deeply.nested     : Nat
      some.ns.pretty.deeply.nested.doc : Doc2
      some.outside                     : Nat
      some.outside.doc                 : Doc2
```

``` ucm
test-html-docs/main> add

  ⍟ I've added these definitions:

    some.ns.direct                   : Nat
    some.ns.direct.doc               : Doc2
    some.ns.pretty.deeply.nested     : Nat
    some.ns.pretty.deeply.nested.doc : Doc2
    some.outside                     : Nat
    some.outside.doc                 : Doc2

test-html-docs/main> docs.to-html some.ns unison-src/transcripts-manual/docs.to-html
```
