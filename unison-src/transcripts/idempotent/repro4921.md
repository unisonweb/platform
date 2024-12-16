In this example, Alice updates `foo`, while Bob adds a new dependent `bar` of the original `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his `bar`.

``` ucm :hide
project/main> builtins.mergeio
```

Initial branch:

``` unison :hide
foo : Text
foo = "old foo"
```

``` ucm :hide
project/main> add

project/main> branch alice
```

Alice's updates:

``` unison :hide
foo : Text
foo = "new foo"
```

``` ucm :hide
project/alice> update

project/main> branch bob
```

Bob's adds:

``` unison :hide
bar : Text
bar = foo ++ " - " ++ foo
```

``` ucm
project/bob> display bar

  "old foo - old foo"
```

``` ucm :hide
project/bob> add
```

If we merge /bob into /alice, we'd expect `bar` to be updated with Alice's change:

``` ucm
project/alice> merge /bob

  Loading branches...

  Computing diff between branches...

  Loading dependents of changes...

  Loading and merging library dependencies...

  Rendering Unison file...

  Typechecking Unison file...

  I merged project/bob into project/alice.

project/alice> view foo bar

  bar : Text
  bar =
    use Text ++
    foo ++ " - " ++ foo

  foo : Text
  foo = "new foo"

project/alice> display foo bar

  "new foo"

  "old foo - old foo"
```

But then, bafflingly, if you restart ucm with this same codebase, `display foo bar` will show the new definition, meaning that somehow the in-memory results are out of sync with what's actually in the databse?

``` bash
$ ucm transcript --save-codebase transcripts/idempotent/repro4921.md
$ ucm --codebase /private/var/folders/7j/rwwnwnyn0djb5tw9bnxhd5bc0000gn/T/transcript-e39aecc3fc26a4dd #for example
# <snip>
# project/alice> display foo bar
#
#  "new foo"
#
#
#  "new foo - new foo"
```
