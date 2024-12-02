``` ucm
scratch/main> builtins.mergeio

  Done.
```

``` unison
test> pass = [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      pass : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> pass = [Ok "Passed"]
    
    ✅ Passed Passed
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    pass : [Result]
```

``` unison
test> pass = [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> pass = [Ok "Passed"]
    
    ✅ Passed Passed (cached)
```

``` ucm
scratch/main> add

  ⊡ Ignored previously added definitions: pass

scratch/main> test

  Cached test results (`help testcache` to learn more)

    1. pass   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

``` unison
> ImmutableArray.fromList [?a, ?b, ?c]
> ImmutableByteArray.fromBytes 0xs123456
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ✅

  scratch.u changed.

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > ImmutableArray.fromList [?a, ?b, ?c]
          ⧩
          ImmutableArray.fromList [?a, ?b, ?c]

    2 | > ImmutableByteArray.fromBytes 0xs123456
          ⧩
          fromBytes 0xs123456
```
