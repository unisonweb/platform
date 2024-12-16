``` ucm :hide
scratch/main> builtins.mergeio lib.builtin
```

Consider

``` unison
test> subnamespace.myTerm.tests.ex1 = []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      subnamespace.myTerm.tests.ex1 : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> subnamespace.myTerm.tests.ex1 = []
    
```

Would be nice to be able to add docs the shortcut way we can for terms as in

``` unison
{{ This is a term }}
term = []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      term     : [elem]
      term.doc : Doc2
```

in this way:

``` unison
{{ Test comes from an appendix of RFC 8675309 }}
test> subnamespace.myTerm.tests.ex1 = []
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      >     : ∀ test subnamespace.myTerm.tests.ex1 elem.
                test -> subnamespace.myTerm.tests.ex1 -> [elem]
      >.doc : Doc2
```

As you can see, this parsed really weird.
