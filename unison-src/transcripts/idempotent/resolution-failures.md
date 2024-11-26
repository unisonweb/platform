# Resolution Errors

This transcript tests the errors printed to the user when a name cannot be resolved.

## Codebase Setup

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

First we define differing types with the same name in different namespaces:

``` unison
unique type one.AmbiguousType = one.AmbiguousType
unique type two.AmbiguousType = two.AmbiguousType

one.ambiguousTerm = "term one"
two.ambiguousTerm = "term two"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type one.AmbiguousType
      type two.AmbiguousType
      one.ambiguousTerm : Text
      two.ambiguousTerm : Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type one.AmbiguousType
    type two.AmbiguousType
    one.ambiguousTerm : Text
    two.ambiguousTerm : Text
```

## Tests

Now we introduce code which isn't sufficiently qualified.
It is ambiguous which type from which namespace we mean.

We expect the output to:

1.  Print all ambiguous usage sites separately
2.  Print possible disambiguation suggestions for each unique ambiguity

``` unison :error
-- We intentionally avoid using a constructor to ensure the constructor doesn't
-- affect type resolution.
useAmbiguousType : AmbiguousType -> ()
useAmbiguousType _ = ()

useUnknownType : UnknownType -> ()
useUnknownType _ = ()

-- Despite being a duplicate disambiguation, this should still be included in the annotations printout
separateAmbiguousTypeUsage : AmbiguousType -> ()
separateAmbiguousTypeUsage _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.


    ❓
    
    I couldn't resolve any of these symbols:
    
        3 | useAmbiguousType : AmbiguousType -> ()
        4 | useAmbiguousType _ = ()
        5 | 
        6 | useUnknownType : UnknownType -> ()
        7 | useUnknownType _ = ()
        8 | 
        9 | -- Despite being a duplicate disambiguation, this should still be included in the annotations printout
       10 | separateAmbiguousTypeUsage : AmbiguousType -> ()
    
    
    Symbol          Suggestions
                    
    AmbiguousType   one.AmbiguousType
                    two.AmbiguousType
                    
    UnknownType     No matches
```

Currently, ambiguous terms are caught and handled by type directed name resolution,
but expect it to eventually be handled by the above machinery.

``` unison :error
useAmbiguousTerm = ambiguousTerm
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what ambiguousTerm refers to here:

      1 | useAmbiguousTerm = ambiguousTerm

  The name ambiguousTerm is ambiguous. I couldn't narrow it down
  by type, as any type would work here.

  I found some terms in scope that have matching names and
  types. Maybe you meant one of these:

  one.ambiguousTerm : Text
  two.ambiguousTerm : Text
```
