``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

``` unison :hide
foldMap = "top-level"
nested.deeply.foldMap = "nested"
lib.base.foldMap = "lib"
lib.dep.lib.transitive.foldMap = "transitive-lib"
-- A deeply nested definition with the same hash as the top level one.
-- This should not be included in the completion results if a better name with the same hash IS included.
lib.dep.lib.transitive_same_hash.foldMap = "top-level"
foldMapWith = "partial match"

other = "other"
```

``` ucm :hide
scratch/main> add
```

Completion should find all the `foldMap` definitions in the codebase,
sorted by number of name segments, shortest first.

Individual LSP clients may still handle sorting differently, e.g. doing a fuzzy match over returned results, or
prioritizing exact matches over partial matches. We don't have any control over that.

``` ucm
scratch/main> debug.lsp-name-completion foldMap

  Matching Path   Name                             Hash
  foldMap         foldMap                          #o38ps8p4q6
  foldMapWith     foldMapWith                      #r9rs4mcb0m
  foldMap         nested.deeply.foldMap            #snrjegr5dk
  foldMap         lib.base.foldMap                 #jf4buul17k
  foldMap         lib.dep.lib.transitive.foldMap   #0o01gvr3fi
```

Should still find the term which has a matching hash to a better name if the better name doesn't match.

``` ucm
scratch/main> debug.lsp-name-completion transitive_same_hash.foldMap

  Matching Path                  Name                                       Hash
  transitive_same_hash.foldMap   lib.dep.lib.transitive_same_hash.foldMap   #o38ps8p4q6
```
