# Type modifiers are optional, `unique` is the default.

``` ucm :hide
scratch/main> builtins.merge
```

Types and abilities may be prefixed with either `unique` or `structural`. When left unspecified, `unique` is assumed.

``` unison
type Abc = Abc
unique type Def = Def
structural type Ghi = Ghi

ability MyAbility where const : a
unique ability MyAbilityU where const : a
structural ability MyAbilityS where const : a
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Abc
      type Def
      structural type Ghi
        (also named builtin.Unit)
      ability MyAbility
      structural ability MyAbilityS
      ability MyAbilityU
```
