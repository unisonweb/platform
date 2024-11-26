# namespace.dependencies command

``` ucm
scratch/main> builtins.merge lib.builtins

  Done.
```

``` unison :hide
const a b = a
external.mynat = 1
mynamespace.dependsOnText = const external.mynat 10
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    const                     : a -> b -> a
    external.mynat            : Nat
    mynamespace.dependsOnText : Nat

scratch/main> namespace.dependencies mynamespace

  External dependency   Dependents in scratch/main:.mynamespace
  lib.builtins.Nat      1. dependsOnText
                        
  const                 1. dependsOnText
                        
  external.mynat        1. dependsOnText
```
