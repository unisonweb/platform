# namespace.dependencies command

```unison
myMetadata = "just some text"
```

```unison
dependsOnNat = 1
dependsOnInt = -1
dependsOnIntAndNat = Nat.drop 1 10
hasMetadata = 3
```

```ucm
  ☝️  The namespace .dependencies is empty.

.dependencies> add

  ⍟ I've added these definitions:
  
    dependsOnInt       : Int
    dependsOnIntAndNat : Nat
    dependsOnNat       : Nat
    hasMetadata        : Nat

.dependencies> link .metadata.myMetadata hasMetadata

  Updates:
  
    1. dependencies.hasMetadata : Nat
       + 2. myMetadata : Text

.dependencies> namespace.dependencies

  External dependency   Dependents in .dependencies
  builtin.Int           1. dependsOnInt
                        
  builtin.Nat           2. dependsOnIntAndNat
                        3. dependsOnNat
                        4. hasMetadata
                        
  builtin.Text          4. hasMetadata
                        
  builtin.Nat.drop      2. dependsOnIntAndNat
                        
  metadata.myMetadata   4. hasMetadata

```
