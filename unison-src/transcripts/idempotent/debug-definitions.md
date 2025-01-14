``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
x = 30

y : Nat
y =
  z = x + 2
  z + 10

structural type Optional a = Some a | None

ability Ask a where
  ask : a
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ability Ask a
    structural type Optional a
      (also named builtin.Optional)
    x : Nat
    y : Nat

scratch/main> debug.term.abt Nat.+

  Builtin term: ##Nat.+

scratch/main> debug.term.abt y

  (let Ref(ReferenceBuiltin "Nat.+") Ref(ReferenceDerived (Id "qpo3o788girkkbb43uf6ggqberfduhtnqbt7096eojlrp27jieco09mdasb7b0b06ej9hj60a00nnbbdo8he0b4e0m7vtopifiuhdig" 0)) 2 in (User "z". Ref(ReferenceBuiltin "Nat.+") (Var User "z") 10)):ReferenceBuiltin "Nat"

scratch/main> debug.term.abt Some

  Constructor #0 of the following type:
  DataDeclaration
      { modifier = Structural
      , annotation = External
      , bound =
          [ User "a" ]
      , constructors' =
          [
              ( External
              , User "Constructor0"
              ,
                  ( User "a". Var User "a" -> ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ,
              ( External
              , User "Constructor1"
              ,
                  ( User "a". ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ]
      }

scratch/main> debug.term.abt ask

  Constructor #0 of the following type:
  EffectDeclaration
      { toDataDecl = DataDeclaration
          { modifier = Unique "a1ns7cunv2dvjmum0q8jbc54g6811cbh"
          , annotation = External
          , bound =
              [ User "a" ]
          , constructors' =
              [
                  ( External
                  , User "Constructor0"
                  ,
                      ( User "a".
                          (
                              {
                                  [ ReferenceDerived
                                      ( Id "d8m1kmiscgfrl5n9ruvq1432lntfntl7nnao45qlk2uqhparm0uq2im0kbspu6u6kv65hd0i5oljq9m4b78peh5ekpma7gkihtsmfh0" 0 )
                                      ( Var User "a" )
                                  ]
                              } Var User "a"
                          )
                      )
                  )
              ]
          }
      }

scratch/main> debug.type.abt Nat

  Builtin type: ##Nat

scratch/main> debug.type.abt Optional

  DataDeclaration
      { modifier = Structural
      , annotation = External
      , bound =
          [ User "a" ]
      , constructors' =
          [
              ( External
              , User "Constructor0"
              ,
                  ( User "a". Var User "a" -> ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ,
              ( External
              , User "Constructor1"
              ,
                  ( User "a". ReferenceDerived
                      ( Id "nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg" 0 )
                      ( Var User "a" )
                  )
              )
          ]
      }

scratch/main> debug.type.abt Ask

  EffectDeclaration
      { toDataDecl = DataDeclaration
          { modifier = Unique "a1ns7cunv2dvjmum0q8jbc54g6811cbh"
          , annotation = External
          , bound =
              [ User "a" ]
          , constructors' =
              [
                  ( External
                  , User "Constructor0"
                  ,
                      ( User "a".
                          (
                              {
                                  [ ReferenceDerived
                                      ( Id "d8m1kmiscgfrl5n9ruvq1432lntfntl7nnao45qlk2uqhparm0uq2im0kbspu6u6kv65hd0i5oljq9m4b78peh5ekpma7gkihtsmfh0" 0 )
                                      ( Var User "a" )
                                  ]
                              } Var User "a"
                          )
                      )
                  )
              ]
          }
      }
```
