Regression test to ensure that `type` and `ability` in embedded doc links are
lexed properly when they occur at the start of identifiers.

That is, `{abilityPatterns}` should be a link to the **term** `abilityPatterns`,
not the ability `Patterns`; the lexer should see this as a single identifier.

See https://github.com/unisonweb/unison/issues/2642 for an example.

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
abilityPatterns : ()
abilityPatterns = ()

structural ability Patterns where p : ()

typeLabels : Nat
typeLabels = 5

structural type Labels = Labels

docs.example1 = {{A doc that links to the {abilityPatterns} term}}
docs.example2 = {{A doc that links to the {ability Patterns} ability}}
docs.example3 = {{A doc that links to the {typeLabels} term}}
docs.example4 = {{A doc that links to the {type Labels} type}}
```

``` ucm :hide
scratch/main> add
```

Now we check that each doc links to the object of the correct name:

``` ucm
scratch/main> display docs.example1

  A doc that links to the abilityPatterns term

scratch/main> display docs.example2

  A doc that links to the Patterns ability

scratch/main> display docs.example3

  A doc that links to the typeLabels term

scratch/main> display docs.example4

  A doc that links to the Labels type
```
