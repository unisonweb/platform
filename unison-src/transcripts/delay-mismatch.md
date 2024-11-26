```ucm
scratch/main> builtins.merge lib.builtins
```

```unison:error
missingDo : 'Nat
missingDo = 2
```

```unison:error
superfluousDo : Nat
superfluousDo = do
  2
```
