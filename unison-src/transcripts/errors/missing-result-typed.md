### Transcript parser hidden errors

When an error is encountered in a `unison :hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide:all
a : Nat
a =
  b = 24
```
