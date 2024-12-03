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

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
The last element of a block must be an expression, but this is a
definition:

    3 |   b = 24

Try adding an expression at the end of the block.
It should be of type Nat.
```
