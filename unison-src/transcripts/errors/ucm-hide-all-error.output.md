### Transcript parser hidden errors

Dangerous scary words\!

When an expected error is not encountered in a `ucm :hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

``` ucm :hide:all:error
scratch/main> history
```

🛑

The transcript was expecting an error in the stanza above, but did not encounter one.
