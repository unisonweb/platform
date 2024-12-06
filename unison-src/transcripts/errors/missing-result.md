### Transcript parser hidden errors

When an error is encountered in a `unison :hide:all` block
then the transcript parser should print the stanza
and surface a helpful message.

``` unison :hide:all
x =
  y = 24
```
