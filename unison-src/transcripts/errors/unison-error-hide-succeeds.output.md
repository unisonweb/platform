
### Transcript parser hidden errors

When an expected error is not encountered in a `unison:hide:error` block
then the transcript parser should print the stanza
and surface a helpful message.

```unison
myVal = 3
```


```



🛑

Transcript failed due to an unexpected success above.
