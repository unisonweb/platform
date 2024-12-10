This transcript will fail, because we’re claiming that the stanza has a bug, but `do.something` errors as expected.

``` ucm :error :bug
scratch/main> do.something
```

🎉

## You fixed a bug\!

The stanza above marked with `:error :bug` is now failing with

``` 
⚠️
I don't know how to do.something. Type `help` or `?` to get
help.
```

so you can remove `:bug` and close any appropriate Github issues. If the error message is different from the expected error message, open a new issue and reference it in this transcript.
