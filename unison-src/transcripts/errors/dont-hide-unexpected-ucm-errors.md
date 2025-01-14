Since this code block is expecting an error, we still hide it. It seems unusual to want to hide an error, but maybe it’s just too verbose or something. This follows the author’s intent.

``` ucm :hide :error
scratch/main> help pull
scratch/main> not.a.command
```

For comparison, here’s what we get without `:hide`.

``` ucm :error
scratch/main> help pull
scratch/main> not.a.command
```

Even though this code block has `:hide` on it, we should still see the error output, because it wasn’t expecting an error. But we should continue to hide the output _before_ the error.

``` ucm :hide
scratch/main> help pull
scratch/main> not.a.command
```
