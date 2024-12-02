Since this code block is expecting an error, we still hide it. It seems unusual to want to hide an error, but maybe it’s just too verbose or something. This follows the author’s intent.

``` unison :hide :error
x + x +
```

For comparison, here is what we get without the `:hide`.

``` unison :error
x + x +
```

Even though this code block has `:hide` on it, we should still see the error output, because it wasn’t expecting an error.

``` unison :hide
x + x +
```
