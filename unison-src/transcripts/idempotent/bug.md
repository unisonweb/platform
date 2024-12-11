This tests that `:bug` behaves similarly to `:error` when the stanza fails.

``` ucm :bug
scratch/main> do.something

  ⚠️
  I don't know how to do.something. Type `help` or `?` to get
  help.
```

And when combined with `:error`, it should expect a successful result.

``` ucm :error :bug
scratch/main> help edit

  edit
  `edit foo` prepends the definition of `foo` to the top of the most recently saved file.
  `edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH.
```
