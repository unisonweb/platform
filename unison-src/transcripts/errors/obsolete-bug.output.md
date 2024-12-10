This transcript will error, because we’re claiming that the stanza has a bug, but `help` works as expected.

``` ucm :bug
scratch/main> help edit

  edit
  `edit foo` prepends the definition of `foo` to the top of the most recently saved file.
  `edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH.
```

🎉

## You fixed a bug\!

The stanza above with `:bug` is now passing\! You can remove `:bug` and close any appropriate Github issues.
