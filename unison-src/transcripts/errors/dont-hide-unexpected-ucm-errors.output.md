Since this code block is expecting an error, we still hide it. It seems unusual to want to hide an error, but maybe it’s just too verbose or something. This follows the author’s intent.

``` ucm :hide:error
scratch/main> help pull

scratch/main> not.a.command
```

For comparison, here’s what we get without `:hide`.

``` ucm :error
scratch/main> help pull

  pull
  The `pull` command merges a remote namespace into a local
  branch

  `pull @unison/base/main`                merges the branch
                                          `main` of the Unison
                                          Share hosted project
                                          `@unison/base` into
                                          the current branch
  `pull @unison/base/main my-base/topic`  merges the branch
                                          `main` of the Unison
                                          Share hosted project
                                          `@unison/base` into
                                          the branch `topic` of
                                          the local `my-base`
                                          project

  where `remote` is a project or project branch, such as:
    Project (defaults to the /main branch) `@unison/base`
    Project Branch                         `@unison/base/feature`
    Contributor Branch                     `@unison/base/@johnsmith/feature`
    Project Release                        `@unison/base/releases/1.0.0`

scratch/main> not.a.command

  ⚠️
  I don't know how to not.a.command. Type `help` or `?` to get
  help.
```

Even though this code block has `:hide` on it, we should still see the error output, because it wasn’t expecting an error. But we should continue to hide the output *before* the error.

``` ucm :hide
scratch/main> help pull
scratch/main> not.a.command
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️
I don't know how to not.a.command. Type `help` or `?` to get
help.
```
