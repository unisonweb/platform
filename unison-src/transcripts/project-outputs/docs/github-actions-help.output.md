## Some things I wish I'd known about Github Actions

You can't have an `env:` key defined in terms of another `env` key, but you can use `$GITHUB_ENV` to get around this.

You can't define a `matrix` at the top level, it has to be defined within a `job`'s `strategy`.

`runs-on:` doesn't allow `env` for some reason.

Strings don't need quotes, unless you need to force something to be a string.

A `@ref` is always needed on a remote action.

Windows doesn't seem to honor the `default: run: shell:` setting, so you need to set the `shell:` on `run:` manually?

Don't hesitate to do a lot with `run:` blocks aka bash scripts — at least bash is mature and well documented.

e.g.
echo "bar=whatever" \>\> $GITHUB\_OUTPUT
\# access with `steps.<name>.outputs.bar` in yaml strings

``` 
echo "foo=whatever" >> $GITHUB_ENV
# access with `env.foo` in yaml strings, or `$foo` in bash
```

`$GITHUB_ENV` updates the `env` context between steps, but not in the middle of a step. Obvious in retrospect.

It's not clear to me when to use `$GITHUB_OUTPUT` vs `$GITHUB_ENV`, but I have been favoring `$GITHUB_ENV` because it requires fewer characters to access.
However, it seems a little wrong.

### `hashFiles()`

`hashFiles()` can only access files inside of and relative to `$GITHUB_WORKSPACE`.

### `if:`

Although the type rules don't totally make sense in Github Actions, `if:` takes a Boolean.

Therefore, I think the String interpolation in `if: ${{runner.os}} != 'Windows'` causes the whole expression to become a String, which is coerced to `true`, when you definitely didn't mean `if: true`.  So don't use `${{}}` here.

### Job names

Job names will automatically get `(${{matrix.os}})` if you don't use `${{matrix.os}}` somewhere in the name.

### Windows

The whole thing with `.exe` is a mess. Unix commands typically drop and add `.exe` correctly as needed, but Github Actions (e.g. `actions/upload-artifact`?) don't.

### Cache

When using the `cache` action, getting a cache hit on the primary key means you won't update the cache with any changes.

When picking a key, you have to ask, "Which key, if exactly matched, would mean that I'm already so done that I don't even want to save anything new from this run."

Similarly, `save-always: true` only if a key hit means there will be nothing new to save, even if a previous run failed AND a failed result is worth starting with.

Backup restore keys: "Is there a prior run that would be worth starting out from? With the caveat that any irrelevant garbage it includes will be saved into this run too."

### Upload Artifact

I suspect on Windows it can't support paths that select a drive in a Unix-y way,
like `/c/asdf` or `/d/asdf`. It's got to be `C:/asdf` or `C:\asdf` etc.

Upload will complain if any

Upload and Download plugin versions have to match.

### Reusability

Github supports splitting off "reusable workflows" (`jobs` that can be imported into another workflow), and "composite actions" (multi-step `steps` that can be imported into another `job`).

#### Composite actions

Needs to have `shell:` specified on every `run:`

#### Reusable workflows

These have to be in `.github/workflows`, you can't organize them deeper, or elsewhere.

### Reference

Default Environment Variables:
https://docs.github.com/en/actions/learn-github-actions/variables\#default-environment-variables

Workflow syntax:
https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions

Reusable workflows:
https://docs.github.com/en/actions/using-workflows/reusing-workflows

Composite actions:
https://docs.github.com/en/actions/creating-actions/creating-a-composite-action
