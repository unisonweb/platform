You can use a keyword or reserved operator as a name segment if you surround it with backticks.

``` ucm :error
scratch/main> view `match`

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    `match`

scratch/main> view `=`

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    `=`
```

You can also use backticks to expand the set of valid symbols in a symboly name segment to include these three: `.()`

This allows you to spell `.` or `()` as name segments (which historically have appeared in the namespace).

``` ucm :error
scratch/main> view `.`

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    `.`

scratch/main> view `()`

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    `()`
```
