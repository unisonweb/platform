Some preconditions due to another bug

``` unison :hide
five = 5
```

``` ucm :hide
preconditions/main> add

scratch/main> alias.type ##Nat lib.builtin.Nat
```

from @kylegoetz:

I found a way to inadvertently add a watch expression/statement to the codebase.

Before describing how to replicate, I want to explain why I think it's the wrong behavior:

Given:

``` unison
> y = 5
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ✅

  scratch.u changed.

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > y = 5
          ⧩
          5
```

``` ucm
scratch/main> add

```

No error, but if you clear out the scratch file and `view y`, you get `The following names were not found in the codebase. Check your spelling.` back in ucm. Also, if you `ls`, `y` is nowhere to be found.

``` unison
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I loaded scratch.u and didn't find anything.
```

``` ucm :error
scratch/main> view y

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    y
```

``` ucm
scratch/main> ls

  1. lib/ (1 type)
```

So far, this makes sense. Watch expressions are not addable to the codebase.

However, if you now put this into a scratch file:

``` unison
> x = 5
test = do x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      test : 'Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > x = 5
          ⧩
          5
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    test : 'Nat
```

`add` will successfully add `test` even though it's dependent upon a watch expression.

If you `ls` or `find x` you will not find `x`

``` ucm
scratch/main> ls

  1. lib/ (1 type)
  2. test ('Nat)
```

``` ucm :error
scratch/main> find x

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  😶

  No results. Check your spelling, or try using tab completion
  to supply command arguments.

  `debug.find.global` can be used to search outside the current
  namespace.
```

If you `view test`, you will see something like:

``` ucm
scratch/main> view test

  test : 'Nat
  test = do #k09e57l61r
```

and if you view the hash, you will get

``` ucm
scratch/main> view #k09e57l61r

  #k09e57l61r : Nat
  #k09e57l61r = 5
```

So the watch expression *does* get added to the codebase.

I expected `add test` to fail because it's reliant on something that shouldn't be added to the codebase.

This could be a problem if someone is using watch expressions to test code using credentials that shouldn't be added to the codebase. (I was investigating this behavior specifically because I thought I might be able to test a library using live credentials and put the private stuff in a watch expression to ensure it doesn't get added to the codebase (which is, once added, not removable).
