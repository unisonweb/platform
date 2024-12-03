``` ucm :hide
scratch/main> builtins.merge
```

Given a test that depends on another definition,

``` unison :hide
foo n = n + 1

test> mynamespace.foo.test =
  n = 2
  if (foo n) == 2 then [ Ok "passed" ] else [ Fail "wat" ]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo                  : Nat -> Nat
    mynamespace.foo.test : [Result]
```

if we change the type of the dependency, the test should show in the scratch file as a test watch.

``` unison
foo n = "hello, world!"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : n -> Text
```

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.
```

``` unison :added-by-ucm scratch.u
foo n = "hello, world!"

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

test> mynamespace.foo.test =
  n = 2
  if foo n == 2 then [Ok "passed"] else [Fail "wat"]

```
