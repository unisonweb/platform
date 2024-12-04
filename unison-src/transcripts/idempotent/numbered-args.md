# Using numbered arguments in UCM

``` ucm :hide
scratch/main> alias.type ##Text Text
```

First lets add some contents to our codebase.

``` unison
foo = "foo"
bar = "bar"
baz = "baz"
qux = "qux"
quux = "quux"
corge = "corge"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      bar   : Text
      baz   : Text
      corge : Text
      foo   : Text
      quux  : Text
      qux   : Text
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    bar   : Text
    baz   : Text
    corge : Text
    foo   : Text
    quux  : Text
    qux   : Text
```

We can get the list of things in the namespace, and UCM will give us a numbered
list:

``` ucm
scratch/main> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text
```

We can ask to `view` the second element of this list:

``` ucm
scratch/main> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

scratch/main> view 2

  baz : Text
  baz = "baz"
```

And we can `view` multiple elements by separating with spaces:

``` ucm
scratch/main> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

scratch/main> view 2 3 5

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  quux : Text
  quux = "quux"
```

We can also ask for a range:

``` ucm
scratch/main> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

scratch/main> view "2-4"

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  foo : Text
  foo = "foo"
```

And we can ask for multiple ranges and use mix of ranges and numbers:

``` ucm
scratch/main> find

  1. bar : Text
  2. baz : Text
  3. corge : Text
  4. foo : Text
  5. quux : Text
  6. qux : Text
  7. builtin type Text

scratch/main> view "1-3" 4 "5-6"

  bar : Text
  bar = "bar"

  baz : Text
  baz = "baz"

  corge : Text
  corge = "corge"

  foo : Text
  foo = "foo"

  quux : Text
  quux = "quux"

  qux : Text
  qux = "qux"
```
