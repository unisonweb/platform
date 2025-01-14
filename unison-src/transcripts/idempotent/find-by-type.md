``` ucm :hide
scratch/main> alias.type ##Text builtin.Text
```

``` unison :hide
unique type A = A Text

foo : A
foo = A "foo!"

bar : Text -> A
bar = A

baz : A -> Text
baz = cases
  A t -> t
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type A
    bar : Text -> A
    baz : A -> Text
    foo : A

scratch/main> find : Text -> A

  1. bar : Text -> A
  2. A.A : Text -> A

scratch/main> find : A -> Text

  1. baz : A -> Text

scratch/main> find : A

  1. foo : A
```

``` ucm :error
scratch/main> find : Text

  ☝️

  I couldn't find exact type matches, resorting to fuzzy
  matching...

  1. baz : A -> Text
  2. bar : Text -> A
  3. A.A : Text -> A
```
