``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo

scratch/main> move.term Foo.Bar Stray.Bar

  Done.
```

Now we've set up a situation where the constructor is not where it's supposed to be; it's somewhere else.

``` unison
unique type Foo = Bar Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
```

Note that the constructor name shown here (implied to be called `Foo.Stray.Bar`) doesn't really exist, it's just showing up due to a pretty-printer bug.

``` ucm :error
scratch/main> view Foo

  type Foo = Stray.Bar Nat

scratch/main> update

  Sorry, I wasn't able to perform the update:

  The type Foo has some constructors with missing names, and I
  can't perform an update in this situation.

  You can use `view Foo` and
  `alias.term <hash> Foo.<ConstructorName>` to give names to
  each unnamed constructor, and then try the update again.
```
