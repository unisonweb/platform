``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat
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
```

``` unison
unique type Foo
  = Bar Nat
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

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view Foo

  type Foo = Bar Nat

scratch/main> find.verbose

  1. -- #h88o5sirfn0a8f4o81sb012p2rha5h8r73n8bloc8qq94kqmltjq94iiep2e6dj7ppuulc8jce2f0vmddqp76nm0hqs9jh53s502v4g
     type Foo
     
  2. -- #h88o5sirfn0a8f4o81sb012p2rha5h8r73n8bloc8qq94kqmltjq94iiep2e6dj7ppuulc8jce2f0vmddqp76nm0hqs9jh53s502v4g#0
     Foo.Bar : Nat -> Foo
     
```
