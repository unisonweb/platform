`update` / `patch` (anything that a patch) ignores the namespace named "lib" at the location it's applied. This follows
the project organization convention that dependencies are put in "lib"; it's much easier to apply a patch to all of
one's own code if the "lib" namespace is simply ignored.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
foo = 100
lib.foo = 100
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo     : Nat
      lib.foo : Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    foo     : Nat
    lib.foo : Nat
```

``` unison
foo = 200
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
        (The old definition is also named lib.foo.)
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> names foo

  'foo':
  Hash          Kind   Names
  #9ntnotdp87   Term   foo
```
