``` unison
foo = 123

bar = 456

mytest = [Ok "ok"]
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar    : Nat
      foo    : Nat
      mytest : [Result]

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    bar    : Nat
    foo    : Nat
    mytest : [Result]

scratch/main> edit.new foo bar

  ☝️
  
  I added 2 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

scratch/main> edit.new mytest

  ☝️
  
  I added 1 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
``` unison :added-by-ucm scratch.u
bar : Nat
bar = 456

foo : Nat
foo = 123
```

``` unison :added-by-ucm scratch.u
test> mytest = [Ok "ok"]
```

``` ucm
scratch/main> edit.new missing

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    missing

```
# `edit`

The `edit` command adds to the current fold, and takes care not to add definitions that are already in the file.

This stanza does nothing for some reason (transcript runner bug?), so we repeat it twice.

``` unison
foo = 17
bar = 18
baz = 19
```

``` ucm

```
``` unison
foo = 17
bar = 18
baz = 19
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      baz : Nat
      foo : Nat

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    baz : Nat
    foo : Nat

```
``` unison
foo = 17
bar = 18
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
``` ucm
scratch/main> edit bar baz

  ☝️
  
  I added 1 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
``` unison :added-by-ucm scratch.u
baz : Nat
baz = 19
```

