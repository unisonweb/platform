# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

``` unison
unique ability Channels where
  send : a -> {Channels} ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ability Channels
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ability Channels
```

Now we update the ability, changing the name of the constructor, *but*, we simultaneously
add a new top-level term with the same name as the constructor which is being
removed from Channels.

``` unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = ()

thing : '{Channels} ()
thing _ = send 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a -> ()
      thing         : '{Channels} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ability Channels
```

These should fail with a term/ctor conflict since we exclude the ability from the update.

``` ucm :error
scratch/main> update.old patch Channels.send

  x These definitions failed:

    Reason
    term/ctor collision   Channels.send   : a -> ()

    Tip: Use `help filestatus` to learn more.

scratch/main> update.old patch thing

  ⍟ I've added these definitions:

    Channels.send : a -> ()
    thing         : '{Channels} ()

  ⍟ I've updated these names to your new definition:

    ability Channels
```

If however, `Channels.send` and `thing` *depend* on `Channels`, updating them should succeed since it pulls in the ability as a dependency.

``` unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = sends [a]

thing : '{Channels} ()
thing _ = send 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⊡ Previously added definitions will be ignored: Channels
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      Channels.send : a ->{Channels} ()
      thing         : '{Channels} ()
```

These updates should succeed since `Channels` is a dependency.

``` ucm
scratch/main> update.old.preview patch Channels.send

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⊡ Previously added definitions will be ignored: Channels
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      Channels.send : a ->{Channels} ()

scratch/main> update.old.preview patch thing

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⊡ Previously added definitions will be ignored: Channels
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      Channels.send : a ->{Channels} ()
      thing         : '{Channels} ()
```

We should also be able to successfully update the whole thing.

``` ucm
scratch/main> update.old

  ⊡ Ignored previously added definitions: Channels

  ⍟ I've updated these names to your new definition:

    Channels.send : a ->{Channels} ()
    thing         : '{Channels} ()
```

# Constructor-term conflict

``` ucm :hide
scratch/main2> builtins.merge lib.builtins
```

``` unison
X.x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      X.x : Nat
```

``` ucm
scratch/main2> add

  ⍟ I've added these definitions:

    X.x : Nat
```

``` unison
structural ability X where
  x : ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    x These definitions would fail on `add` or `update`:
    
      Reason
      blocked structural ability X
      ctor/term collision   X.x   
    
      Tip: Use `help filestatus` to learn more.
```

This should fail with a ctor/term conflict.

``` ucm :error
scratch/main2> add

  x These definitions failed:

    Reason
    blocked structural ability X
    ctor/term collision   X.x   

    Tip: Use `help filestatus` to learn more.
```
