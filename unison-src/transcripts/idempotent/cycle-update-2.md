Update a member of a cycle with a type-preserving update, but sever the cycle.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      ping : 'Nat
      pong : 'Nat
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ping : 'Nat
    pong : 'Nat
```

``` unison
ping : 'Nat
ping _ = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view ping pong

  ping : 'Nat
  ping _ = 3

  pong : 'Nat
  pong _ =
    use Nat +
    ping() + 2
```
