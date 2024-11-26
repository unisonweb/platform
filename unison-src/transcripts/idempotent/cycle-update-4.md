`update` properly discovers and establishes new cycles.

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
ping : 'Nat
ping _ = 1

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
ping _ = !clang + 1

clang : 'Nat
clang _ = !pong + 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      clang : 'Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat
```

``` ucm
scratch/main> update.old ping

  ⍟ I've added these definitions:

    clang : 'Nat

  ⍟ I've updated these names to your new definition:

    ping : 'Nat
    pong : 'Nat

scratch/main> view ping pong clang

  clang : 'Nat
  clang _ =
    use Nat +
    pong() + 3

  ping : 'Nat
  ping _ =
    use Nat +
    clang() + 1

  pong : 'Nat
  pong _ =
    use Nat +
    ping() + 2
```
