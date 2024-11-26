# Duplicate Term Detection

``` ucm :hide
scratch/main> builtins.merge
```

Trivial duplicate terms should be detected:

``` unison :error
x = 1
x = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name x:
      1 | x = 1
      2 | x = 2
```

Equivalent duplicate terms should be detected:

``` unison :error
x = 1
x = 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name x:
      1 | x = 1
      2 | x = 1
```

Duplicates from record accessors/setters should be detected

``` unison :error
structural type Record = {x: Nat, y: Nat}
Record.x = 1
Record.x.set = 2
Record.x.modify = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name Record.x:
      1 | structural type Record = {x: Nat, y: Nat}
      2 | Record.x = 1


  I found multiple bindings with the name Record.x.modify:
      1 | structural type Record = {x: Nat, y: Nat}
      2 | Record.x = 1
      3 | Record.x.set = 2
      4 | Record.x.modify = 2


  I found multiple bindings with the name Record.x.set:
      1 | structural type Record = {x: Nat, y: Nat}
      2 | Record.x = 1
      3 | Record.x.set = 2
```

Duplicate terms and constructors should be detected:

``` unison :error
structural type SumType = X

SumType.X = 1

structural ability AnAbility where
  thing : Nat -> ()

AnAbility.thing = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name AnAbility.thing:
      6 |   thing : Nat -> ()
      7 | 
      8 | AnAbility.thing = 2


  I found multiple bindings with the name SumType.X:
      1 | structural type SumType = X
      2 | 
      3 | SumType.X = 1
```
