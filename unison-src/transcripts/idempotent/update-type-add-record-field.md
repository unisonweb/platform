``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo
```

``` unison
unique type Foo = { bar : Nat, baz : Int }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Foo.baz        : Foo -> Int
      Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
      Foo.baz.set    : Int -> Foo -> Foo
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view Foo

  type Foo = { bar : Nat, baz : Int }

scratch/main> find.verbose

  1. -- #m0tpa159pbsdld5ea0marnq9614dnmjjc72n1evi4bsk45a1hl84qprt6vdvejuuiuc3f5o23olc1t19tk1dt8mjobmr0chqc3svij8
     type Foo
     
  2. -- #16o21u2lli7rd8f3h4epnblpfk3h68gag99d5gicihcpk15dkv4m9601picg37ncsbg2e8j63tu7ebs40jrcoifs7f6nqrus3qnfgv0
     Foo.bar : Foo -> Nat
     
  3. -- #64v4pv4rvmnts82gbsb1u2dvgdu4eqq8leq37anqjrkq8s9c7ogrjahdotc36nrodva6ok1rs4ah5k09i7sb0clvcap2773k1t7thb8
     Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
     
  4. -- #bkfjhnu5jqv2m49hosd8g6m4e5u9ju4b1du90cji8s8hnaendvnep2a5cd085ejtu27c4lm3u7slamk52p86rubp211jc5c0qcut2l0
     Foo.bar.set : Nat -> Foo -> Foo
     
  5. -- #u394ule3vr1ab8q5nit6miktgpki9gj4nft5jfjsho6cflg94kf953mdhjj8s18e9j1525iv8l5ebjhebnuc01q51fl8ni5n9j0gs28
     Foo.baz : Foo -> Int
     
  6. -- #cbbi7mqcaqdlcl41uajb608b8fi5dfvc654rmd47mk9okpn1t3jltrf8psnn3g2tnr1ftctj753fjhco3ku1oapc664upo1h6eodfrg
     Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
     
  7. -- #hhi45ik1245qq3g93586f998di8j5afvjamqr2m08auqq8ogqt4d01rejrse4qsl27381vnqnt8uffhgvnc0nk22o5uabimjhji4868
     Foo.baz.set : Int -> Foo -> Foo
     
  8. -- #m0tpa159pbsdld5ea0marnq9614dnmjjc72n1evi4bsk45a1hl84qprt6vdvejuuiuc3f5o23olc1t19tk1dt8mjobmr0chqc3svij8#0
     Foo.Foo : Nat -> Int -> Foo
     
```
