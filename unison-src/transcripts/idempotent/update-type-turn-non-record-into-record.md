``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Nat
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
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
    
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

  type Foo = { bar : Nat }

scratch/main> find.verbose

  1. -- #5mod0n8ps2emue478fdroo6adp4ovt41qogtmduta8vgv1v8mi8ep2ho0rc1mg699j1feojmv0oe9ndbul5t64menchhnklpgji45o0
     type Foo
     
  2. -- #pshsb3s03nqe194ks3ap3kid0gpb13d68u83gss8vtmbfqma97f84b4vqf362r8gieulqnbfidvh9idkgp6k7mllmss92bh9ebqmolo
     Foo.bar : Foo -> Nat
     
  3. -- #184mc2vauvn8197ecedvus5ubj787dgav6cjkvqqnohej8f997ku7iicurnkvlcqtlv29mjad0mjr3td241q7b0b0kg0i9v4n3qq7vo
     Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
     
  4. -- #sc08708c9s5mhtg6r1obh2mckvjhc5pf2e83lafrkrjrpkikh9kn09vag7kbugcnit50ak8vgr1100am6iqo4ln75uq4dck9pasvnv8
     Foo.bar.set : Nat -> Foo -> Foo
     
  5. -- #5mod0n8ps2emue478fdroo6adp4ovt41qogtmduta8vgv1v8mi8ep2ho0rc1mg699j1feojmv0oe9ndbul5t64menchhnklpgji45o0#0
     Foo.Foo : Nat -> Foo
     
```
