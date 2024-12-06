``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo = Bar Nat

makeFoo : Nat -> Foo
makeFoo n = Bar (n+10)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Foo
      makeFoo : Nat -> Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    makeFoo : Nat -> Foo
```

``` unison
unique type Foo = internal.Bar Nat

Foo.Bar : Nat -> Foo
Foo.Bar n = internal.Bar n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⊡ Previously added definitions will be ignored: Foo
    
    ⍟ These new definitions are ok to `add`:
    
      Foo.Bar : Nat -> Foo
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

scratch/main> view Foo

  type Foo = internal.Bar Nat

scratch/main> find.verbose

  1. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60
     type Foo
     
  2. -- #36rn6jqt1k5jccb3c7vagp3jam74dngr92kgcntqhs6dbkua54verfert2i6hsku6uitt9s2jvt1msric0tgemal52d5apav6akn25o
     Foo.Bar : Nat -> Foo
     
  3. -- #b509v3eg4kehsg29g6pvrogeb71ue32nm2fj9284n4i7lprsr7u9a7g6s695d09du0fsfti6rrsk1s62q5thpr1jjkqb3us3s0lrd60#0
     Foo.internal.Bar : Nat -> Foo
     
  4. -- #204frdcl0iid1ujkkfbkc6b3v7cgqp56h1q3duc46i5md6qb4m6am1fqbceb335u87l05gkdnaa7fjn4alj1diukgme63e41lh072l8
     makeFoo : Nat -> Foo
     
```
