``` ucm :hide
scratch/main> builtins.merge lib.builtin
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
    
      type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
      Foo.baz        : Foo -> Int
      Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
      Foo.baz.set    : Int -> Foo -> Foo
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo
    Foo.baz        : Foo -> Int
    Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
    Foo.baz.set    : Int -> Foo -> Foo
```

``` unison
unique type Foo = { bar : Nat }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo
```

We want the field accessors to go away; but for now they are here, causing the update to fail.

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

scratch/main> view Foo

  type Foo = { bar : Nat, baz : Int }

scratch/main> find.verbose

  1. -- #05gh1dur4778dauh9slaofprc5356n47qpove0c1jl0birt2fcu301js8auu5vfr5bjfga9j8ikuk07ll9fu1gj3ehrp3basguhsd58
     type Foo
     
  2. -- #77mi33dv8ac2s90852khi35km5gsamhnpada8mai0k36obbttgg17qld719ospcs1ht9ctolg3pjsqs6qjnl3hfmu493rgsher73sc0
     Foo.bar : Foo -> Nat
     
  3. -- #7m1n2178r5u12jdnb6crcmanu2gm961kdvbjul5m6hta1s57avibsvk6p5g9efut8sennpgstbb8kf97eujbbuiplsoloa4cael7t90
     Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
     
  4. -- #ghuqoel4pao6v8e7un238i3e86vv7a7pnvgaq8m9s32edm1upgv35gri2iu32ipn9r4poli56r5kr3vtjfrltem696grfl75al4jkgg
     Foo.bar.set : Nat -> Foo -> Foo
     
  5. -- #p8emkm2s09n3nsd8ne5f6fro0vsldk8pn7n6rcf417anuvvun43qrk1ioofs6pdq4537eosao17c7ibvktktr3lfqglmj26gmbulmj0
     Foo.baz : Foo -> Int
     
  6. -- #0il9pl29jpe3fh6vp3qeqai73915k3qffhf4bgttrgsj000b9fgs3bqoj8ugjop6kdr04acc34m1bj7lf417tslfeva7dmmoqdu5hug
     Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
     
  7. -- #87rjeqltvvd4adffsheqae62eefoge8p78pvnjdkc9q1stq20lhubvtpos0io4v3vhnol8nn2uollup97l4orq1fh2h12b0imeuuc58
     Foo.baz.set : Int -> Foo -> Foo
     
  8. -- #05gh1dur4778dauh9slaofprc5356n47qpove0c1jl0birt2fcu301js8auu5vfr5bjfga9j8ikuk07ll9fu1gj3ehrp3basguhsd58#0
     Foo.Foo : Nat -> Int -> Foo
     
```

``` unison :added-by-ucm scratch.u
type Foo = { bar : Nat }

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

Foo.baz : Foo -> Int
Foo.baz = cases Foo _ baz -> baz

Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
Foo.baz.modify f = cases Foo bar baz -> Foo bar (f baz)

Foo.baz.set : Int -> Foo -> Foo
Foo.baz.set baz1 = cases Foo bar _ -> Foo bar baz1

```
