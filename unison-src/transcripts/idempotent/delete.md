# Delete

``` ucm :hide
scratch/main> builtins.merge lib.builtins
```

The delete command can delete both terms and types.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

``` ucm :error
scratch/main> delete.verbose foo

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    foo
```

Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

``` unison :hide
foo = 1
structural type Foo = Foo ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo
    foo : Nat

scratch/main> delete.verbose foo

  Removed definitions:

    1. foo : Nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.verbose Foo

  Removed definitions:

    1. structural type Foo

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.verbose Foo.Foo

  Removed definitions:

    1. Foo.Foo : '#089vmor9c5

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

How about an ambiguous term?

``` unison :hide
a.foo = 1
a.bar = 2
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    a.bar : Nat
    a.foo : Nat

scratch/main> debug.alias.term.force a.bar a.foo

  Done.
```

A delete should remove both versions of the term.

``` ucm
scratch/main> delete.verbose a.foo

  Removed definitions:

    1. a.foo#gjmq673r1v : Nat

  Name changes:

    Original               Changes
    2. a.bar            ┐  3. a.foo#dcgdua2lj6 (removed)
    4. a.foo#dcgdua2lj6 ┘  

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> ls a

  1. bar (Nat)
```

Let's repeat all that on a type, for completeness.

``` unison :hide
structural type a.Foo = Foo ()
structural type a.Bar = Bar
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type a.Bar
      (also named lib.builtins.Unit)
    structural type a.Foo

scratch/main> debug.alias.type.force a.Bar a.Foo

  Done.

scratch/main> delete.verbose a.Foo

  Removed definitions:

    1. structural type a.Foo#089vmor9c5

  Name changes:

    Original                Changes
    2. a.Bar             ┐  3. a.Foo#00nv2kob8f (removed)
    4. lib.builtins.Unit │  
    5. a.Foo#00nv2kob8f  ┘  

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.verbose a.Foo.Foo

  Removed definitions:

    1. a.Foo.Foo : '#089vmor9c5

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

Finally, let's try to delete a term and a type with the same name.

``` unison :hide
foo = 1
structural type foo = Foo ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type foo
    foo : Nat

scratch/main> delete.verbose foo

  Removed definitions:

    1. structural type foo
    2. foo : Nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

We want to be able to delete multiple terms at once

``` unison :hide
a = "a"
b = "b"
c = "c"
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    a : Text
    b : Text
    c : Text

scratch/main> delete.verbose a b c

  Removed definitions:

    1. a : Text
    2. b : Text
    3. c : Text

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

We can delete terms and types in the same invocation of delete

``` unison :hide
structural type Foo = Foo ()
a = "a"
b = "b"
c = "c"
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo
    a : Text
    b : Text
    c : Text

scratch/main> delete.verbose a b c Foo

  Removed definitions:

    1. structural type Foo
    2. a : Text
    3. b : Text
    4. c : Text

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> delete.verbose Foo.Foo

  Name changes:

    Original      Changes
    1. Foo.Foo ┐  2. Foo.Foo (removed)
    3. foo.Foo ┘  

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

We can delete a type and its constructors

``` unison :hide
structural type Foo = Foo ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo

scratch/main> delete.verbose Foo Foo.Foo

  Removed definitions:

    1. structural type Foo

  Name changes:

    Original      Changes
    2. Foo.Foo ┐  3. Foo.Foo (removed)
    4. foo.Foo ┘  

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

You should not be able to delete terms which are referenced by other terms

``` unison :hide
a = 1
b = 2
c = 3
d = a + b + c
```

``` ucm :error
scratch/main> add

  ⍟ I've added these definitions:

    a : Nat
    b : Nat
      (also named a.bar)
    c : Nat
    d : Nat

scratch/main> delete.verbose a b c

  ⚠️

  I didn't delete the following definitions because they are
  still in use:

  Dependency   Referenced In
  c            1. d
               
  a            2. d
```

But you should be able to delete all terms which reference each other in a single command

``` unison :hide
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    e : Nat
    f : Nat
    g : Nat
    h : Nat

scratch/main> delete.verbose e f g h

  Removed definitions:

    1. e : Nat
    2. f : Nat
    3. g : Nat
    4. h : Nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

You should be able to delete a type and all the functions that reference it in a single command

``` unison :hide
structural type Foo = Foo Nat

incrementFoo : Foo -> Nat
incrementFoo = cases
  (Foo.Foo n) -> n + 1
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Foo
    incrementFoo : Foo -> Nat

scratch/main> delete.verbose Foo Foo.Foo incrementFoo

  Removed definitions:

    1. structural type Foo
    2. Foo.Foo      : Nat -> Foo
    3. incrementFoo : Foo -> Nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

If you mess up on one of the names of your command, delete short circuits

``` unison :hide
e = 11
f = 12 + e
g = 13 + f
h = e + f + g
```

``` ucm :error
scratch/main> add

  ⍟ I've added these definitions:

    e : Nat
    f : Nat
    g : Nat
    h : Nat

scratch/main> delete.verbose e f gg

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    gg
```

Cyclical terms which are guarded by a lambda are allowed to be deleted

``` unison :hide
ping _ = 1 Nat.+ !pong
pong _ = 4 Nat.+ !ping
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    ping : 'Nat
    pong : 'Nat

scratch/main> delete.verbose ping

  Removed definitions:

    1. ping : 'Nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.

scratch/main> view pong

  pong : 'Nat
  pong _ =
    use Nat +
    4 + #l9uq1dpl5v.1()
```
