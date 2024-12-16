``` ucm :hide
scratch/main> builtins.merge
```

If you

``` unison
foo.bar : Nat
foo.bar = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      foo.bar : Nat
```

``` ucm :hide
scratch/main> add
```

and then put this into `scratch.u`, you'll see a scary message that says `update` will fail:

``` unison
unique type foo = bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    x These definitions would fail on `add` or `update`:
    
      Reason
      blocked type foo
      ctor/term collision   foo.bar   
    
      Tip: Use `help filestatus` to learn more.
```

... but `update` doesn't fail.

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now we have one name `foo.bar` that refers to two things.

``` ucm
scratch/main> view foo.bar

  type foo = bar#ls59rkdkv5#0

  foo.bar#cq22mm4sca : Nat
  foo.bar#cq22mm4sca = 17
```
