``` ucm :hide
scratch/main> builtins.merge
```

Checks a case that was resulting in variable capture when compiling
pattern matching. `y` was evidently getting captured by the variable
introduced for `confuser decoy`

``` unison
type NatBox = NatBox Nat
type Decoy a = { confuser : Tres }

type Tres = One | Two | Three

xyzzy : NatBox -> Decoy a -> Nat
xyzzy box decoy =
      (NatBox y) = box
      (natty) =  -- Note that these parentheses are required
        match confuser decoy with
          Tres.One -> y
          Two -> y + 1
          Three -> 11
      natty

> xyzzy (NatBox 1) (Decoy One)
> xyzzy (NatBox 1) (Decoy Two)
> xyzzy (NatBox 1) (Decoy Three)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type Decoy a
      type NatBox
      type Tres
      Decoy.confuser        : Decoy a -> Tres
      Decoy.confuser.modify : (Tres ->{g} Tres)
                              -> Decoy a1
                              ->{g} Decoy a
      Decoy.confuser.set    : Tres -> Decoy a1 -> Decoy a
      xyzzy                 : NatBox -> Decoy a -> Nat

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    16 | > xyzzy (NatBox 1) (Decoy One)
           ⧩
           1

    17 | > xyzzy (NatBox 1) (Decoy Two)
           ⧩
           2

    18 | > xyzzy (NatBox 1) (Decoy Three)
           ⧩
           11
```
