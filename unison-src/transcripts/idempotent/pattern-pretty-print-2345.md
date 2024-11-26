Regression test for https://github.com/unisonweb/unison/pull/2377

``` ucm :hide
scratch/main> builtins.merge
```

``` unison
structural ability Ab where
  a: Nat -> ()

dopey = cases
  ?0 -> ()
  _ -> ()

grumpy = cases
  d -> ()

happy = cases
  true -> ()
  false -> ()

sneezy = cases
  +1 -> ()
  _ -> ()

bashful = cases
  Some a -> ()
  _ -> ()

mouthy = cases
  [] -> ()
  _ -> ()

pokey = cases
  h +: t -> ()
  _ -> ()

sleepy = cases
  i :+ l -> ()
  _ -> ()

demure = cases
  [0] -> ()
  _ -> ()

angry = cases
  a ++ [] -> ()

tremulous = cases
  (0,1) -> ()
  _ -> ()

throaty = cases
  { Ab.a a -> k } -> ()
  { _ } -> ()

agitated = cases
  a | a == 2 -> ()
  _ -> ()

doc = cases
  y@4 -> ()
  _ -> ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural ability Ab
      agitated  : Nat -> ()
      angry     : [t] -> ()
      bashful   : Optional a -> ()
      demure    : [Nat] -> ()
      doc       : Nat -> ()
      dopey     : Char -> ()
      grumpy    : ff284oqf651 -> ()
      happy     : Boolean -> ()
      mouthy    : [t] -> ()
      pokey     : [t] -> ()
      sleepy    : [t] -> ()
      sneezy    : Int -> ()
      throaty   : Request {g, Ab} x -> ()
      tremulous : (Nat, Nat) -> ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural ability Ab
    agitated  : Nat -> ()
    angry     : [t] -> ()
    bashful   : Optional a -> ()
    demure    : [Nat] -> ()
    doc       : Nat -> ()
    dopey     : Char -> ()
    grumpy    : ff284oqf651 -> ()
    happy     : Boolean -> ()
    mouthy    : [t] -> ()
    pokey     : [t] -> ()
    sleepy    : [t] -> ()
    sneezy    : Int -> ()
    throaty   : Request {g, Ab} x -> ()
    tremulous : (Nat, Nat) -> ()

scratch/main> view dopey

  dopey : Char -> ()
  dopey = cases
    ?0 -> ()
    _  -> ()

scratch/main> view grumpy

  grumpy : ff284oqf651 -> ()
  grumpy = cases d -> ()

scratch/main> view happy

  happy : Boolean -> ()
  happy = cases
    true  -> ()
    false -> ()

scratch/main> view sneezy

  sneezy : Int -> ()
  sneezy = cases
    +1 -> ()
    _  -> ()

scratch/main> view bashful

  bashful : Optional a -> ()
  bashful = cases
    Some a -> ()
    _      -> ()

scratch/main> view mouthy

  mouthy : [t] -> ()
  mouthy = cases
    [] -> ()
    _  -> ()

scratch/main> view pokey

  pokey : [t] -> ()
  pokey = cases
    h +: t -> ()
    _      -> ()

scratch/main> view sleepy

  sleepy : [t] -> ()
  sleepy = cases
    i :+ l -> ()
    _      -> ()

scratch/main> view demure

  demure : [Nat] -> ()
  demure = cases
    [0] -> ()
    _   -> ()

scratch/main> view angry

  angry : [t] -> ()
  angry = cases a ++ [] -> ()

scratch/main> view tremulous

  tremulous : (Nat, Nat) -> ()
  tremulous = cases
    (0, 1) -> ()
    _      -> ()

scratch/main> view throaty

  throaty : Request {g, Ab} x -> ()
  throaty = cases
    { Ab.a a -> k } -> ()
    { _ }           -> ()

scratch/main> view agitated

  agitated : Nat -> ()
  agitated = cases
    a | a == 2 -> ()
    _ -> ()

scratch/main> view doc

  doc : Nat -> ()
  doc = cases
    y@4 -> ()
    _   -> ()
```
