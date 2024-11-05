This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added  to either `reparses-with-same-hash.u` or `reparses.u` as regression tests.

``` unison
x = ()
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : ()

```
So we can see the pretty-printed output:

``` ucm
scratch/a1> edit.new 1-1000

  ☝️
  
  I added 111 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
````` unison :added-by-ucm scratch.u
structural ability Abort where abort : {Abort} a

structural ability Ask a where ask : {Ask a} a

structural type Fix_2337
  = Fix_2337 Boolean Boolean

structural ability Fix_2392 where zonk : {Fix_2392} Nat

structural type Fix_2392a x y
  = Oog Nat Nat (Nat, Nat)

structural type foo.Join
  = Join Boolean
  | Table
  | Values [Nat]

structural type Fully.qualifiedName
  = Dontcare () Nat

structural type HandlerWebSocket x y z p q
  = HandlerWebSocket x

structural type Id a
  = Id a

structural type SomethingUnusuallyLong
  = SomethingUnusuallyLong Text Text Text

structural type UUID
  = UUID Nat (Nat, Nat)

structural ability Zoink where
  nay : Text -> (Nat, Nat) ->{Zoink} Nat
  yay.there : Text ->{Zoink} Nat

(>>>>) : Nat -> Nat -> ()
(>>>>) n = cases _ -> bug ""

Abort.toDefault! : a -> '{g, Abort} a ->{g} a
Abort.toDefault! default thunk =
  h x = Abort.toDefault! (handler_1778 default x) thunk
  handle thunk() with h

Abort.toOptional : '{g, Abort} a -> '{g} Optional a
Abort.toOptional thunk = do toOptional! thunk

Abort.toOptional! : '{g, Abort} a ->{g} Optional a
Abort.toOptional! thunk = toDefault! None do Some thunk()

catchAll : x -> Nat
catchAll x = 99

Decode.remainder : '{Ask (Optional Bytes)} Bytes
Decode.remainder = do match ask with
  None   -> Bytes.empty
  Some b -> b Bytes.++ Decode.remainder()

ex1 : Nat
ex1 =
  use Foo.bar qux1 qux3
  use Nat +
  a = qux3 + qux3
  qux1 + qux1 + Foo.bar.qux2

ex2 : Nat
ex2 =
  use Foo.bar qux1
  use Nat +
  a =
    use Foo.bar qux3
    z = 203993
    qux3 + qux3
  qux1 + qux1 + Foo.bar.qux2

ex3 : ()
ex3 =
  a = do
    use Foo.bar qux3
    use Nat +
    x = qux3 + qux3
    x + x
  ()

ex3a : ()
ex3a =
  use Foo.bar qux3
  use Nat +
  a = do qux3 + qux3
  ()

fixity : '('())
fixity =
  do
    use Nat * +
    (===) = (==)
    f <| x = f x
    (<<) f g x = f (g x)
    (>>) f g x = g (f x)
    id x = x
    (do
      (%) = Nat.mod
      ($) = (+)
      c = 1 * (2 + 3) * 4
      plus = 1 + 2 + 3
      plus2 = 1 + (2 + 3)
      d = true && (false || true)
      z = true || false && true
      e = 1 + 2 >= 3 + 4
      f = 9 % 2 === 0
      g = 0 == 9 % 2
      h = 2 * (10 $ 20)
      i1 = 1 * 2 $ (3 * 4) $ 5
      i2 = (1 * 2 $ 3) * 4 $ 5
      oo = (2 * 10 $ 20) * 30 $ 40
      ffffffffffffffffffff x = x + 1
      gg x = x * 2
      j = 10 |> ffffffffffffffffffff |> gg |> gg |> gg |> gg |> gg
      k = ffffffffffffffffffff << gg << ffffffffffffffffffff <| 10
      l = 10 |> (ffffffffffffffffffff >> gg >> ffffffffffffffffffff)
      zzz = 1 + 2 * 3 < 4 + 5 * 6 && 7 + 8 * 9 > 10 + 11 * 12
      zz =
        (1 * 2 + 3 * 3 < 4 + 5 * 6 && 7 + 8 * 9 > 10 + 11 * 12)
          === (1 + 3 * 3 < 4 + 5 * 6 && 7 + 8 * 9 > 10 + 11 * 12)
      zzzz =
        1 * 2 + 3 * 3 < 4 + 5 * 6
          && 7 + 8 * 9 > 10 + 11 * 12 === 1 + 3 * 3 < 4 + 5 * 6
          && 7 + 8 * 9 > 10 + 11 * 12
      ())
      |> id

fix_1035 : Text
fix_1035 =
  use Text ++
  "aaaaaaaaaaaaaaaaaaaaaa"
    ++ "bbbbbbbbbbbbbbbbbbbbbb"
    ++ "cccccccccccccccccccccc"
    ++ "dddddddddddddddddddddd"

fix_1536 : 'Nat
fix_1536 = do
  y = 0
  y

fix_1778 : 'Optional Nat
fix_1778 =
  (do
    abort
    0) |> toOptional

fix_2048 : Doc2
fix_2048 =
  {{
  **my text** __my text__ **MY_TEXT** ___MY__TEXT___ ~~MY~TEXT~~ **MY*TEXT**
  }}

fix_2224 : [()] -> ()
fix_2224 = cases
  x +: (x' +: rest) -> x
  _                 -> ()

fix_2224a : [()] -> ()
fix_2224a = cases
  rest :+ x' :+ x -> ()
  _               -> ()

fix_2224b : [[()]] -> ()
fix_2224b = cases
  rest :+ (rest' :+ x) -> x
  _                    -> ()

fix_2271 : Doc2
fix_2271 =
  {{
  # Full doc body indented
  
    ``` raw
    myVal1 = 42
    myVal2 = 43
    myVal4 = 44
    ```
    
    ``` raw
    indented1= "hi"
    indented2="this is two indents"
    ```
    
    I am two spaces over
  }}

Fix_2337.f : Fix_2337 -> Boolean
Fix_2337.f = cases Fix_2337 a b -> a

Fix_2392.f : Nat -> Fix_2392a ('{Fix_2392} a) ('{Fix_2392} b) -> Nat
Fix_2392.f n _ = n

fix_2650 : Nat
fix_2650 =
  addNumbers : 'Nat
  addNumbers = do
    use Nat +
    y = 12
    13 + y
  addNumbers()

fix_2650a : tvar -> fun -> ()
fix_2650a tvar fun = ()

fix_2650b : tvar -> '()
fix_2650b tvar =
  do
    fix_2650a tvar cases
      Some _ ->
        "oh boy isn't this a very very very very very very very long string?"
      None -> ""

fix_2650c : Optional Nat -> ()
fix_2650c = cases
  Some loooooooooooooooooooooooooooooooooooooooooooooooooooooooong| loooooooooooooooooooooooooooooooooooooooooooooooooooooooong
    == 1  ->
    ()
  _ -> ()

fix_3110a : x -> f -> ()
fix_3110a x f =
  _ = 99
  ()

fix_3110b : ()
fix_3110b =
  fix_3110a
    [1, 2, 3] (x -> let
      y = Nat.increment x
      ())

fix_3110c : ()
fix_3110c = fix_3110a [1, 2, 3] (x -> ignore (Nat.increment x))

fix_3110d : ()
fix_3110d = fix_3110a [1, 2, 3] do
  x -> do
    y = Nat.increment x
    ()

fix_3627 : Nat -> Nat -> Nat
fix_3627 = cases
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ->
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      Nat.+ bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

fix_3710 : '(Nat, Nat, Nat, Nat, Nat, Nat)
fix_3710 = do
  (a, b) = (1, 2)
  (c, d) = (3, 4)
  (e, f) = (5, 6)
  (a, b, c, d, e, f)

fix_3710a : (Nat, Nat, Nat, Nat, Nat, Nat)
fix_3710a =
  (a, b) = (1, 2)
  (c, d) = (3, 4)
  (e, f) = (5, 6)
  (a, b, c, d, e, f)

fix_3710b : x -> (Nat, x, Nat, Nat, Nat, Nat)
fix_3710b x =
  (a, b) = (1, x)
  (c, d) = (3, 4)
  (e, f) = (5, 6)
  (a, b, c, d, e, f)

fix_3710c : x -> '(Nat, x, Nat, Nat, Nat, Nat)
fix_3710c x = do
  (a, b) = (1, x)
  (c, d) = (3, 4)
  (e, f) = (5, 6)
  (a, b, c, d, e, f)

fix_3710d : Optional a -> a
fix_3710d = cases
  Some x -> x
  None   -> bug "oops"

fix_4258 : x -> y -> z -> ()
fix_4258 x y z =
  _ = "fix_4258"
  ()

fix_4258_example : ()
fix_4258_example = fix_4258 1 () 2

fix_4340 : HandlerWebSocket (Nat ->{g, Abort} Text) y z p q
fix_4340 = HandlerWebSocket cases
  1 -> "hi sdflkj sdlfkjsdflkj sldfkj sldkfj sdf asdlkfjs dlfkj sldfkj sdf"
  _ -> abort

fix_4352 : Doc2
fix_4352 = {{ `` +1 `` }}

fix_4384 : Doc2
fix_4384 = {{ {{ docExampleBlock 0 do 2 }} }}

fix_4384a : Doc2
fix_4384a =
  use Nat +
  {{ {{ docExampleBlock 0 do 1 + 1 }} }}

fix_4384b : Doc2
fix_4384b = {{ {{ docExampleBlock 0 do 99 }} }}

fix_4384c : Doc2
fix_4384c =
  use Nat +
  {{
  {{ docExampleBlock 0 do
    x = 1
    y = 2
    x + y }}
  }}

fix_4384d : Doc2
fix_4384d =
  {{
  {{
  docExampleBlock 0 do
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18] }}
  }}

fix_4384e : Doc2
fix_4384e =
  id : x -> x
  id x = x
  {{
  {{
  docExampleBlock
    0 (id id id id id id id id id id id id id id id id id id id id id (x -> 0))
  }}
  }}

fix_4727 : Doc2
fix_4727 = {{ `` 0xs900dc0ffee `` }}

fix_4729a : Doc2
fix_4729a =
  {{
  # H1A
  
    ## H2A
    
       ```
       {{
       # H1B
       
         ## B2B
         
            
       }}
       ```
    
    ## H2A
    
       
  }}

fix_4729b : Doc2
fix_4729b =
  {{
  # H1A
  
    ## H2A
    
       {{ docTable
         [[{{
             # HA
             
               
             }}, {{
             # HB
             
               
             }}], [{{
             # a
             
               
             }}, {{
             # b
             
               
             }}]] }}
    
    ## H2A
    
       
  }}

fix_4729c : Doc2
fix_4729c =
  {{
  # Examples ``
  docCallout
    (Some
      (syntax.docUntitledSection
        [syntax.docSection (syntax.docParagraph [syntax.docWord "Title"]) []]))
    (syntax.docUntitledSection
      [ syntax.docParagraph
          [ syntax.docWord "This"
          , syntax.docWord "is"
          , syntax.docWord "a"
          , syntax.docWord "callout"
          , syntax.docWord "with"
          , syntax.docWord "a"
          , syntax.docWord "title"
          ]
      ]) ``
  
    
  }}

Fix_525.bar.quaffle : Nat
Fix_525.bar.quaffle = 32

fix_525_exampleTerm : Text -> Nat
fix_525_exampleTerm quaffle =
  use Nat +
  bar.quaffle + 1

fix_525_exampleType : Id qualifiedName -> Id Fully.qualifiedName
fix_525_exampleType z = Id (Dontcare () 19)

fnApplicationSyntax : Nat
fnApplicationSyntax =
  use Nat +
  Environment.default = do 1 + 1
  oog = do 2 + 2
  blah : Nat -> Float -> Nat
  blah x y = x + 1
  _ = blah Environment.default() 1.0
  blah oog() (max 1.0 2.0)

Foo.bar.qux1 : Nat
Foo.bar.qux1 = 42

Foo.bar.qux2 : Nat
Foo.bar.qux2 = 44

Foo.bar.qux3 : Nat
Foo.bar.qux3 = 46

Foo'.bar.qux1 : Text
Foo'.bar.qux1 = "43"

Foo'.bar.qux2 : Text
Foo'.bar.qux2 = "45"

Foo'.bar.qux3 : Text
Foo'.bar.qux3 = "47"

forkAt : loc -> c -> Nat
forkAt loc c =
  x = 99
  390439034

handler_1778 : a -> Request {Abort} a -> a
handler_1778 default = cases
  { a }          -> a
  { abort -> _ } -> default

ignore : x -> ()
ignore x = ()

longlines : x -> x
longlines x =
  u = 92393
  x

longlines1 : 'Text
longlines1 =
  do
    longlines
      (longlines_helper
        "This has to laksdjf alsdkfj alskdjf asdf be a long enough string to force a line break"
        ())

longlines2 : (Text, '{g} Bytes)
longlines2 =
  ( "adsf"
  , do
      toUtf8
        "adsfsfdgsfdgsdfgsdfgsfdgsfdgsdgsgsgfsfgsgsfdgsgfsfdgsgfsfdgsdgsdfgsgf"
  )

longlines_helper : x -> 'x
longlines_helper x = do x

multiline_fn : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Nat
multiline_fn a b c d e f g h i j = 42

multiline_list : [Nat]
multiline_list =
  use Nat +
  [ 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
  , multiline_fn
      12939233
      2102020
      329292
      429292
      522020
      62929292
      72020202
      820202
      920202
      1020202
  ]

nested_fences : Doc2
nested_fences =
  {{
  ```` raw
  ``` unison
  r = "boopydoo"
  ```
  ````
  }}

raw_a : Text
raw_a =
  """
  a
  b
  """

raw_b : Text
raw_b =
  """
  a
  b
  c -- note blank line
  
  """

raw_c : Text
raw_c =
  """
  ignored (wonky case)
  Use an extra blank line if you'd like a trailing newline. Like so:
  
  """

raw_d : Text
raw_d =
  """
  ignored (works great)
  Use an extra blank line if you'd like a trailing newline. Like so:
  
  """

simplestPossibleExample : Nat
simplestPossibleExample =
  use Nat +
  1 + 1

softhang : a -> b -> Nat
softhang a b = 42

softhang2 : x -> f -> Nat
softhang2 x f = 0

softhang21 : Nat
softhang21 =
  use Nat +
  handle
    x = 1
    y = abort
    x + y
  with cases
    { a }          -> a
    { abort -> _ } -> 0

softhang21a : Text
softhang21a =
  use Nat +
  handle
    x = 1
    y = abort
    x + y
  with cases
    { a } -> "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "
    { abort -> _ } ->
      "lskdfjlaksjdf al;ksdjf;lkj sa;sldkfja;sldfkj a;lsdkfj asd;lfkj "

softhang22 : Nat
softhang22 = softhang2 [0, 1, 2, 3, 4, 5] cases
  0 -> 0
  1 -> 1
  n -> n Nat.+ 100

softhang23 : 'Nat
softhang23 = do
  catchAll do
    use Nat +
    x = 1
    y = 2
    x + y

softhang24 : 'Nat
softhang24 = do match 0 with
  0 -> 0
  1 -> 1
  n -> n

softhang25 : Text
softhang25 = match Nat.increment 1 with
  2 -> "yay"
  n -> "oh no"

softhang26 : Nat
softhang26 = softhang2 [1, 2, 3, 4] cases
  0 -> 1
  n -> n Nat.+ 1

softhang27 : somewhere -> Nat
softhang27 somewhere = forkAt somewhere do
  use Nat +
  x = 1
  y = 2
  x + y

softhang28 : Nat
softhang28 = 
  softhang2 [0, 1, 2, 3, 4, 5] cases
    0 -> 0
    1 -> 1
    n ->
      forkAt
        0
        (n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n
          Nat.+ n)

softhang_a : x -> 'Nat
softhang_a x = do
  use Nat +
  a = 1
  b = 2
  softhang a do
    c = 3
    a + b

softhang_b : x -> 'Nat
softhang_b x =
  do
    use Nat +
    a = 1
    b = 2
    softhang
      (100
      + 200
      + 300
      + 400
      + 500
      + 600
      + 700
      + 800
      + 900
      + 1000
      + 1100
      + 1200
      + 1300
      + 1400
      + 1500)
      do
      c = 3
      a + b

softhang_c : x -> 'Nat
softhang_c x = do
  use Nat +
  a = 1
  b = 2
  1 + (softhang a do
    c = 3
    a + b)

softhang_d : x -> '(b -> Nat)
softhang_d x = do
  use Nat +
  a = 1
  b = 2
  c = softhang do
    c = 3
    a + b
  c

somethingVeryLong : 'Nat
somethingVeryLong =
  go x =
    do
      match (a -> a) x with
        SomethingUnusuallyLong
          lijaefliejalfijelfj aefilaeifhlei liaehjffeafijij
          | lijaefliejalfijelfj == aefilaeifhlei    -> 0
          | lijaefliejalfijelfj == liaehjffeafijij  -> 1
        _ -> 2
  go (SomethingUnusuallyLong "one" "two" "three")

stew_issue : ()
stew_issue =
  error x = ()
  a ++ b = 0
  toText a = a
  Debug : a -> b -> ()
  Debug a b = ()
  error (Debug None do Debug "Failed " 42)

stew_issue2 : ()
stew_issue2 =
  error x = ()
  a ++ b = 0
  toText a = a
  Debug : a -> b -> ()
  Debug a b = ()
  error (Debug None do "Failed " ++ toText 42)

stew_issue3 : ()
stew_issue3 =
  id x = x
  error x = ()
  a ++ b = 0
  blah x y = 99
  toText a = a
  configPath = 0
  Debug a b = ()
  error
    (Debug None do
      "Failed to get timestamp of config file " ++ toText configPath)

test3 : '('('r))
test3 = do
  run : Nat -> a
  run x = bug x
  runrun = 42
  a = "asldkfj"
  b = "asdflkjasdf"
  do do run runrun do do runrun

use_clauses_example : Int -> Text -> Nat
use_clauses_example oo quaffle =
  use Nat +
  bar.quaffle + bar.quaffle + 1

use_clauses_example2 : Int -> Nat
use_clauses_example2 oo =
  use Nat +
  quaffle = "hi"
  bar.quaffle + bar.quaffle + bar.quaffle + 1

UUID.random : 'UUID
UUID.random = do UUID 0 (0, 0)

UUID.randomUUIDBytes : 'Bytes
UUID.randomUUIDBytes = do
  (UUID a (b, _)) = random()
  encodeNat64be a Bytes.++ encodeNat64be b

(|>) : a -> (a ->{e} b) ->{e} b
a |> f = f a
`````

This diff should be empty if the two namespaces are equivalent. If it's nonempty, the diff will show us the hashes that differ.

``` ucm
scratch/main> diff.namespace /a1: /a2:

  The namespaces are identical.

```
Now check that definitions in 'reparses.u' at least parse on round trip:

This just makes 'roundtrip.u' the latest scratch file.

``` unison
x = ()
```

``` ucm
scratch/a3> edit.new 1-5000

  ☝️
  
  I added 2 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
```` unison :added-by-ucm scratch.u
explanationOfThisFile : Text
explanationOfThisFile =
  """
  Put definitions in here that are expected to
  parse with a different hash after pretty-printing.
  """

sloppyDocEval : Doc2
sloppyDocEval =
  use Nat +
  {{
  Here's an example of an eval block that's technically a lambda but should
  print as a backticked block (since old docs in the wild still use this
  format).
  
  ```
  1 + 1
  ```
  }}
````

These are currently all expected to have different hashes on round trip.

``` ucm
scratch/main> diff.namespace /a3_new: /a3:

  Updates:
  
    1. sloppyDocEval : Doc2
       ↓
    2. sloppyDocEval : Doc2

```
## Other regression tests not covered by above

### Builtins should appear commented out in the edit.new command

Regression test for https://github.com/unisonweb/unison/pull/3548

``` ucm
scratch/regressions> alias.term ##Nat.+ plus

  Done.

scratch/regressions> edit.new plus

  ☝️
  
  I added 1 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

scratch/regressions> load

  Loading changes detected in scratch.u.

  I loaded scratch.u and didn't find anything.

```
``` unison :added-by-ucm scratch.u
-- builtin plus : ##Nat -> ##Nat -> ##Nat
```

