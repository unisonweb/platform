``` ucm :hide
scratch/main> builtins.mergeio

scratch/main> load unison-src/transcripts-using-base/base.u

scratch/main> add
```

## Structural find and replace

Here's a scratch file with some rewrite rules:

``` unison :hide
ex1 = List.map (x -> x + 1) [1,2,3,4,5,6,7]

eitherToOptional e a =
  @rewrite
    term Left e ==> None
    term Right a ==> Some a
    case Left e ==> None
    case Right a ==> Some a
    signature e a . Either e a ==> Optional a

Either.mapRight : (a ->{g} b) -> Either e a ->{g} Either e b
Either.mapRight f = cases
  Left e -> Left e
  Right a -> Right (f a)

rule1 f x = @rewrite
  term x + 1 ==> Nat.increment x
  term (a -> f a) ==> f -- eta reduction

unique type Optional2 a = Some2 a | None2

rule2 x = @rewrite signature Optional ==> Optional2
```

Let's rewrite these:

``` ucm
scratch/main> rewrite rule1

  ☝️

  I found and replaced matches in these definitions: ex1

  The rewritten file has been added to the top of scratch.u

scratch/main> rewrite eitherToOptional

  ☝️

  I found and replaced matches in these definitions:
  Either.mapRight

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): ex1

ex1 = List.map Nat.increment [1, 2, 3, 4, 5, 6, 7]

eitherToOptional e a =
  @rewrite
    term Left e ==> None
    term Right a ==> Some a
    case Left e ==> None
    case Right a ==> Some a
    signature e a . Either e a ==> Optional a

Either.mapRight : (a ->{g} b) -> Either e a ->{g} Either e b
Either.mapRight f = cases
  Left e  -> Left e
  Right a -> Right (f a)

rule1 f x =
  use Nat +
  @rewrite
    term x + 1 ==> Nat.increment x
    term a -> f a ==> f

type Optional2 a = None2 | Some2 a

rule2 x = @rewrite signature Optional ==> Optional2
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): Either.mapRight

ex1 = List.map Nat.increment [1, 2, 3, 4, 5, 6, 7]

eitherToOptional e a =
  @rewrite
    term Left e ==> None
    term Right a ==> Some a
    case Left e ==> None
    case Right a ==> Some a
    signature e a . Either e a ==> Optional a

Either.mapRight : (a ->{g} b) -> Optional a ->{g} Optional b
Either.mapRight f = cases
  None   -> None
  Some a -> Some (f a)

rule1 f x =
  use Nat +
  @rewrite
    term x + 1 ==> Nat.increment x
    term a -> f a ==> f

type Optional2 a = None2 | Some2 a

rule2 x = @rewrite signature Optional ==> Optional2
```

``` ucm :hide
scratch/main> load

scratch/main> add
```

After adding to the codebase, here's the rewritten source:

``` ucm
scratch/main> view ex1 Either.mapRight rule1

  Either.mapRight : (a ->{g} b) -> Optional a ->{g} Optional b
  Either.mapRight f = cases
    None   -> None
    Some a -> Some (f a)

  ex1 : [Nat]
  ex1 = List.map Nat.increment [1, 2, 3, 4, 5, 6, 7]

  rule1 :
    (i ->{g} o)
    -> Nat
    -> Rewrites
      ( RewriteTerm Nat Nat,
        RewriteTerm (i ->{g, g1} o) (i ->{g} o))
  rule1 f x =
    use Nat +
    @rewrite
      term x + 1 ==> Nat.increment x
      term a -> f a ==> f
```

Another example, showing that we can rewrite to definitions that only exist in the file:

``` unison :hide
unique ability Woot1 where woot1 : () -> Nat
unique ability Woot2 where woot2 : () -> Nat

woot1to2 x = @rewrite
  term Woot1.woot1 ==> Woot2.woot2
  term blah ==> blah2
  signature _ . Woot1 ==> Woot2

wootEx : Nat ->{Woot1} Nat
wootEx a =
  _ = !woot1
  blah

blah = 123
blah2 = 456
```

Let's apply the rewrite `woot1to2`:

``` ucm
scratch/main> rewrite woot1to2

  ☝️

  I found and replaced matches in these definitions: wootEx

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): wootEx

ability Woot1 where
  woot1 : '{Woot1} Nat

ability Woot2 where
  woot2 : '{Woot2} Nat

woot1to2 x =
  @rewrite
    term Woot1.woot1 ==> Woot2.woot2
    term blah ==> blah2
    signature _ . Woot1 ==> Woot2

wootEx : Nat ->{Woot2} Nat
wootEx a =
  _ = Woot2.woot2()
  blah2

blah = 123

blah2 = 456
```

``` ucm :hide
scratch/main> load

scratch/main> add
```

After adding the rewritten form to the codebase, here's the rewritten `Woot1` to `Woot2`:

``` ucm
scratch/main> view wootEx

  wootEx : Nat ->{Woot2} Nat
  wootEx a =
    _ = woot2()
    blah2
```

This example shows that rewrite rules can to refer to term definitions that only exist in the file:

``` unison :hide
foo1 =
  b = "b"
  123

foo2 =
  a = "a"
  233

rule = @rewrite
  case None ==> Left "oh no"
  term foo1 ==> foo2
  case None ==> Left "89899"

sameFileEx =
  _ = "ex"
  foo1
```

``` ucm :hide
scratch/main> rewrite rule

scratch/main> load

scratch/main> add
```

After adding the rewritten form to the codebase, here's the rewritten definitions:

``` ucm
scratch/main> view foo1 foo2 sameFileEx

  foo1 : Nat
  foo1 =
    b = "b"
    123

  foo2 : Nat
  foo2 =
    a = "a"
    233

  sameFileEx : Nat
  sameFileEx =
    _ = "ex"
    foo2
```

## Capture avoidance

``` unison :hide
bar1 =
  b = "bar"
  123

bar2 =
  a = 39494
  233

rule bar2 = @rewrite
  case None ==> Left "oh no"
  term bar1 ==> bar2

sameFileEx =
  _ = "ex"
  bar1
```

In the above example, `bar2` is locally bound by the rule, so when applied, it should not refer to the `bar2` top level binding.

``` ucm
scratch/main> rewrite rule

  ☝️

  I found and replaced matches in these definitions: sameFileEx

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): sameFileEx

bar1 =
  b = "bar"
  123

bar2 =
  a = 39494
  233

rule bar2 =
  @rewrite
    case None ==> Left "oh no"
    term bar1 ==> bar2

sameFileEx =
  _ = "ex"
  bar21
```

Instead, it should be an unbound free variable, which doesn't typecheck:

``` ucm :error
scratch/main> load

  Loading changes detected in scratch.u.

  I couldn't figure out what bar21 refers to here:

     19 |   bar21

  I found one or more terms in scope with similar names and the right types.
  If you meant to use one of these, try using it instead:

  bar1 : Nat
```

In this example, the `a` is locally bound by the rule, so it shouldn't capture the `a = 39494` binding which is in scope at the point of the replacement:

``` unison :hide
bar2 =
  a = 39494
  233

rule a = @rewrite
  case None ==> Left "oh no"
  term 233 ==> a
```

``` ucm
scratch/main> rewrite rule

  ☝️

  I found and replaced matches in these definitions: bar2

  The rewritten file has been added to the top of scratch.u
```

``` unison :added-by-ucm scratch.u
-- | Rewrote using: 
-- | Modified definition(s): bar2

bar2 =
  a = 39494
  a1

rule a =
  @rewrite
    case None ==> Left "oh no"
    term 233 ==> a
```

The `a` introduced will be freshened to not capture the `a` in scope, so it remains as an unbound variable and is a type error:

``` ucm :error
scratch/main> load

  Loading changes detected in scratch.u.

  I couldn't figure out what a1 refers to here:

      6 |   a1

  I found one or more terms in scope with similar names and the right types.
  If you meant to use one of these, try using it instead:

  (<|) : (i ->{g} o) -> i ->{g} o
  Bytes.at : Nat -> Bytes -> Optional Nat
  List.at : Nat -> [a] -> Optional a
```

## Structural find

``` unison :hide
eitherEx = Left ("hello", "there")
```

``` ucm :hide
scratch/main> add
```

``` unison :hide
findEitherEx x = @rewrite term Left ("hello", x) ==> Left ("hello" Text.++ x)
findEitherFailure = @rewrite signature a . Either Failure a ==> ()
```

``` ucm
scratch/main> sfind findEitherEx

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. eitherEx

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> sfind findEitherFailure

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. catch
    2. printText
    3. reraise
    4. toEither
    5. toEither.handler

  Tip: Try `edit 1` or `edit 1-5` to bring these into your
       scratch file.

scratch/main> find 1-5

  1. Exception.catch : '{g, Exception} a ->{g} Either Failure a
  2. Exception.reraise : Either Failure a ->{Exception} a
  3. Exception.toEither : '{ε, Exception} a
                          ->{ε} Either Failure a
  4. Exception.toEither.handler : Request {Exception} a
                                  -> Either Failure a
  5. printText : Text ->{IO} Either Failure ()
```
