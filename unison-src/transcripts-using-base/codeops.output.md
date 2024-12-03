Test for code serialization operations.

Define a function, serialize it, then deserialize it back to an actual
function. Also ask for its dependencies for display later.

``` unison
save : a -> Bytes
save x = Value.serialize (Value.value x)

Code.save : Code -> Bytes
Code.save = Code.serialize

Code.get : Link.Term -> Code
Code.get tl = match Code.lookup tl with
  Some co -> co
  None -> throw "could not look up code"

load : Bytes ->{io2.IO, Throw Text} a
load b = match Value.deserialize b with
  Left _ -> throw "could not deserialize value"
  Right v -> match Value.load v with
    Left _ -> throw "could not load value"
    Right x -> x

Code.load : Bytes ->{io2.IO, Throw Text} Code
Code.load b = match Code.deserialize b with
  Left _ -> throw "could not deserialize code"
  Right co -> co

roundtrip : a ->{io2.IO, Throw Text} a
roundtrip x = load (save x)

handleTest : Text -> Request {Throw Text} a -> Result
handleTest t = let
  pfx = "(" ++ t ++ ") "
  cases
    { _ } -> Ok (pfx ++ "passed")
    { Throw.throw s -> _ } -> Fail (pfx ++ s)

expectFailure : Text -> Request {Throw Text} a -> Result
expectFailure t =
  pfx = "(" ++ t ++ ") "
  cases
    { _ } -> Fail (pfx ++ ": expected failure, but succeeded")
    { Throw.throw _ -> _ } -> Ok (pfx ++ "passed")

identical : Text -> a -> a ->{Throw Text} ()
identical err x y =
  if x == y
  then ()
  else throw ("mismatch" ++ err)

structural type Three a b c = zero a | one b | two c

showThree : Three Nat Nat Nat -> Text
showThree = cases
  zero n -> "zero " ++ toText n
  one n -> "one " ++ toText n
  two n -> "two " ++ toText n

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = cases
  [] -> []
  x +: xs -> f x ++ concatMap f xs

prod : [a] -> [b] -> [(a,b)]
prod l = cases
  [] -> []
  y +: ys -> map (x -> (x,y)) l ++ prod l ys

threes : [Three Nat Nat Nat]
threes = map zero fib10 ++ map one fib10 ++ map two fib10

extensionals
   : (a -> b -> Text)
  -> (a -> b -> c)
  -> (a -> b -> c)
  -> [(a,b)] ->{Throw Text} ()
extensionals sh f g = cases
  [] -> ()
  (x,y) +: xs ->
    identical (" on: " ++ sh x y) (f x y) (g x y)
    extensionals sh f g xs

fib10 : [Nat]
fib10 = [1,2,3,5,8,13,21,34,55,89]

mutual0 n = mutual1 (n+1)
mutual1 n = mutual2 (drop n 1)
mutual2 n =
 if n == 0 then 5
 else mutual0 (drop n 1)


extensionality : Text -> (Three Nat Nat Nat -> Nat -> b) ->{io2.IO} Result
extensionality t f = let
  sh t n = "(" ++ showThree t ++ ", " ++ toText n ++ ")"
  handle
    g = roundtrip f
    extensionals sh f g (prod threes fib10)
  with handleTest t

identicality : Text -> a ->{io2.IO} Result
identicality t x
  = handle identical "" x (roundtrip x) with handleTest t

idempotence : Text -> Link.Term ->{io2.IO} Result
idempotence t tl =
  handle let
    co1 = Code.get tl
    b1 = Code.save co1
    co2 = Code.load b1
    b2 = Code.save co2
    identical "" b1 b2
  with handleTest t

-- Check that the transitive dependencies of some code
-- would pass validation.
verify : Text -> [(Link.Term,Code)] -> ()
verify name rco =
  handle
    match validateLinks rco with
      Left rs -> throw "missing links"
      Right [] -> ()
      Right rs -> throw "invalid links"
  with cases
    { r } -> r
    { raise _ -> _ } -> throw "failure raised"

verified : Text -> Link.Term ->{io2.IO} Result
verified name link =
  handle verify name (Code.transitiveDeps link)
  with handleTest ("verified " ++ name)

rejected : Text -> [(Link.Term,Code)] ->{io2.IO} Result
rejected name rco =
  handle verify name rco
  with expectFailure ("rejected " ++ name)

missed : Text -> Link.Term -> Result
missed name link =
  rco = match Code.transitiveDeps link with
    _ +: co -> co
    _ -> []
  rejected ("missing " ++ name) rco

swapped : Text -> Link.Term -> Result
swapped name link =
  rco0 = Code.transitiveDeps link
  rco = uncurry List.zip (first List.reverse (List.unzip rco0))
  rejected ("swapped " ++ name) rco
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural type Three a b c
      Code.get       : Link.Term ->{IO, Throw Text} Code
      Code.load      : Bytes ->{IO, Throw Text} Code
      Code.save      : Code -> Bytes
      concatMap      : (a ->{g} [b]) -> [a] ->{g} [b]
      expectFailure  : Text -> Request {Throw Text} a -> Result
      extensionality : Text
                       -> (Three Nat Nat Nat -> Nat -> b)
                       ->{IO} Result
      extensionals   : (a -> b -> Text)
                       -> (a -> b -> c)
                       -> (a -> b -> c)
                       -> [(a, b)]
                       ->{Throw Text} ()
      fib10          : [Nat]
      handleTest     : Text -> Request {Throw Text} a -> Result
      idempotence    : Text -> Link.Term ->{IO} Result
      identical      : Text -> a -> a ->{Throw Text} ()
      identicality   : Text -> a ->{IO} Result
      load           : Bytes ->{IO, Throw Text} a
      missed         : Text -> Link.Term ->{IO} Result
      mutual0        : Nat -> Nat
      mutual1        : Nat -> Nat
      mutual2        : Nat -> Nat
      prod           : [a] -> [b] -> [(a, b)]
      rejected       : Text -> [(Link.Term, Code)] ->{IO} Result
      roundtrip      : a ->{IO, Throw Text} a
      save           : a -> Bytes
      showThree      : Three Nat Nat Nat -> Text
      swapped        : Text -> Link.Term ->{IO} Result
      threes         : [Three Nat Nat Nat]
      verified       : Text -> Link.Term ->{IO} Result
      verify         : Text
                       -> [(Link.Term, Code)]
                       ->{Throw Text} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural type Three a b c
    Code.get       : Link.Term ->{IO, Throw Text} Code
    Code.load      : Bytes ->{IO, Throw Text} Code
    Code.save      : Code -> Bytes
    concatMap      : (a ->{g} [b]) -> [a] ->{g} [b]
    expectFailure  : Text -> Request {Throw Text} a -> Result
    extensionality : Text
                     -> (Three Nat Nat Nat -> Nat -> b)
                     ->{IO} Result
    extensionals   : (a -> b -> Text)
                     -> (a -> b -> c)
                     -> (a -> b -> c)
                     -> [(a, b)]
                     ->{Throw Text} ()
    fib10          : [Nat]
    handleTest     : Text -> Request {Throw Text} a -> Result
    idempotence    : Text -> Link.Term ->{IO} Result
    identical      : Text -> a -> a ->{Throw Text} ()
    identicality   : Text -> a ->{IO} Result
    load           : Bytes ->{IO, Throw Text} a
    missed         : Text -> Link.Term ->{IO} Result
    mutual0        : Nat -> Nat
    mutual1        : Nat -> Nat
    mutual2        : Nat -> Nat
    prod           : [a] -> [b] -> [(a, b)]
    rejected       : Text -> [(Link.Term, Code)] ->{IO} Result
    roundtrip      : a ->{IO, Throw Text} a
    save           : a -> Bytes
    showThree      : Three Nat Nat Nat -> Text
    swapped        : Text -> Link.Term ->{IO} Result
    threes         : [Three Nat Nat Nat]
    verified       : Text -> Link.Term ->{IO} Result
    verify         : Text
                     -> [(Link.Term, Code)]
                     ->{Throw Text} ()
```

``` unison
structural ability Zap where
  zap : Three Nat Nat Nat

h : Three Nat Nat Nat -> Nat -> Nat
h y x = match y with
  zero y -> x + y
  one y -> x + y + y
  two y -> x + 3*y

f : Nat ->{Zap} Nat
f x = h zap x

fVal : Value
fVal = Value.value f

fDeps : [Link.Term]
fDeps = Value.dependencies fVal

fSer : Bytes
fSer = Value.serialize fVal

rotate : Three Nat Nat Nat -> Three Nat Nat Nat
rotate = cases
  zero n -> one (n+1)
  one n -> two (n+2)
  two n -> zero (drop n 6)

zapper : Three Nat Nat Nat -> Request {Zap} r -> r
zapper t = cases
  { r } -> r
  { zap -> k } -> handle k t with zapper (rotate t)

bigFun : Nat -> Nat -> Nat -> Nat
bigFun i j k = let
  f x y = i + x + y
  g x y = j + x + y
  h x y = k + x + y
  f j k + g i k + h i j

tests : '{io2.IO} [Result]
tests =
  '[ extensionality "ext f" (t x -> handle f x with zapper t)
   , extensionality "ext h" h
   , identicality "ident compound" (x -> handle f x with zapper (zero 5))
   , identicality "ident fib10" fib10
   , identicality "ident effect" (_ -> zap)
   , identicality "ident zero" zero
   , identicality "ident h" h
   , identicality "ident text" "hello"
   , identicality "ident int" +5
   , identicality "ident float" 0.5
   , identicality "ident termlink" fDeps
   , identicality "ident bool" false
   , identicality "ident bytes" [fSer, Bytes.empty]
   ]

badLoad : '{IO} [Result]
badLoad _ =
  payload = Bytes.fromList[0,0,0,3,0,1,64,175,174,29,188,217,78,209,175,255,137,165,135,165,1,20,151,182,215,54,21,196,43,159,247,106,175,177,213,20,111,178,134,214,188,207,243,196,240,187,111,44,245,111,219,223,98,88,183,163,97,22,18,153,104,185,125,175,157,36,209,151,166,168,102,0,1,0,0,0,0,0,2,0,0,0,0]
  go _ =
    match Value.deserialize payload with
      Left t -> Fail "deserialize exception"
      Right a -> match Value.load a with
        Left terms ->
            bs = Value.serialize (Value.value terms)
            s = size bs
            Ok ("serialized" ++ toText s)
        Right _ ->
            Ok "actually loaded"
  match toEither go with
    Right v -> [v]
    Left _ -> [Fail "Exception"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      structural ability Zap
      badLoad : '{IO} [Result]
      bigFun  : Nat -> Nat -> Nat -> Nat
      f       : Nat ->{Zap} Nat
      fDeps   : [Link.Term]
      fSer    : Bytes
      fVal    : Value
      h       : Three Nat Nat Nat -> Nat -> Nat
      rotate  : Three Nat Nat Nat -> Three Nat Nat Nat
      tests   : '{IO} [Result]
      zapper  : Three Nat Nat Nat -> Request {Zap} r -> r
```

This simply runs some functions to make sure there isn't a crash. Once
we gain the ability to capture output in a transcript, it can be modified
to actual show that the serialization works.

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    structural ability Zap
    badLoad : '{IO} [Result]
    bigFun  : Nat -> Nat -> Nat -> Nat
    f       : Nat ->{Zap} Nat
    fDeps   : [Link.Term]
    fSer    : Bytes
    fVal    : Value
    h       : Three Nat Nat Nat -> Nat -> Nat
    rotate  : Three Nat Nat Nat -> Three Nat Nat Nat
    tests   : '{IO} [Result]
    zapper  : Three Nat Nat Nat -> Request {Zap} r -> r

scratch/main> io.test tests

    New test results:

    1. tests   ◉ (ext f) passed
               ◉ (ext h) passed
               ◉ (ident compound) passed
               ◉ (ident fib10) passed
               ◉ (ident effect) passed
               ◉ (ident zero) passed
               ◉ (ident h) passed
               ◉ (ident text) passed
               ◉ (ident int) passed
               ◉ (ident float) passed
               ◉ (ident termlink) passed
               ◉ (ident bool) passed
               ◉ (ident bytes) passed

  ✅ 13 test(s) passing

  Tip: Use view 1 to view the source of a test.

scratch/main> io.test badLoad

    New test results:

    1. badLoad   ◉ serialized77

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

``` unison
codeTests : '{io2.IO} [Result]
codeTests =
  '[ idempotence "idem f" (termLink f)
   , idempotence "idem h" (termLink h)
   , idempotence "idem rotate" (termLink rotate)
   , idempotence "idem zapper" (termLink zapper)
   , idempotence "idem showThree" (termLink showThree)
   , idempotence "idem concatMap" (termLink concatMap)
   , idempotence "idem big" (termLink bigFun)
   , idempotence "idem extensionality" (termLink extensionality)
   , idempotence "idem identicality" (termLink identicality)
   , verified "f" (termLink f)
   , verified "h" (termLink h)
   , verified "rotate" (termLink rotate)
   , verified "zapper" (termLink zapper)
   , verified "showThree" (termLink showThree)
   , verified "concatMap" (termLink concatMap)
   , verified "big" (termLink bigFun)
   , verified "extensionality" (termLink extensionality)
   , verified "identicality" (termLink identicality)
   , verified "mutual0" (termLink mutual0)
   , verified "mutual1" (termLink mutual0)
   , verified "mutual2" (termLink mutual0)
   , missed "mutual0" (termLink mutual0)
   , missed "mutual1" (termLink mutual1)
   , missed "mutual2" (termLink mutual2)
   , swapped "zapper" (termLink zapper)
   , swapped "extensionality" (termLink extensionality)
   , swapped "identicality" (termLink identicality)
   , swapped "mututal0" (termLink mutual0)
   , swapped "mututal1" (termLink mutual1)
   , swapped "mututal2" (termLink mutual2)
   ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      codeTests : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    codeTests : '{IO} [Result]

scratch/main> io.test codeTests

    New test results:

    1. codeTests   ◉ (idem f) passed
                   ◉ (idem h) passed
                   ◉ (idem rotate) passed
                   ◉ (idem zapper) passed
                   ◉ (idem showThree) passed
                   ◉ (idem concatMap) passed
                   ◉ (idem big) passed
                   ◉ (idem extensionality) passed
                   ◉ (idem identicality) passed
                   ◉ (verified f) passed
                   ◉ (verified h) passed
                   ◉ (verified rotate) passed
                   ◉ (verified zapper) passed
                   ◉ (verified showThree) passed
                   ◉ (verified concatMap) passed
                   ◉ (verified big) passed
                   ◉ (verified extensionality) passed
                   ◉ (verified identicality) passed
                   ◉ (verified mutual0) passed
                   ◉ (verified mutual1) passed
                   ◉ (verified mutual2) passed
                   ◉ (rejected missing mutual0) passed
                   ◉ (rejected missing mutual1) passed
                   ◉ (rejected missing mutual2) passed
                   ◉ (rejected swapped zapper) passed
                   ◉ (rejected swapped extensionality) passed
                   ◉ (rejected swapped identicality) passed
                   ◉ (rejected swapped mututal0) passed
                   ◉ (rejected swapped mututal1) passed
                   ◉ (rejected swapped mututal2) passed

  ✅ 30 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

``` unison
validateTest : Link.Term ->{IO} Result
validateTest l = match Code.lookup l with
  None -> Fail "Couldn't look up link"
  Some co -> match Code.validate [(l, co)] with
    Some f -> Fail "invalid code pre"
    None -> match Code.deserialize (Code.serialize co) with
      Left _ -> Fail "code failed deserialization"
      Right co -> match Code.validate [(l, co)] with
        Some f -> Fail "invalid code post"
        None -> Ok "validated"

vtests : '{IO} [Result]
vtests _ =
  List.map validateTest
    [ termLink fib10
    , termLink compose
    , termLink List.all
    , termLink hex
    , termLink isDirectory
    , termLink delay
    , termLink printLine
    , termLink isNone
    ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      validateTest : Link.Term ->{IO} Result
      vtests       : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    validateTest : Link.Term ->{IO} Result
    vtests       : '{IO} [Result]

scratch/main> io.test vtests

    New test results:

    1. vtests   ◉ validated
                ◉ validated
                ◉ validated
                ◉ validated
                ◉ validated
                ◉ validated
                ◉ validated
                ◉ validated

  ✅ 8 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
