# Unit tests for builtin functions

``` ucm :hide
scratch/main> builtins.mergeio

scratch/main> load "unison-src/transcripts-using-base/base.u"

scratch/main> add
```

This transcript defines unit tests for builtin functions. There's a single `scratch/main> test` execution at the end that will fail the transcript with a nice report if any of the tests fail.

## `Int` functions

``` unison :hide
use Int

-- used for some take/drop tests later
bigN = Nat.shiftLeft 1 63

-- Note: you can make the tests more fine-grained if you
-- want to be able to tell which one is failing
test> Int.tests.arithmetic =
      checks [
        eq (+1 + +1) +2,
        +10 - +4 == +6,
        eq (+11 * +6) +66,
        eq (+11 * +6) +66,
        +10 / +3 == +3,
        +10 / +5 == +2,
        mod +10 +3 == +1,
        mod +10 +2 == +0,
        mod -13 +3 == +2,
        mod -13 -3 == -1,
        mod -13 -5 == -3,
        mod -13 +5 == +2,
        negate +99 == -99,
        increment +99 == +100,
        not (isEven +99),
        isEven +100,
        isOdd +105,
        not (isOdd +108),
        signum +99 == +1,
        signum -3949 == -1,
        signum +0 == +0,
        gt +42 -1,
        lt +42 +1000,
        lteq +43 +43,
        lteq +43 +44,
        gteq +43 +43,
        gteq +43 +41
        ]

test> Int.tests.bitTwiddling =
      checks [
        and +5 +4 == +4,
        and +5 +1 == +1,
        or +4 +1 == +5,
        xor +5 +1 == +4,
        complement -1 == +0,
        popCount +1 == 1,
        popCount +2 == 1,
        popCount +4 == 1,
        popCount +5 == 2,
        popCount -1 == 64,
        leadingZeros +1 == 63,
        trailingZeros +1 == 0,
        leadingZeros +2 == 62,
        trailingZeros +2 == 1,
        pow +2 6 == +64,
        shiftLeft +1 6 == +64,
        shiftRight +64 6 == +1
        ]

test> Int.tests.conversions =
      checks [
        truncate0 -2438344 == 0,
        truncate0 +999 == 999,
        toText +0 == "0",
        toText +10 == "10",
        toText -1039 == "-1039",
        fromText "+0" == Some +0,
        fromText "a8f9djasdlfkj" == None,
        fromText "3940" == Some +3940,
        fromText "1000000000000000000000000000" == None,
        fromText "-1000000000000000000000000000" == None,
        toFloat +9394 == 9394.0,
        toFloat -20349 == -20349.0
        ]
```

``` ucm :hide
scratch/main> add
```

## `Nat` functions

``` unison :hide
use Nat

test> Nat.tests.arithmetic =
      checks [
        eq (1 + 1) 2,
        drop 10 4 == 6,
        sub 10 12 == -2,
        eq (11 * 6) 66,
        10 / 3 == 3,
        10 / 5 == 2,
        mod 10 3 == 1,
        mod 10 2 == 0,
        18446744073709551615 / 2 == 9223372036854775807,
        mod 18446744073709551615 2 == 1,
        increment 99 == 100,
        not (isEven 99),
        isEven 100,
        isOdd 105,
        not (isOdd 108),
        gt 42 1,
        lt 42 1000,
        lteq 43 43,
        lteq 43 44,
        gteq 43 43,
        gteq 43 41,
        ]

test> Nat.tests.bitTwiddling =
      checks [
        and 5 4 == 4,
        and 5 1 == 1,
        or 4 1 == 5,
        xor 5 1 == 4,
        complement (complement 0) == 0,
        popCount 1 == 1,
        popCount 2 == 1,
        popCount 4 == 1,
        popCount 5 == 2,
        popCount (complement 0) == 64,
        leadingZeros 1 == 63,
        trailingZeros 1 == 0,
        leadingZeros 2 == 62,
        trailingZeros 2 == 1,
        pow 2 6 == 64,
        shiftLeft 1 6 == 64,
        shiftRight 64 6 == 1
        ]

test> Nat.tests.conversions =
      checks [
        toFloat 2438344 == 2438344.0,
        toFloat 0 == 0.0,
        toText 0 == "0",
        toText 32939 == "32939",
        toText 10 == "10",
        fromText "ooga" == None,
        fromText "90" == Some 90,
        fromText "-1" == None,
        fromText "100000000000000000000000000" == None,
        unsnoc "abc" == Some ("ab", ?c),
        uncons "abc" == Some (?a, "bc"),
        unsnoc "" == None,
        uncons "" == None,
        Text.fromCharList (Text.toCharList "abc") == "abc",
        Bytes.fromList (Bytes.toList 0xsACE0BA5E) == 0xsACE0BA5E
        ]
```

``` ucm :hide
scratch/main> add
```

## `Boolean` functions

``` unison :hide
test> Boolean.tests.orTable =
      checks [
        (true || true) == true,
        (true || false) == true,
        (false || true) == true,
        (false || false) == false
      ]
test> Boolean.tests.andTable =
      checks [
        (true && true) == true,
        (false && true) == false,
        (true && false) == false,
        (false && false) == false
      ]
test> Boolean.tests.notTable =
      checks [
        not true == false,
        not false == true
      ]
```

``` ucm :hide
scratch/main> add
```

## `Text` functions

``` unison :hide
test> Text.tests.takeDropAppend =
      checks [
        "yabba" ++ "dabba" == "yabbadabba",
        Text.take 0 "yabba" == "",
        Text.take 2 "yabba" == "ya",
        Text.take 99 "yabba" == "yabba",
        Text.drop 0 "yabba" == "yabba",
        Text.drop 2 "yabba" == "bba",
        Text.drop 99 "yabba" == "",
        Text.take bigN "yabba" == "yabba",
        Text.drop bigN "yabba" == ""
        ]

test> Text.tests.repeat =
      checks [
        Text.repeat 4 "o" == "oooo",
        Text.repeat 0 "o" == ""
      ]

test> Text.tests.alignment =
      checks [
        Text.alignLeftWith 5 ?\s "a" == "a    ",
        Text.alignRightWith 5 ?_ "ababa" == "ababa",
        Text.alignRightWith 5 ?_ "ab" == "___ab"
      ]

test> Text.tests.literalsEq = checks [":)" == ":)"]

test> Text.tests.patterns =
  use Pattern many or run isMatch capture join replicate
  use Text.patterns literal digit letter anyChar space punctuation notCharIn charIn charRange notCharRange eof
  l = literal
  checks [
    run digit "1abc" == Some ([], "abc"),
    run (capture (many digit)) "11234abc" == Some (["11234"], "abc"),
    run (many letter) "abc11234abc" == Some ([], "11234abc"),
    run (join [many space, capture (many anyChar)]) "   abc123" == Some (["abc123"], ""),
    run (many punctuation) "!!!!,,,..." == Some ([], ""),
    run (charIn [?0,?1]) "0" == Some ([], ""),
    run (notCharIn [?0,?1]) "0" == None,
    run (many (notCharIn [?0,?1])) "asjdfskdfjlskdjflskdjf011" == Some ([], "011"),
    run (capture (many (charRange ?a ?z))) "hi123" == Some (["hi"], "123"),
    run (capture (many (notCharRange ?, ?,))) "abc123," == Some (["abc123"], ","),
    run (capture (many (notCharIn [?,,]))) "abracadabra,123" == Some (["abracadabra"], ",123"),
    run (capture (many (or digit letter))) "11234abc,remainder" == Some (["11234abc"], ",remainder"),
    run (capture (replicate 1 5 (or digit letter))) "1a2ba aaa" == Some (["1a2ba"], " aaa"),
    run (captureAs "foo" (many (or digit letter))) "11234abc,remainder" == Some (["foo"], ",remainder"),
    run (join [(captureAs "foo" (many digit)), captureAs "bar" (many letter)]) "11234abc,remainder" == Some (["foo", "bar"], ",remainder"),
    -- Regression test for: https://github.com/unisonweb/unison/issues/3530
    run (capture (replicate 0 1 (join [literal "a", literal "b"]))) "ac" == Some ([""], "ac"),
    isMatch (join [many letter, eof]) "aaaaabbbb" == true,
    isMatch (join [many letter, eof]) "aaaaabbbb1" == false,
    isMatch (join [l "abra", many (l "cadabra")]) "abracadabracadabra" == true,

  ]


test> Text.tests.indexOf =
   haystack = "01020304" ++ "05060708" ++ "090a0b0c01"
   needle1 = "01"
   needle2 = "02"
   needle3 = "0304"
   needle4 = "05"
   needle5 = "0405"
   needle6 = "0c"
   needle7 = haystack
   needle8 = "lopez"
   needle9 = ""
   checks [
     Text.indexOf needle1 haystack == Some 0,
     Text.indexOf needle2 haystack == Some 2,
     Text.indexOf needle3 haystack == Some 4,
     Text.indexOf needle4 haystack == Some 8,
     Text.indexOf needle5 haystack == Some 6,
     Text.indexOf needle6 haystack == Some 22,
     Text.indexOf needle7 haystack == Some 0,
     Text.indexOf needle8 haystack == None,
     Text.indexOf needle9 haystack == Some 0,
   ]

test> Text.tests.indexOfEmoji =
  haystack = "clap 👏 your 👏 hands 👏 if 👏 you 👏 love 👏 unison"
  needle1 = "👏"
  needle2 = "👏 "
  checks [
    Text.indexOf needle1 haystack == Some 5,
    Text.indexOf needle2 haystack == Some 5,
  ]

```

``` ucm :hide
scratch/main> add
```

## `Bytes` functions

``` unison :hide
test> Bytes.tests.at =
        bs = Bytes.fromList [77, 13, 12]
        checks [
          Bytes.at 1 bs == Some 13,
          Bytes.at 0 bs == Some 77,
          Bytes.at 99 bs == None,
          Bytes.take bigN bs == bs,
          Bytes.drop bigN bs == empty
        ]

test> Bytes.tests.compression =
        roundTrip b =
          (Bytes.zlib.decompress (Bytes.zlib.compress b) == Right b)
            && (Bytes.gzip.decompress (Bytes.gzip.compress b) == Right b)

        checks [
          roundTrip 0xs2093487509823745709827345789023457892345,
          roundTrip 0xs00000000000000000000000000000000000000000000,
          roundTrip 0xs,
          roundTrip 0xs11111111111111111111111111,
          roundTrip 0xsffffffffffffffffffffffffffffff,
          roundTrip 0xs222222222fffffffffffffffffffffffffffffff,
          -- these fail due to bad checksums and/or headers
          isLeft (zlib.decompress 0xs2093487509823745709827345789023457892345),
          isLeft (gzip.decompress 0xs201209348750982374593939393939709827345789023457892345)
        ]

test> Bytes.tests.fromBase64UrlUnpadded =
  checks [Exception.catch
           '(fromUtf8
              (raiseMessage () (Bytes.fromBase64UrlUnpadded (toUtf8 "aGVsbG8gd29ybGQ")))) == Right "hello world"
         , isLeft (Bytes.fromBase64UrlUnpadded (toUtf8 "aGVsbG8gd29ybGQ="))]

test> Bytes.tests.indexOf =
   haystack = 0xs01020304 ++ 0xs05060708 ++ 0xs090a0b0c01
   needle1 = 0xs01
   needle2 = 0xs02
   needle3 = 0xs0304
   needle4 = 0xs05
   needle5 = 0xs0405
   needle6 = 0xs0c
   needle7 = haystack
   needle8 = 0xsffffff
   checks [
     Bytes.indexOf needle1 haystack == Some 0,
     Bytes.indexOf needle2 haystack == Some 1,
     Bytes.indexOf needle3 haystack == Some 2,
     Bytes.indexOf needle4 haystack == Some 4,
     Bytes.indexOf needle5 haystack == Some 3,
     Bytes.indexOf needle6 haystack == Some 11,
     Bytes.indexOf needle7 haystack == Some 0,
     Bytes.indexOf needle8 haystack == None,

   ]

```

``` ucm :hide
scratch/main> add
```

## `List` comparison

``` unison :hide
test> checks [
        compare [] [1,2,3] == -1,
        compare [1,2,3] [1,2,3,4] == -1,
        compare [1,2,3,4] [1,2,3] == +1,
        compare [1,2,3] [1,2,3] == +0,
        compare [3] [1,2,3] == +1,
        compare [1,2,3] [1,2,4] == -1,
        compare [1,2,2] [1,2,1,2] == +1,
        compare [1,2,3,4] [3,2,1] == -1
      ]
```

``` ucm :hide
scratch/main> add
```

Other list functions

``` unison :hide
test> checks [
        List.take bigN [1,2,3] == [1,2,3],
        List.drop bigN [1,2,3] == []
      ]
```

## `Any` functions

``` unison
> [Any "hi", Any (41 + 1)]

test> Any.test1 = checks [(Any "hi" == Any "hi")]
test> Any.test2 = checks [(not (Any "hi" == Any 42))]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Any.test1 : [Result]
      Any.test2 : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > [Any "hi", Any (41 + 1)]
          ⧩
          [Any "hi", Any 42]

    3 | test> Any.test1 = checks [(Any "hi" == Any "hi")]
    
    ✅ Passed Passed

    4 | test> Any.test2 = checks [(not (Any "hi" == Any 42))]
    
    ✅ Passed Passed
```

``` ucm :hide
scratch/main> add
```

## Sandboxing functions

``` unison
openFile1 t = openFile t
openFile2 t = openFile1 t

validateSandboxedSimpl ok v =
  match Value.validateSandboxed ok v with
    Right [] -> true
    _ -> false

openFiles =
  [ not (validateSandboxed [] openFile)
  , not (validateSandboxed [] openFile1)
  , not (validateSandboxed [] openFile2)
  ]

test> Sandbox.test1 = checks [validateSandboxed [] "hello"]
test> Sandbox.test2 = checks openFiles
test> Sandbox.test3 = checks [validateSandboxed [termLink openFile.impl]
openFile]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Sandbox.test1          : [Result]
      Sandbox.test2          : [Result]
      Sandbox.test3          : [Result]
      openFile1              : Text
                               -> FileMode
                               ->{IO, Exception} Handle
      openFile2              : Text
                               -> FileMode
                               ->{IO, Exception} Handle
      openFiles              : [Boolean]
      validateSandboxedSimpl : [Link.Term]
                               -> Value
                               ->{IO} Boolean

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    15 | test> Sandbox.test1 = checks [validateSandboxed [] "hello"]
    
    ✅ Passed Passed

    16 | test> Sandbox.test2 = checks openFiles
    
    ✅ Passed Passed

    17 | test> Sandbox.test3 = checks [validateSandboxed [termLink openFile.impl]
    
    ✅ Passed Passed
```

``` ucm :hide
scratch/main> add
```

``` unison
openFilesIO = do
  checks
    [ not (validateSandboxedSimpl [] (value openFile))
    , not (validateSandboxedSimpl [] (value openFile1))
    , not (validateSandboxedSimpl [] (value openFile2))
    , sandboxLinks (termLink openFile)
        == sandboxLinks (termLink openFile1)
    , sandboxLinks (termLink openFile1)
        == sandboxLinks (termLink openFile2)
    ]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      openFilesIO : '{IO} [Result]
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    openFilesIO : '{IO} [Result]

scratch/main> io.test openFilesIO

    New test results:

    1. openFilesIO   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

## Universal hash functions

Just exercises the function

``` unison
> Universal.murmurHash 1
test> Universal.murmurHash.tests = checks [Universal.murmurHash [1,2,3] == Universal.murmurHash [1,2,3]]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      Universal.murmurHash.tests : [Result]

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > Universal.murmurHash 1
          ⧩
          1208954131003843843

    2 | test> Universal.murmurHash.tests = checks [Universal.murmurHash [1,2,3] == Universal.murmurHash [1,2,3]]
    
    ✅ Passed Passed
```

``` ucm :hide
scratch/main> add
```

## Run the tests

Now that all the tests have been added to the codebase, let's view the test report. This will fail the transcript (with a nice message) if any of the tests are failing.

``` ucm
scratch/main> test

  Cached test results (`help testcache` to learn more)

    1.  Any.test1                           ◉ Passed
    2.  Any.test2                           ◉ Passed
    3.  Boolean.tests.andTable              ◉ Passed
    4.  Boolean.tests.notTable              ◉ Passed
    5.  Boolean.tests.orTable               ◉ Passed
    6.  Bytes.tests.at                      ◉ Passed
    7.  Bytes.tests.compression             ◉ Passed
    8.  Bytes.tests.fromBase64UrlUnpadded   ◉ Passed
    9.  Bytes.tests.indexOf                 ◉ Passed
    10. Int.tests.arithmetic                ◉ Passed
    11. Int.tests.bitTwiddling              ◉ Passed
    12. Int.tests.conversions               ◉ Passed
    13. Nat.tests.arithmetic                ◉ Passed
    14. Nat.tests.bitTwiddling              ◉ Passed
    15. Nat.tests.conversions               ◉ Passed
    16. Sandbox.test1                       ◉ Passed
    17. Sandbox.test2                       ◉ Passed
    18. Sandbox.test3                       ◉ Passed
    19. test.rtjqan7bcs                     ◉ Passed
    20. Text.tests.alignment                ◉ Passed
    21. Text.tests.indexOf                  ◉ Passed
    22. Text.tests.indexOfEmoji             ◉ Passed
    23. Text.tests.literalsEq               ◉ Passed
    24. Text.tests.patterns                 ◉ Passed
    25. Text.tests.repeat                   ◉ Passed
    26. Text.tests.takeDropAppend           ◉ Passed
    27. Universal.murmurHash.tests          ◉ Passed

  ✅ 27 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
