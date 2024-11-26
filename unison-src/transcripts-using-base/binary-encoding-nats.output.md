``` unison
unique type EncDec = EncDec Text (Nat -> Bytes) (Bytes -> Optional (Nat, Bytes))

BE64 = EncDec "64 bit Big Endian" encodeNat64be decodeNat64be
LE64 = EncDec "64 bit Little Endian" encodeNat64le decodeNat64le
BE32 = EncDec "32 bit Big Endian" encodeNat32be decodeNat32be
LE32 = EncDec "32 bit Little Endian" encodeNat32le decodeNat32le
BE16 = EncDec "16 bit Big Endian" encodeNat16be decodeNat16be
LE16 = EncDec "16 bit Little Endian" encodeNat16le decodeNat16le

testRoundTrip : Nat -> EncDec -> {IO, Stream Result} ()
testRoundTrip n = cases
  EncDec label enc dec ->
    encoded = enc n
    match dec encoded with
      Some (n', remain) ->
        if n == n' then
          emit (Ok ("successfully decoded " ++ (toText n) ++ " using " ++ label))
        else
          emit (Fail ("decoded " ++ (toText n') ++ " instead of " ++ (toText n) ++ " using " ++ label))
        if (size remain) > 0 then
          emit (Fail ("unconsumed input using " ++ label))
        else
          emit (Ok ("consumed all input"))
      None -> emit (Fail ("failed to decode " ++ (toText n) ++ " using " ++ label))

testNat : Nat -> '{IO, Stream Result} ()
testNat n _ =
  if n >= (shiftLeft 1 32) then
    testRoundTrip n BE64
    testRoundTrip n LE64
  else if n >= (shiftLeft 1 16) then
    testRoundTrip n BE64
    testRoundTrip n LE64
    testRoundTrip n BE32
    testRoundTrip n LE32
  else
    testRoundTrip n BE64
    testRoundTrip n LE64
    testRoundTrip n BE32
    testRoundTrip n LE32
    testRoundTrip n BE16
    testRoundTrip n LE16


testABunchOfNats _ =
  (runTest (testNat 0xFFFFFFFF)) ++
  (runTest (testNat 0x41000000)) ++
  (runTest (testNat 0x00410000)) ++
  (runTest (testNat 0x00004100)) ++
  (runTest (testNat 0x86753099)) ++
  (runTest (testNat 0x00000041)) ++
  (runTest (testNat 0))
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      type EncDec
      BE16             : EncDec
      BE32             : EncDec
      BE64             : EncDec
      LE16             : EncDec
      LE32             : EncDec
      LE64             : EncDec
      testABunchOfNats : ∀ _. _ ->{IO} [Result]
      testNat          : Nat -> '{IO, Stream Result} ()
      testRoundTrip    : Nat -> EncDec ->{IO, Stream Result} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    type EncDec
    BE16             : EncDec
    BE32             : EncDec
    BE64             : EncDec
    LE16             : EncDec
    LE32             : EncDec
    LE64             : EncDec
    testABunchOfNats : ∀ _. _ ->{IO} [Result]
    testNat          : Nat -> '{IO, Stream Result} ()
    testRoundTrip    : Nat -> EncDec ->{IO, Stream Result} ()

scratch/main> io.test testABunchOfNats

    New test results:

    1. testABunchOfNats   ◉ successfully decoded 4294967295 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4294967295 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4294967295 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4294967295 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 1090519040 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 1090519040 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 1090519040 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 1090519040 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4259840 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4259840 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4259840 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 4259840 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 16 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 16640 using 16 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 2255827097 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 2255827097 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 2255827097 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 2255827097 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 16 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 65 using 16 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 64 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 64 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 32 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 32 bit Little Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 16 bit Big Endian
                          ◉ consumed all input
                          ◉ successfully decoded 0 using 16 bit Little Endian
                          ◉ consumed all input

  ✅ 68 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
