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

``` ucm
scratch/main> add
scratch/main> io.test testABunchOfNats
```
