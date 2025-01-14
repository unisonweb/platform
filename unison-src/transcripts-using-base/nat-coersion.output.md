``` unison

testNat: Nat -> Optional Int -> Optional Float -> {Stream Result}()
testNat n expectInt expectFloat =
  float = Float.fromRepresentation n
  int = Int.fromRepresentation n

  n2 = Float.toRepresentation float
  n3 = Int.toRepresentation int

  match expectFloat with
    None -> emit (Ok "skipped")
    Some expect -> expectU ("expected " ++ (Float.toText expect) ++ " got " ++ (Float.toText float)) expect float
                   expectU ("round trip though float, expected " ++ (Nat.toText n) ++ " got " ++ (Nat.toText n2)) n n2

  match expectInt with
    None -> emit (Ok "skipped")
    Some expect -> expectU ("expected " ++ (Int.toText expect) ++ " got " ++ (Int.toText int)) expect int
                   expectU ("round trip though Int, expected " ++ (Nat.toText n) ++ " got " ++ (Nat.toText n3)) n n3



test: '{io2.IO}[Result]
test = 'let
   testABunchOfNats: '{Stream Result}()
   testABunchOfNats _ =
     testNat 0 (Some +0) (Some 0.0)
     testNat 1 (Some +1) None
     testNat 18446744073709551615 (Some -1) None  -- we don't have a way of expressing Nan
     testNat 0x3FF0000000000001 (Some +4607182418800017409) (Some 1.0000000000000002 )

   runTest testABunchOfNats
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      test    : '{IO} [Result]
      testNat : Nat
                -> Optional Int
                -> Optional Float
                ->{Stream Result} ()
```

``` ucm
scratch/main> add

  ⍟ I've added these definitions:

    test    : '{IO} [Result]
    testNat : Nat
              -> Optional Int
              -> Optional Float
              ->{Stream Result} ()

scratch/main> io.test test

    New test results:

    1. test   ◉ expected 0.0 got 0.0
              ◉ round trip though float, expected 0 got 0
              ◉ expected 0 got 0
              ◉ round trip though Int, expected 0 got 0
              ◉ skipped
              ◉ expected 1 got 1
              ◉ round trip though Int, expected 1 got 1
              ◉ skipped
              ◉ expected -1 got -1
              ◉ round trip though Int, expected 18446744073709551615 got 18446744073709551615
              ◉ expected 1.0000000000000002 got 1.0000000000000002
              ◉ round trip though float, expected 4607182418800017409 got 4607182418800017409
              ◉ expected 4607182418800017409 got 4607182418800017409
              ◉ round trip though Int, expected 4607182418800017409 got 4607182418800017409

  ✅ 14 test(s) passing

  Tip: Use view 1 to view the source of a test.
```
