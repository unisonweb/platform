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

``` ucm
scratch/main> add
scratch/main> io.test test
```
