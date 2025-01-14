Test for new Text -> Bytes conversions explicitly using UTF-8 as the encoding

Unison has function for converting between `Text` and a UTF-8 `Bytes` encoding of the Text.

``` ucm
scratch/main> find Utf8
```

ascii characters are encoded as single bytes (in the range 0-127).

``` unison
ascii: Text
ascii = "ABCDE"

> toUtf8 ascii

```

non-ascii characters are encoded as multiple bytes.

``` unison
greek: Text
greek = "ΑΒΓΔΕ"

> toUtf8 greek
```

We can check that encoding and then decoding should give us back the same `Text` we started with

``` unison
checkRoundTrip: Text -> [Result]
checkRoundTrip t =
  bytes = toUtf8 t
  match fromUtf8.impl bytes with
    Left e -> [Result.Fail "could not decode"]
    Right t' -> if t == t' then [Result.Ok "Passed"] else [Result.Fail ("Got: " ++ t' ++ " Expected: " ++ t)]

greek = "ΑΒΓΔΕ"

test> greekTest = checkRoundTrip greek
```

If we try to decode an invalid set of bytes, we get back `Text` explaining the decoding error:

``` unison
greek_bytes = Bytes.fromList [206, 145, 206, 146, 206, 147, 206, 148, 206]


-- Its an error if we drop the first byte
> match fromUtf8.impl (drop 1 greek_bytes) with
  Left (Failure _ t _) -> t
  _ -> bug "expected a left"

```
