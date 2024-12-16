This gives a confusing error for a subtle reason.

``` unison :error
type CompressionMethod =

type ConnectionState = { compressionMethod: CompressionMethod}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        3 | type ConnectionState = { compressionMethod: CompressionMethod}
    
```

It's confusing because there's no case shown (the pattern-matching code at issue is synthetic and hidden), and no preceding case(s) either.

Here it is without any record syntax:

``` unison :error
type X =

type A = A1 X

ax = cases 
  A1 x -> x
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  This case would be ignored because it's already covered by the preceding case(s):
        6 |   A1 x -> x
    
```

It's trying to say that the pattern `A1 x` can never be matched because `A1` can never be used, because `X` can't be instantiated, because there are no constructors for it.
