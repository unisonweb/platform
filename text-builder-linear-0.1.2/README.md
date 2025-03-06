# text-builder-linear [![Hackage](http://img.shields.io/hackage/v/text-builder-linear.svg)](https://hackage.haskell.org/package/text-builder-linear) [![Stackage LTS](http://stackage.org/package/text-builder-linear/badge/lts)](http://stackage.org/lts/package/text-builder-linear) [![Stackage Nightly](http://stackage.org/package/text-builder-linear/badge/nightly)](http://stackage.org/nightly/package/text-builder-linear)

_Linear types for linear times!_

Builder for strict `Text` and `ByteString`, based on linear types. It consistently
outperforms lazy `Builder` from `text` as well as a strict builder from `text-builder`,
and scales better.

## Example

```haskell
> :set -XOverloadedStrings
> import Data.Text.Builder.Linear
> fromText "foo" <> fromChar '_' <> fromDec (42 :: Int)
"foo_42"
```

## Design

String builders in Haskell serve the same purpose as `StringBuilder` in Java to prevent
quadratic slow down in concatenation.

Classic builders such as `Data.Text.Lazy.Builder` are lazy and fundamentally are
[`dlist`](https://hackage.haskell.org/package/dlist) with bells and whistles:
instead of actually concatenating substrings we compose actions, which implement
concatenation, building a tree of thunks. The tree can be forced partially, left-to-right,
producing chunks of strict `Text`, combined into a lazy one. Neither input, nor output need to be materialized in full, which potentially allows for fusion. Such builders allow
linear time complexity, but constant factors are relatively high, because thunks are
expensive. To a certain degree this is mitigated by inlining, which massively reduces
number of nodes.

Strict builders such as [`text-builder`](https://hackage.haskell.org/package/text-builder)
offer another design: they first inspect their input in full to determine output length,
then allocate a buffer of required size and fill it in one go. If everything inlines nicely,
the length may be known in compile time, which gives blazingly fast runtime. In more
complex cases it still builds a tree of thunks and forces all inputs to be materialized.

This package offers two interfaces. One is a mutable `Buffer` with linear API,
which operates very similar to `StringBuilder` in Java. It allocates a buffer
with extra space at the ends to append new strings. If there is not enough free space
to insert new data, it allocates a twice larger buffer and copies itself there.
The dispatch happens in runtime, so we do not need to inspect and materialize all inputs
beforehand; and inlining is mostly irrelevant.
Exponential growth provides for amortized linear time.
Such structure can be implemented without linear types, but that would
greatly affect user experience by polluting everything with `ST` monad.
Users are encouraged to use `Buffer` API, and built-in benchmarks refer to it.

The second interface is more traditional `newtype Builder = Builder (Buffer ⊸ Buffer)`
with `Monoid` instance. This type provides easy migration from other builders,
but may suffer from insufficient inlining, allocating a tree of thunks. It is still
significantly faster than `Data.Text.Lazy.Builder`, as witnessed by benchmarks
for `blaze-builder` below.

## Case study

Let's benchmark builders, which concatenate all `Char` from `minBound` to `maxBound`, producing a large `Text`:

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, tasty-bench, text, text-builder, text-builder-linear
ghc-options: -O2
-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Text.Builder as TB
import qualified Data.Text.Builder.Linear as TBL
import System.Environment (getArgs)
import Test.Tasty.Bench

mkBench :: Monoid a => String -> (Char -> a) -> (a -> Int) -> Benchmark
mkBench s f g = bench s $ nf (g . foldMap f . enumFromTo minBound) maxBound
{-# INLINE mkBench #-}

main :: IO ()
main = defaultMain
  [ mkBench "text, lazy" TLB.singleton (fromIntegral . TL.length . TLB.toLazyText)
  , mkBench "text, strict" TLB.singleton (T.length . TL.toStrict . TLB.toLazyText)
  , mkBench "text-builder" TB.char (T.length . TB.run)
  , mkBench "text-builder-linear" TBL.fromChar (T.length . TBL.runBuilder)
  ]
```

Running this program with `cabal run Main.hs -- +RTS -T` yields following results:

```
text, lazy:
  4.25 ms ± 107 μs,  11 MB allocated, 912 B  copied
text, strict:
  7.18 ms ± 235 μs,  24 MB allocated,  10 MB copied
text-builder:
  80.1 ms ± 3.0 ms, 218 MB allocated, 107 MB copied
text-builder-linear:
  5.37 ms ± 146 μs,  44 MB allocated,  78 KB copied
```

The first result seems the best both in time and memory and corresponds to the
usual `Text` builder, where we do not materialize the entire result at all.
It builds chunks of lazy `Text` lazily and consumes them at once by
`TL.length`. Thus there are 11 MB of allocations in nursery, none of which
survive generation 0 garbage collector, so nothing is copied.

The second result is again the usual `Text` builder, but emulates a strict
consumer: we materialize a strict `Text` before computing length. Allocation
are doubled, and half of them (corresponding to the strict `Text`) survive to
the heap. Time is also almost twice longer, but still quite good.

The third result is for `text-builder` and demonstrates how bad things could
go with strict builders, aiming to precompute the precise length of the
buffer: allocating a thunk per char is tremendously slow and expensive.

The last result corresponds to the current package. We generate a strict
`Text` by growing and reallocating the buffer, thus allocations are quite
high. Nevertheless, it is already faster than the usual `Text` builder with
strict consumer and does not strain the garbage collector.

Things get very different if we remove `{-# INLINE mkBench #-}`:

```
text, lazy:
  36.9 ms ± 599 μs, 275 MB allocated,  30 KB copied
text, strict:
  44.7 ms ± 1.3 ms, 287 MB allocated,  25 MB copied
text-builder:
  77.6 ms ± 2.2 ms, 218 MB allocated, 107 MB copied
text-builder-linear:
  5.35 ms ± 212 μs,  44 MB allocated,  79 KB copied
```

Builders from `text` package degrade rapidly, 6-8x slower and 10-20x more
allocations. That's because their constant factors rely crucially on
everything getting inlined, which makes their performance fragile and
unreliable in large-scale applications. On the bright side of things, our
builder remains as fast as before and now is a clear champion.

## Benchmarks for `Text`

Measured with GHC 9.6 on aarch64:

|Group / size|`text`|`text-builder`|  |This package|  |
|------------|-----:|-------------:|-:|-----------:|-:|
| **Text** ||||||
|1|47.4 ns|24.2 ns|0.51x|35.2 ns|0.74x|
|10|509 ns|195 ns|0.38x|197 ns|0.39x|
|100|4.94 μs|1.74 μs|0.35x|1.66 μs|0.34x|
|1000|52.6 μs|17.0 μs|0.32x|15.0 μs|0.28x|
|10000|646 μs|206 μs|0.32x|155 μs|0.24x|
|100000|12.2 ms|3.34 ms|0.27x|2.60 ms|0.21x|
|1000000|159 ms|55.3 ms|0.35x|16.1 ms|0.10x|
| **Char** ||||||
|1|46.9 ns|21.1 ns|0.45x|22.3 ns|0.48x|
|10|229 ns|152 ns|0.66x|79.9 ns|0.35x|
|100|2.00 μs|1.23 μs|0.61x|618 ns|0.31x|
|1000|21.9 μs|10.3 μs|0.47x|6.28 μs|0.29x|
|10000|285 μs|153 μs|0.54x|68.5 μs|0.24x|
|100000|7.70 ms|4.08 ms|0.53x|992 μs|0.13x|
|1000000|110 ms|106 ms|0.96x|9.19 ms|0.08x|
| **Decimal** ||||||
|1|97.7 ns|872 ns|8.92x|80.2 ns|0.82x|
|10|864 ns|8.72 μs|10.09x|684 ns|0.79x|
|100|9.07 μs|93.5 μs|10.32x|7.25 μs|0.80x|
|1000|92.4 μs|1.06 ms|11.44x|67.5 μs|0.73x|
|10000|1.13 ms|13.4 ms|11.88x|667 μs|0.59x|
|100000|18.7 ms|141 ms|7.57x|7.57 ms|0.41x|
|1000000|229 ms|1.487 s|6.48x|67.8 ms|0.30x|
| **Hexadecimal** ||||||
|1|403 ns|749 ns|1.86x|43.9 ns|0.11x|
|10|3.94 μs|7.66 μs|1.94x|308 ns|0.08x|
|100|42.8 μs|89.0 μs|2.08x|2.88 μs|0.07x|
|1000|486 μs|986 μs|2.03x|27.7 μs|0.06x|
|10000|7.10 ms|12.6 ms|1.77x|283 μs|0.04x|
|100000|80.1 ms|133 ms|1.65x|3.53 ms|0.04x|
|1000000|867 ms|1.340 s|1.55x|28.9 ms|0.03x|
| **Double** ||||||
|1|7.56 μs|18.3 μs|2.42x|414 ns|0.05x|
|10|76.5 μs|188 μs|2.46x|4.23 μs|0.06x|
|100|754 μs|2.35 ms|3.11x|44.4 μs|0.06x|
|1000|7.94 ms|25.8 ms|3.25x|436 μs|0.05x|
|10000|79.1 ms|285 ms|3.60x|4.90 ms|0.06x|
|100000|796 ms|2.938 s|3.69x|45.1 ms|0.06x|
|1000000|8.003 s|32.411 s|4.05x|436 ms|0.05x|

If you are not convinced by synthetic data,
here are benchmarks for
[`blaze-markup` after migration to `Data.Text.Builder.Linear`](https://github.com/Bodigrim/blaze-markup):

```
bigTable
  992  μs ±  80 μs, 49% less than baseline
basic
  4.35 μs ± 376 ns, 47% less than baseline
wideTree
  1.26 ms ±  85 μs, 53% less than baseline
wideTreeEscaping
  217  μs ± 7.8 μs, 58% less than baseline
deepTree
  242  μs ±  23 μs, 48% less than baseline
manyAttributes
  811  μs ±  79 μs, 58% less than baseline
customAttribute
  1.68 ms ± 135 μs, 56% less than baseline
```

## Benchmarks for `ByteString`

Somewhat surprisingly, `text-builder-linear` now offers rendering to strict `ByteString`
as well. It is consistently faster than `bytestring` when a string gets over 32k
(which is `defaultChunkSize` for `bytestring` builder). For mid-sized strings
`bytestring` is slightly faster in certain disciplines, mostly by virtue of using
`cbits` via FFI, while this package remains 100% native Haskell.

Benchmarks below were measured with GHC 9.6 on aarch64 and include comparison
to [`bytestring-strict-builder`](https://hackage.haskell.org/package/bytestring-strict-builder):

|Group / size|`bytestring`|`…-strict-builder`|  |This package|  |
|------------|-----------:|-----------------:|-:|-----------:|-:|
| **Text** ||||||
|1|106 ns|33.5 ns|0.32x|35.2 ns|0.33x|
|10|322 ns|217 ns|0.68x|197 ns|0.61x|
|100|2.49 μs|1.89 μs|0.76x|1.66 μs|0.67x|
|1000|21.8 μs|18.5 μs|0.85x|15.0 μs|0.69x|
|10000|231 μs|212 μs|0.92x|155 μs|0.67x|
|100000|3.97 ms|3.54 ms|0.89x|2.60 ms|0.66x|
|1000000|81.2 ms|51.5 ms|0.63x|16.1 ms|0.20x|
| **Char** ||||||
|1|99.0 ns|19.4 ns|0.20x|22.3 ns|0.23x|
|10|270 ns|82.9 ns|0.31x|79.9 ns|0.30x|
|100|1.77 μs|723 ns|0.41x|618 ns|0.35x|
|1000|20.4 μs|8.37 μs|0.41x|6.28 μs|0.31x|
|10000|322 μs|129 μs|0.40x|68.5 μs|0.21x|
|100000|10.4 ms|2.50 ms|0.24x|992 μs|0.10x|
|1000000|143 ms|67.4 ms|0.47x|9.19 ms|0.06x|
| **Decimal** ||||||
|1|152 ns|174 ns|1.14x|80.2 ns|0.53x|
|10|685 ns|1.55 μs|2.26x|684 ns|1.00x|
|100|5.88 μs|17.2 μs|2.93x|7.25 μs|1.23x|
|1000|60.3 μs|196 μs|3.25x|67.5 μs|1.12x|
|10000|648 μs|4.25 ms|6.57x|667 μs|1.03x|
|100000|11.2 ms|62.8 ms|5.62x|7.57 ms|0.68x|
|1000000|150 ms|655 ms|4.37x|67.8 ms|0.45x|
| **Hexadecimal** ||||||
|1|94.7 ns|||43.9 ns|0.46x|
|10|255 ns|||308 ns|1.21x|
|100|1.72 μs|||2.88 μs|1.67x|
|1000|18.9 μs|||27.7 μs|1.46x|
|10000|250 μs|||283 μs|1.13x|
|100000|6.94 ms|||3.53 ms|0.51x|
|1000000|93.2 ms|||28.9 ms|0.31x|
| **Double** ||||||
|1|457 ns|||414 ns|0.91x|
|10|3.94 μs|||4.23 μs|1.07x|
|100|40.3 μs|||44.4 μs|1.10x|
|1000|398 μs|||436 μs|1.10x|
|10000|5.65 ms|||4.90 ms|0.87x|
|100000|63.3 ms|||45.1 ms|0.71x|
|1000000|673 ms|||436 ms|0.65x|
