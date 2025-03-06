## Parser combinators 1.3.0

* Changed the `Control.Applicative.Permutations` module to only require
  `Applicative` and not `Monad`. This module is the least restrictive and works
  with parsers which are not `Monad`s.

* Added the `Control.Monad.Permutations` module. This module may be
  substantially more efficient for some parsers which are `Monad`s.

* Corrected how permutation parsers intercalate effects and components; parsing
  an effect requires that a component immediately follows or else a parse error
  will result.

## Parser combinators 1.2.1

* The tests in `parser-combinators-tests` now work with Megaparsec 8.

* Dropped support for GHC 8.2.

## Parser combinators 1.2.0

* Added `manyTill_` and `someTill_` combinators which work like the older
  `manyTill` and `someTill` except they also return the result of the `end`
  parser.

* Dropped support for GHC 8.0.

## Parser combinators 1.1.0

* Added support for ternary operators; see `TernR` in
  `Control.Monad.Combinators.Expr`.

## Parser combinators 1.0.3

* Dropped support for GHC 7.10.

* Added a test suite as a separate package called
  `parser-combinators-tests`.

## Parser combinators 1.0.2

* Defined `liftA2` for `Permutation` manually. The new definition should be
  more efficient.

* Made inner `Maybe` field in `Permutation` strict.

## Parser combinators 1.0.1

* Cosmetic changes in the source code.

## Parser combinators 1.0.0

* Added the `Control.Monad.Combinators.Expr` module.

* Dropped the compatibility operators `(<$$>)`, `(<$?>)`, `(<||>)`, and
  `(<|?>)` from `Control.Applicative.Permutations`.

* Dropped support for GHCs older than 7.10.

## Parser combinators 0.4.0

* Improved the documentation.

* Re-exported `Control.Applicative.empty` from
  `Control.Applicative.Combinators`.

* Added the `Control.Monad.Combinators` and
  `Control.Monad.Combinators.NonEmpty` modules which contain more efficient
  versions of the combinators from `Control.Applicative.Combinators` and
  `Control.Applicative.Combinators.NonEmpty` respectively.

## Parser combinators 0.3.0

* Added the `skipCount` combinator.

* Improved algorithmic efficiency of the `count'` combinator.

## Parser combinators 0.2.1

* Removed the byte order marking at the beginning of the
  `Control.Applicative.Permutations` module.

## Parser combinators 0.2.0

* Added `Control.Applicative.Combinators.NonEmpty` module that exports
  non-empty list versions of combinators that cannot return empty lists.

* Added `Control.Applicative.Permutations` module that provides generalized
  permutation parser combinators.

## Parser combinators 0.1.0

* Initial release.
