# Revision history for nonempty-vector

## 0.2.3

* Support GHC 9.8.x

## 0.2.2.0

* Drop support for GHC<8.10, update CI, bump bounds for `primitive`.
* Added `partitionWith` from `vector`. [(#11)](https://github.com/emilypi/nonempty-vector/pull/11) - Thanks @AlistairB!

## 0.2.1.0

* Added `consV` and `snocV` primitives for consing a vector to create a nonempty one. [(#8)](https://github.com/emilypi/nonempty-vector/pull/8) - Thanks @AlistairB!
* Updated CI and cabal support
* Expose constructors in new `Data.Vector.NonEmpty.Internal` module
* Modules are now marked trustworthy
* `@since` annotations have been added.

## 0.2.0.2

* Removed spurious dependency on `semigroups`

## 0.2.0.1

* Missed a strictness tick in `postscanl'`

* INLINE pragma for slice

## 0.2.0.0

* Remove naughty `Generic`, and `Alternative` instances as they can construct empty `NonEmptyVector`s

* Handwritten `Read` and `Read1` instances with safe cons

* Added `uncons`, `unsnoc`, `replicate1`, `generate1`, `iterateN1`, `unsafeCreate`, `unsafeCreateT`, `unfoldr1`, `unfoldr1N`, `unfoldr1M`, `unfoldr1NM`,

* Added `unsafeFromList`, `unsafeFromVector`, and `fromNonEmptyN1`

* Add `ifilterM`

* Add doctests for all new functions + many familiar ones

## 0.1.0.0

* Remove `MonadFail` instance for the sake of backcompat with LTS < 13
* Drop Cabal version down to 2.0

## 0.0.1.1 -- 2019-10-20

* Export `toMVector` and `fromMVector`
* clean up docs

## 0.0.1.0 -- 2019-10-20

* First version. Released on an unsuspecting world.
