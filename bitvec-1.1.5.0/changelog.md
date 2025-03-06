# 1.1.5.0

* Make `zipBits` unconditionally strict in its second bit
  vector argument (thanks to @treeowl).

* Add `simd` flag (enabled by default) to use a C SIMD
  implementation for `zipBits`, `invertBits`, `countBits`,
  `bitIndex`, `nthBitIndex`, `selectBits`, `excludeBits`,
  `reverseBits` (thanks to @konsumlamm).

* Decomission `libgmp` flag.

# 1.1.4.0

* Include `Data.Bit.Gmp` only if `libgmp` flag is set.
* Tweak inlining pragmas to inline less aggressively.

# 1.1.3.0

* Fix malformed `signum` for `F2Poly`.

# 1.1.2.0

* Fix `setBit`, `clearBit`, `complementBit` to preserve vector's length.
* Fix various issues on big-endian architectures.
* Fix Cabal 3.7+ incompatibility.

# 1.1.1.0

* Export `BitVec` and `BitMVec` constructors.

# 1.1.0.0

* Fix a grave bug in `bitIndex`.
* Remove `integer-gmp` flag.
* Make `libgmp` flag disabled by default.
  Users are strongly encouraged to enable it whenever possible.
* Add `mapBits` and `mapInPlace` functions.
* Add `cloneToByteString` and `cloneFromByteString` functions.

# 1.0.3.0

* Add `Bits (Vector Bit)` instance.
* Add `castFromWords8`, `castToWords8`, `cloneToWords8`
  to facilitate interoperation with `ByteString`.

# 1.0.2.0

* Fix out-of-bounds writes in mutable interface.
* Improve thread-safety of mutable interface.
* Add extended GCD for `F2Poly`.
* Change `Show` instance of `F2Poly`.

# 1.0.1.2

* Fix more bugs in `F2Poly` multiplication.

# 1.0.1.1

* Fix bugs in `F2Poly` multiplication.
* Performance improvements.

# 1.0.1.0

* Implement arithmetic of binary polynomials.
* Add `invertBits` and `reverseBits` functions.
* Add `Num`, `Real`, `Integral`, `Fractional` and `NFData` instances.
* Performance improvements.

# 1.0.0.1

* Performance improvements.

# 1.0.0.0

* Redesign API from the scratch.
* Add a thread-safe implementation.
* Add `nthBitIndex` function.

# 0.2.0.1

* Fix `Read` instance.

# 0.2.0.0

* Remove hand-written `Num`, `Real`, `Integral`, `Bits` instances.
* Derive `Bits` and `FiniteBits` instances.
* Expose `Bit` constructor directly and remove `fromBool` function.
* Rename `toBool` to `unBit`.

# 0.1.1.0

* Fix bugs in `MVector` and `Vector` instances of `Bit`.
* Speed up `MVector` and `Vector` instances of `Bit`.
