# Non-empty Vectors

![Build Status](https://github.com/emilypi/nonempty-vector/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/nonempty-vector.svg)](https://hackage.haskell.org/package/nonempty-vector)

This package presents thin wrappers around mutable and immutable [Data.Vector](https://hackage.haskell.org/package/vector) types that enforce non-emptiness. The entire Vector API is supported for both sets of boxed vectors, with future plans to support unboxed, primitive, storable, and generic vectors.

There are no external dependencies that are not already in `base`.
