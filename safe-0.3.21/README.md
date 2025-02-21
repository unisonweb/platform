# Safe [![Hackage version](https://img.shields.io/hackage/v/safe.svg?label=Hackage)](https://hackage.haskell.org/package/safe) [![Stackage version](https://www.stackage.org/package/safe/badge/nightly?label=Stackage)](https://www.stackage.org/package/safe) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/safe/ci.yml?branch=master)](https://github.com/ndmitchell/safe/actions)

A library wrapping `Prelude`/`Data.List` functions that can throw exceptions, such as `head` and `!!`. Each unsafe function has up to four variants, e.g. with `tail`:

* <tt>tail :: [a] -> [a]</tt>, raises an error on `tail []`.
* <tt>tailMay :: [a] -> <i>Maybe</i> [a]</tt>, turns errors into `Nothing`.
* <tt>tailDef :: <i>[a]</i> -> [a] -> [a]</tt>, takes a default to return on errors.
* <tt>tailNote :: <i>String</i> -> [a] -> [a]</tt>, takes an extra argument which supplements the error message.
* <tt>tailSafe :: [a] -> [a]</tt>, returns some sensible default if possible, `[]` in the case of `tail`.

This package is divided into three modules:

* `Safe` contains safe variants of `Prelude` and `Data.List` functions.
* `Safe.Foldable` contains safe variants of `Foldable` functions.
* `Safe.Exact` creates crashing versions of functions like `zip` (errors if the lists are not equal) and `take` (errors if there are not enough elements), then wraps them to provide safe variants.
