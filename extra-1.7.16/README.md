# Extra [![Hackage version](https://img.shields.io/hackage/v/extra.svg?label=Hackage)](https://hackage.haskell.org/package/extra) [![Stackage version](https://www.stackage.org/package/extra/badge/nightly?label=Stackage)](https://www.stackage.org/package/extra) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/extra/ci.yml?branch=master)](https://github.com/ndmitchell/extra/actions)

A library of extra functions for the standard Haskell libraries. Most functions are simple additions, filling out missing functionality. A few functions are available in later versions of GHC, but this package makes them available back to GHC 7.10. A few examples:

* `Control.Monad.Extra.concatMapM` provides a monadic version of `concatMap`, in the same way that `mapM` is a monadic version of `map`.
* `Data.Tuple.Extra.fst3` provides a function to get the first element of a triple.
* `Control.Exception.Extra.retry` provides a function that retries an `IO` action a number of times.
* `Data.Either.Extra.fromLeft` is a function available in GHC 8.0 and above. On GHC 8.0 and above this package reexports the version from `Data.Either` while on GHC 7.10 and below it defines an equivalent version.

The module `Extra` documents all functions provided by this library. Modules such as `Data.List.Extra` provide extra functions over `Data.List` and also reexport `Data.List`. Users are recommended to replace `Data.List` imports with `Data.List.Extra` if they need the extra functionality.

## Which functions?

When producing a library of extra functions I have been guided by a few principles. I encourage others with small useful utility functions contribute them here, perhaps as a temporary stop before proposing they join the standard libraries.

* I have been using most of these functions in my packages - they have proved useful enough to be worth copying/pasting into each project.
* The functions follow the spirit of the original Prelude/base libraries. I am happy to provide partial functions (e.g. `fromRight`), and functions which are specialisations of more generic functions (`whenJust`).
* Most of the functions have trivial implementations that are obvious from their name/signature. If a beginner couldn't write the function, it probably doesn't belong here.
* I have defined only a few new data types or type aliases. It's a package for defining new utilities on existing types, not new types or concepts.

## Base versions

A mapping between `base` versions and GHC compiler versions can be found [here](https://wiki.haskell.org/Base_package#Versions).

## Contributing

My general contribution preferences are [available here](https://github.com/ndmitchell/neil#contributions). In addition, this repo contains some generated code which is checked in, namely [src/Extra.hs](src/Extra.hs) and [test/TestGen.hs](test/TestGen.hs). You can generate those files by either running `runghc Generate.hs` or `ghci` (which uses the [`.ghci` file](.ghci)) and typing `:generate`. All PR's should contain regenerated versions of those files.
