# co-log-core

![Co-logo](https://user-images.githubusercontent.com/8126674/80955687-92f21a80-8df7-11ea-90d3-422dafdc8391.png)

[![GitHub CI](https://github.com/co-log/co-log-core/workflows/CI/badge.svg)](https://github.com/co-log/co-log-core/actions)
[![Hackage][hk-img-core]][hk-core]
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/co-log/co-log/blob/main/LICENSE)

`co-log-core` is a lightweight package that provides core types and functions to 
work with the @LogAction@ data type which is both simple and powerful.

## How to use

`co-log-core` is compatible with the following GHC
versions - [supported versions](https://matrix.hackage.haskell.org/#/package/co-log-core)

In order to start using `co-log-core` in your project, you
will need to set it up with these steps:

1. Add the dependency on `co-log-core` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section according to the below section:

   ```haskell
   build-depends: base ^>= LATEST_SUPPORTED_BASE
                , co-log-core ^>= LATEST_VERSION
   ```

2. To use this package, refer to the below example.

   ```haskell
   module Main (main) where

   import Prelude hiding (log)

   import Colog.Core (LogAction, logStringStdout, (<&))


   app :: LogAction IO String -> IO ()
   app log = do
       log <& "Starting app..."
       log <& "Finishing app..."

   main :: IO ()
   main = app logStringStdout
   ```
 
 
[hk-img-core]: https://img.shields.io/hackage/v/co-log-core.svg?logo=haskell
[hk-core]: https://hackage.haskell.org/package/co-log-core
