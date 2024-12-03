{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.Runtime.ANF qualified as ANF
import Unison.Test.Runtime.ANF.Serialization qualified as ANF.Serialization
import Unison.Test.Runtime.Crypto.Rsa qualified as Rsa
import Unison.Test.Runtime.MCode qualified as MCode
import Unison.Test.Runtime.MCode.Serialization qualified as MCode.Serialization
import Unison.Test.UnisonSources qualified as UnisonSources

test :: Test ()
test =
  tests
    [ ANF.test,
      ANF.Serialization.test,
      MCode.test,
      MCode.Serialization.test,
      Rsa.test,
      UnisonSources.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
