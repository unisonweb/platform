module Main (main) where

import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Test.Doc qualified as Doc
import Unison.Test.Unison qualified as Unison

main :: IO ()
main = withCP65001 . run $ tests [Unison.test, Doc.test]
