module Unison.Merge.Mergeblob5
  ( Mergeblob5 (..),
    makeMergeblob5,
  )
where

import Data.Map.Strict qualified as Map
import Unison.FileParsers qualified as FileParsers
import Unison.Merge.Mergeblob4 (Mergeblob4 (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.UnisonFile (TypecheckedUnisonFile)

data Mergeblob5 = Mergeblob5
  { file :: TypecheckedUnisonFile Symbol Ann
  }

makeMergeblob5 :: Mergeblob4 -> TypeLookup Symbol Ann -> Either (Seq (Result.Note Symbol Ann)) Mergeblob5
makeMergeblob5 blob typeLookup =
  let typecheckingEnv =
        Typechecker.Env
          { ambientAbilities = [],
            termsByShortname = Map.empty,
            typeLookup,
            topLevelComponents = Map.empty
          }
   in case runIdentity (Result.runResultT (FileParsers.synthesizeFile typecheckingEnv blob.file)) of
        (Nothing, notes) -> Left notes
        (Just file, _) -> Right Mergeblob5 {file}
