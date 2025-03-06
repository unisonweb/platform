module Unison.Merge.Mergeblob4
  ( Mergeblob4 (..),
    makeMergeblob4,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Merge.Mergeblob3 (Mergeblob3 (..))
import Unison.Names (Names (..))
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser (ParsingEnv (..))
import Unison.Syntax.Parser qualified as Parser
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation

data Mergeblob4 = Mergeblob4
  { dependencies :: DefnsF Set TermReference TypeReference,
    file :: UnisonFile Symbol Ann
  }

makeMergeblob4 :: Mergeblob3 -> Either (Parser.Err Symbol) Mergeblob4
makeMergeblob4 blob = do
  let stageOneNames =
        Names (Relation.fromMap blob.stageOne.terms) (Relation.fromMap blob.stageOne.types) <> blob.libdeps

      parsingEnv =
        ParsingEnv
          { -- We don't expect to have to generate any new GUIDs, since the uniqueTypeGuid lookup function below should
            -- cover all name in the merged file we're about to parse and typecheck. So, this might be more correct as a
            -- call to `error`.
            uniqueNames = Parser.UniqueName \_ _ -> Nothing,
            -- FIXME we don't need this, right? After the switch to always render unique type guids? Guess it doesn't hurt.
            uniqueTypeGuid = \name -> Identity (Map.lookup name blob.uniqueTypeGuids),
            names = stageOneNames,
            maybeNamespace = Nothing,
            localNamespacePrefixedTypesAndConstructors = mempty
          }
  file <- runIdentity (Parsers.parseFile "<merge>" (Pretty.toPlain 80 blob.unparsedFile) parsingEnv)
  Right
    Mergeblob4
      { dependencies = UnisonFile.dependencies file,
        file
      }
