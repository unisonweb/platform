module U.Codebase.Sqlite.NamedRef where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Unison.Prelude
import Unison.Sqlite (FromField (..), FromRow (..), SQLData (..), ToField (..), ToRow (..), field)

type ReversedSegments = NonEmpty Text

data ConstructorType
  = DataConstructor
  | EffectConstructor

instance ToField (ConstructorType) where
  toField ct = case ct of
    DataConstructor -> (SQLInteger 0)
    EffectConstructor -> (SQLInteger 1)

instance FromField (ConstructorType) where
  fromField f =
    fromField @Int f >>= \case
      0 -> pure DataConstructor
      1 -> pure EffectConstructor
      _ -> fail "Invalid ConstructorType"

data NamedRef ref = NamedRef {reversedSegments :: ReversedSegments, ref :: ref}
  deriving stock (Show, Functor, Foldable, Traversable)

instance (ToRow ref) => ToRow (NamedRef ref) where
  toRow (NamedRef {reversedSegments = segments, ref}) =
    [toField reversedName] <> toRow ref
    where
      reversedName = Text.intercalate "." . toList $ segments

instance (FromRow ref) => FromRow (NamedRef ref) where
  fromRow = do
    reversedSegments <- NonEmpty.fromList . Text.splitOn "." <$> field
    ref <- fromRow
    pure (NamedRef {reversedSegments, ref})

toRowWithNamespace :: (ToRow ref) => NamedRef ref -> [SQLData]
toRowWithNamespace nr = toRow nr <> [SQLText namespace]
  where
    namespace = Text.intercalate "." . reverse . NEL.tail . reversedSegments $ nr
