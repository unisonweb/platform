module Unison.HashQualifiedPrime where

import Data.Text qualified as Text
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Prelude hiding (take)

-- | Like Unison.HashQualified, but doesn't support a HashOnly variant
data HashQualified n = NameOnly n | HashQualified n ShortHash
  deriving stock (Eq, Functor, Generic, Foldable, Ord, Show, Traversable)

type HQSegment = HashQualified NameSegment

toHQ :: HashQualified n -> HQ.HashQualified n
toHQ = \case
  NameOnly n -> HQ.NameOnly n
  HashQualified n sh -> HQ.HashQualified n sh

type HashOrHQ n = Either ShortHash (HashQualified n)

-- | If the 'HQ.HashQualified' is just a 'ShortHash', return it on the 'Left', otherwise return a `HashQualified` on the
--  `Right`.
fromHQ :: HQ.HashQualified n -> HashOrHQ n
fromHQ = \case
  HQ.NameOnly n -> Right $ NameOnly n
  HQ.HashQualified n sh -> Right $ HashQualified n sh
  HQ.HashOnly sh -> Left sh

toName :: HashQualified n -> n
toName = \case
  NameOnly name -> name
  HashQualified name _ -> name

nameLength :: (Name -> Text) -> HashQualified Name -> Int
nameLength nameToText = Text.length . toTextWith nameToText

take :: Int -> HashQualified n -> HashQualified n
take i = \case
  n@(NameOnly _) -> n
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.shortenTo i s)

toNameOnly :: HashQualified n -> HashQualified n
toNameOnly = fromName . toName

toHash :: HashQualified n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh

toStringWith :: (n -> String) -> HashQualified n -> String
toStringWith f = Text.unpack . toTextWith (Text.pack . f)

toTextWith :: (n -> Text) -> HashQualified n -> Text
toTextWith f = \case
  NameOnly name -> f name
  HashQualified name hash -> f name <> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromName :: n -> HashQualified n
fromName = NameOnly

fromNameHash :: n -> Maybe ShortHash -> HashQualified n
fromNameHash name = \case
  Nothing -> NameOnly name
  Just hash -> HashQualified name hash

matchesNamedReferent :: (Eq n) => n -> Referent -> HashQualified n -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: (Eq n) => n -> Reference -> HashQualified n -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Reference.toShortHash r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified Name -> Referent -> HashQualified Name
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r

-- | Sort the list of names by length of segments: smaller number of segments is listed first. NameOnly < HashQualified
sortByLength :: [HashQualified Name] -> [HashQualified Name]
sortByLength =
  sortOn \case
    NameOnly name -> (length (Name.reverseSegments name), Nothing, Name.isAbsolute name)
    HashQualified name hash -> (length (Name.reverseSegments name), Just hash, Name.isAbsolute name)

instance (Name.Alphabetical n) => Name.Alphabetical (HashQualified n) where
  compareAlphabetical (NameOnly n) (NameOnly n2) = Name.compareAlphabetical n n2
  -- NameOnly comes first
  compareAlphabetical NameOnly {} HashQualified {} = LT
  compareAlphabetical HashQualified {} NameOnly {} = GT
  compareAlphabetical (HashQualified n sh) (HashQualified n2 sh2) = Name.compareAlphabetical n n2 <> compare sh sh2
