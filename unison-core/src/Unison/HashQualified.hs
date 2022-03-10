{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified where

import qualified Data.Text as Text
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.ConstructorReference as ConstructorReference
import Unison.Name (Convert, Name, Parse)
import qualified Unison.Name as Name
import Unison.Prelude hiding (fromString)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Unison.Var (Var)
import qualified Unison.Var as Var
import Prelude hiding (take)

data HashQualified n
  = NameOnly n
  | HashOnly ShortHash
  | HashQualified n ShortHash
  deriving (Eq, Foldable, Traversable, Functor, Show, Generic)

stripNamespace :: Text -> HashQualified Name -> HashQualified Name
stripNamespace "" hq = hq
stripNamespace namespace hq = case hq of
  NameOnly name -> NameOnly $ strip name
  HashQualified name sh -> HashQualified (strip name) sh
  ho -> ho
  where
    strip name =
      fromMaybe name $ Name.stripNamePrefix (Name.unsafeFromText namespace) name

toName :: HashQualified n -> Maybe n
toName = \case
  NameOnly name -> Just name
  HashQualified name _ -> Just name
  HashOnly _ -> Nothing

-- Sort the list of names by length of segments: smaller number of
-- segments is listed first. NameOnly < Hash qualified < Hash only
--
-- Examples:
--   [foo.bar.baz, bar.baz] -> [bar.baz, foo.bar.baz]
--   [#a29dj2k91, foo.bar.baz] -> [foo.bar.baz, #a29dj2k91]
--   [foo.bar#abc, foo.bar] -> [foo.bar, foo.bar#abc]
--   [.foo.bar, foo.bar] -> [foo.bar, .foo.bar]
sortByLength :: [HashQualified Name] -> [HashQualified Name]
sortByLength hs = sortOn f hs
  where
    f (NameOnly n) = (length (Name.reverseSegments n), 0, Left n)
    f (HashQualified n _h) = (length (Name.reverseSegments n), 1, Left n)
    f (HashOnly h) = (maxBound, 0, Right h)

hasName, hasHash :: HashQualified Name -> Bool
hasName = isJust . toName
hasHash = isJust . toHash

toHash :: HashQualified n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh
  HashOnly sh -> Just sh

-- partial: assumes either a name or hash is provided (or both)
fromNameHash :: Maybe Name -> Maybe ShortHash -> HashQualified Name
fromNameHash n h = case n of
  Just name -> case h of
    Just hash -> HashQualified name hash
    Nothing -> NameOnly name
  Nothing -> case h of
    Just hash -> HashOnly hash
    Nothing -> error "bad HQ construction"

take :: Int -> HashQualified n -> HashQualified n
take i = \case
  n@(NameOnly _) -> n
  HashOnly s -> HashOnly (SH.take i s)
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.take i s)

toString :: Show n => HashQualified n -> String
toString = Text.unpack . toText

toStringWith :: (n -> String) -> HashQualified n -> String
toStringWith f = Text.unpack . toTextWith (Text.pack . f)

fromString :: String -> Maybe (HashQualified Name)
fromString = fromText . Text.pack

unsafeFromString :: String -> HashQualified Name
unsafeFromString s = fromMaybe msg . fromString $ s
  where
    msg = error $ "HashQualified.unsafeFromString " <> show s

-- Parses possibly-hash-qualified into structured type.
-- Doesn't validate against base58 or the codebase.
fromText :: Text -> Maybe (HashQualified Name)
fromText t = case Text.breakOn "#" t of -- breakOn leaves the '#' on the RHS
  (name, "") -> Just $ NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  ("", hash) -> HashOnly <$> SH.fromText hash
  (name, hash) -> HashQualified (Name.unsafeFromText name) <$> SH.fromText hash

-- Won't crash as long as SH.unsafeFromText doesn't crash on any input that
-- starts with '#', which is true as of the time of this writing, but not great.
unsafeFromText :: Text -> HashQualified Name
unsafeFromText txt = fromMaybe msg . fromText $ txt
  where
    msg = error $ "HashQualified.unsafeFromText " <> show txt

toText :: Show n => HashQualified n -> Text
toText =
  toTextWith (Text.pack . show)

toTextWith :: (n -> Text) -> HashQualified n -> Text
toTextWith f = \case
  NameOnly name -> f name
  HashQualified name hash -> f name <> SH.toText hash
  HashOnly hash -> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromReferent :: Referent -> HashQualified Name
fromReferent = HashOnly . Referent.toShortHash

fromReference :: Reference -> HashQualified Name
fromReference = HashOnly . Reference.toShortHash

fromPattern :: ConstructorReference -> HashQualified Name
fromPattern r = HashOnly $ ConstructorReference.toShortHash r

fromName :: n -> HashQualified n
fromName = NameOnly

unsafeFromVar :: Var v => v -> HashQualified Name
unsafeFromVar = unsafeFromText . Var.name

fromVar :: Var v => v -> Maybe (HashQualified Name)
fromVar = fromText . Var.name

toVar :: Var v => HashQualified Name -> v
toVar = Var.named . toTextWith Name.toText

-- todo: find this logic elsewhere and replace with call to this
matchesNamedReferent :: Name -> Referent -> HashQualified Name -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Referent.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: Name -> Reference -> HashQualified Name -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Reference.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Reference.toShortHash r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified Name -> Referent -> HashQualified Name
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r
  HashOnly _ -> fromReferent r

-- Ordered alphabetically, based on the name. Hashes come last.
instance (Eq n, Name.Alphabetical n) => Ord (HashQualified n) where
  compare a b =
    case (toName a, toName b) of
      (Just n, Just n2) -> Name.compareAlphabetical n n2
      (Nothing, Just _) -> GT
      (Just _, Nothing) -> LT
      (Nothing, Nothing) -> EQ
      <> case (toHash a, toHash b) of
        (Nothing, Nothing) -> EQ
        (Nothing, Just _) -> LT -- prefer NameOnly to HashQualified
        (Just _, Nothing) -> GT
        (Just sh, Just sh2) -> compare sh sh2

instance Convert n n2 => Convert (HashQualified n) (HashQualified n2) where
  convert = fmap Name.convert

instance Convert n (HashQualified n) where
  convert = NameOnly

instance Parse Text (HashQualified Name) where
  parse = fromText

-- instance Show n => Show (HashQualified n) where
--  show = Text.unpack . toText
