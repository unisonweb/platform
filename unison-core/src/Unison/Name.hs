{-# LANGUAGE OverloadedStrings   #-}

module Unison.Name
  ( Name(Name)
  , fromString
  , isPrefixOf
  , joinDot
  , makeAbsolute
  , parent
  , sortNames
  , sortNamed
  , sortNamed'
  , stripNamePrefix
  , stripPrefixes
  , suffixes
  , toString
  , toText
  , toVar
  , unqualified
  , unqualified'
  , unsafeFromText
  , unsafeFromString
  , fromVar
  )
where

import Unison.Prelude

import           Control.Lens                   ( unsnoc )
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Data.RFC5051                  as RFC5051
import           Data.List                      ( sortBy, tails )

newtype Name = Name { toText :: Text } deriving (Eq, Ord)

sortNames :: [Name] -> [Name]
sortNames = sortNamed id

sortNamed :: (a -> Name) -> [a] -> [a]
sortNamed by as = let
  as' = [ (a, Text.unpack (toText (by a))) | a <- as ]
  comp (_,s) (_,s2) = RFC5051.compareUnicode s s2
  in fst <$> sortBy comp as'

-- | Like sortNamed, but takes an additional backup comparison function if two
-- names are equal.
sortNamed' :: (a -> Name) -> (a -> a -> Ordering) -> [a] -> [a]
sortNamed' by by2 as = let
  as' = [ (a, Text.unpack (toText (by a))) | a <- as ]
  comp (a,s) (a2,s2) = RFC5051.compareUnicode s s2 <> by2 a a2
  in fst <$> sortBy comp as'

unsafeFromText :: Text -> Name
unsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else Name t

unsafeFromString :: String -> Name
unsafeFromString = unsafeFromText . Text.pack

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

fromVar :: Var v => v -> Name
fromVar = unsafeFromText . Var.name

toString :: Name -> String
toString = Text.unpack . toText

isPrefixOf :: Name -> Name -> Bool
a `isPrefixOf` b = toText a `Text.isPrefixOf` toText b

-- stripTextPrefix a.b. a.b.c = Just c
-- stripTextPrefix a.b  a.b.c = Just .c;  you probably don't want to do this
-- stripTextPrefix x.y. a.b.c = Nothing
-- stripTextPrefix "" a.b.c = undefined
_stripTextPrefix :: Text -> Name -> Maybe Name
_stripTextPrefix prefix name =
  Name <$> Text.stripPrefix prefix (toText name)

-- stripNamePrefix a.b  a.b.c = Just c
-- stripNamePrefix a.b. a.b.c = undefined, "a.b." isn't a valid name IMO
-- stripNamePrefix x.y  a.b.c = Nothing, x.y isn't a prefix of a.b.c
-- stripNamePrefix "" a.b.c = undefined, "" isn't a valid name IMO
-- stripNamePrefix . .Nat = Just Nat
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix prefix name =
  Name <$> Text.stripPrefix (toText prefix <> mid) (toText name)
  where
  mid = if toText prefix == "." then "" else "."

-- a.b.c.d -> d
stripPrefixes :: Name -> Name
stripPrefixes = unsafeFromText . last . Text.splitOn "." . toText

joinDot :: Name -> Name -> Name
joinDot prefix suffix =
  if toText prefix == "." then Name (toText prefix <> toText suffix)
  else Name (toText prefix <> "." <> toText suffix)

unqualified :: Name -> Name
unqualified = unsafeFromText . unqualified' . toText

-- parent . -> Nothing
-- parent + -> Nothing
-- parent foo -> Nothing
-- parent foo.bar -> foo
-- parent foo.bar.+ -> foo.bar
parent :: Name -> Maybe Name
parent (Name txt) = case unsnoc (Text.splitOn "." txt) of
  Nothing -> Nothing
  Just ([],_) -> Nothing
  Just (init,_) -> Just $ Name (Text.intercalate "." init)

suffixes :: Name -> [Name]
suffixes (Name n) =
  fmap up . tails . dropWhile (== "") $ Text.splitOn "." n
  where
  up ns = Name (Text.intercalate "." ns)

unqualified' :: Text -> Text
unqualified' = last . Text.splitOn "."

makeAbsolute :: Name -> Name
makeAbsolute n | toText n == "."                = Name ".."
               | Text.isPrefixOf "." (toText n) = n
               | otherwise                      = Name ("." <> toText n)

instance Show Name where
  show = toString

instance IsString Name where
  fromString = unsafeFromText . Text.pack

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]
