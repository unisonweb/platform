{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Referent
  ( Referent,
    pattern Ref,
    pattern Con,
    Id,
    pattern RefId,
    pattern ConId,
    fold,
    toId,
    toReference,
    toReferenceId,
    toTermReference,
    toTermReferenceId,
    fromId,
    fromTermReference,
    fromTermReferenceId,
    fromText,

    -- * Lenses
    reference_,

    -- * ShortHash helpers
    isPrefixOf,
    toShortHash,
    toText,
    toString,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Unison.ConstructorReference (ConstructorReference, ConstructorReferenceId, GConstructorReference (..))
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Prelude hiding (fold)
import Unison.Reference (Reference, TermReference, TermReferenceId)
import Unison.Reference qualified as R
import Unison.Reference qualified as Reference
import Unison.ReferentPrime (Referent' (..), reference_, toReference')
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

-- | Specifies a term.
--
-- Either a term 'Reference', a data constructor, or an effect constructor.
--
-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.
type Referent = Referent' Reference

pattern Ref :: TermReference -> Referent
pattern Ref r = Ref' r

pattern Con :: ConstructorReference -> ConstructorType -> Referent
pattern Con r t = Con' r t

{-# COMPLETE Ref, Con #-}

-- | By definition, cannot be a builtin.
type Id = Referent' R.Id

pattern RefId :: R.Id -> Unison.Referent.Id
pattern RefId r = Ref' r

pattern ConId :: ConstructorReferenceId -> ConstructorType -> Unison.Referent.Id
pattern ConId r t = Con' r t

{-# COMPLETE RefId, ConId #-}

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

toId :: Referent -> Maybe Id
toId = \case
  Ref (Reference.ReferenceDerived r) ->
    Just (RefId r)
  Con (ConstructorReference (Reference.ReferenceDerived r) i) t ->
    Just (ConId (ConstructorReference r i) t)
  _ -> Nothing

fromId :: Id -> Referent
fromId = \case
  RefId r -> Ref (Reference.ReferenceDerived r)
  ConId (ConstructorReference r i) t ->
    Con (ConstructorReference (Reference.ReferenceDerived r) i) t

-- todo: move these to ShortHash module
toShortHash :: Referent -> ShortHash
toShortHash = \case
  Ref r -> R.toShortHash r
  Con r _ -> ConstructorReference.toShortHash r

toText :: Referent -> Text
toText = \case
  Ref r -> R.toText r
  Con (ConstructorReference r cid) ct -> R.toText r <> "#" <> ctorTypeText ct <> Text.pack (show cid)

ctorTypeText :: CT.ConstructorType -> Text
ctorTypeText CT.Effect = EffectCtor
ctorTypeText CT.Data = DataCtor

pattern EffectCtor :: (Eq a, IsString a) => a
pattern EffectCtor = "a"

pattern DataCtor :: (Eq a, IsString a) => a
pattern DataCtor = "d"

toString :: Referent -> String
toString = Text.unpack . toText

toReference :: Referent -> Reference
toReference = toReference'

toReferenceId :: Referent -> Maybe Reference.Id
toReferenceId = Reference.toId . toReference

toTermReference :: Referent -> Maybe TermReference
toTermReference = \case
  Con' _ _ -> Nothing
  Ref' reference -> Just reference

toTermReferenceId :: Referent -> Maybe TermReferenceId
toTermReferenceId r = toTermReference r >>= Reference.toId

-- | Inject a Term Reference into a Referent
fromTermReference :: TermReference -> Referent
fromTermReference r = Ref r

fromTermReferenceId :: TermReferenceId -> Referent
fromTermReferenceId = fromTermReference . Reference.fromId

isPrefixOf :: ShortHash -> Referent -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

-- #abc[.xy][#<T>cid]
--
-- >>> fromText "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg#d0"
-- Just (Con' (ConstructorReference #nirp5 0) Data)
--
-- >>> fromText "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg"
-- Just (Ref' #nirp5)
--
-- >>> fromText "##Text.uncons"
-- Just (Ref' ##Text.uncons)
fromText :: Text -> Maybe Referent
fromText t =
  either (const Nothing) Just $
    -- if the string has just one hash at the start, it's just a reference
    if refPart == "#" || refPart == "##"
      then Ref <$> R.fromText t
      else
        if Text.all Char.isDigit cidPart && (not . Text.null) cidPart
          then do
            r <- R.fromText (Text.dropEnd 1 refPart)
            ctorType <- ctorType
            let maybeCid = readMaybe (Text.unpack cidPart)
            case maybeCid of
              Nothing -> Left ("invalid constructor id: " <> Text.unpack cidPart)
              Just cid -> Right $ Con (ConstructorReference r cid) ctorType
          else Left ("invalid constructor id: " <> Text.unpack cidPart)
  where
    ctorType = case Text.take 1 cidPart' of
      EffectCtor -> Right CT.Effect
      DataCtor -> Right CT.Data
      _otherwise ->
        Left
          ( "invalid constructor type (expected '"
              <> EffectCtor
              <> "' or '"
              <> DataCtor
              <> "'): "
              <> Text.unpack cidPart'
          )
    refPart = Text.dropWhileEnd (/= '#') t
    cidPart' = Text.takeWhileEnd (/= '#') t
    cidPart = Text.drop 1 cidPart'

fold :: (r -> a) -> (r -> ConstructorId -> ConstructorType -> a) -> Referent' r -> a
fold fr fc = \case
  Ref' r -> fr r
  Con' (ConstructorReference r i) ct -> fc r i ct
