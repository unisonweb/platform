module Unison.Runtime.TypeTags
  ( Tag (..),
    RTag (..),
    CTag (..),
    PackedTag (..),
    packTags,
    unpackTags,
    maskTags,
    floatTag,
    natTag,
    intTag,
    charTag,
    unitTag,
    leftTag,
    rightTag,
  )
where

import Control.Exception (throw)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List hiding (and, or)
import Data.Map qualified as Map
import GHC.Stack (CallStack, callStack)
import U.Codebase.Reference (Reference)
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude
import Unison.Runtime.Builtin.Types (builtinTypeNumbering)
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as Pretty
import Prelude hiding (abs, and, or, seq)
import Prelude qualified

-- For internal errors
data CompileExn = CE CallStack (Pretty.Pretty Pretty.ColorText)
  deriving (Show)

instance Exception CompileExn

internalBug :: (HasCallStack) => String -> a
internalBug = throw . CE callStack . Pretty.lit . fromString

-- Types representing components that will go into the runtime tag of
-- a data type value. RTags correspond to references, while CTags
-- correspond to constructors.
newtype RTag = RTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

newtype CTag = CTag Word16
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

-- | A combined tag, which is a packed representation of an RTag and a CTag
newtype PackedTag = PackedTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

class Tag t where rawTag :: t -> Word64

instance Tag RTag where rawTag (RTag w) = w

instance Tag CTag where rawTag (CTag w) = fromIntegral w

packTags :: RTag -> CTag -> PackedTag
packTags (RTag rt) (CTag ct) = PackedTag (ri .|. ci)
  where
    ri = rt `shiftL` 16
    ci = fromIntegral ct

unpackTags :: PackedTag -> (RTag, CTag)
unpackTags (PackedTag w) = (RTag $ w `shiftR` 16, CTag . fromIntegral $ w .&. 0xFFFF)

-- Masks a packed tag to extract just the constructor tag portion
maskTags :: PackedTag -> Word64
maskTags (PackedTag w) = (w .&. 0xFFFF)

ensureRTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureRTag s n x
  | n > 0xFFFFFFFFFFFF =
      internalBug $ s ++ "@RTag: too large: " ++ show n
  | otherwise = x

ensureCTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureCTag s n x
  | n > 0xFFFF =
      internalBug $ s ++ "@CTag: too large: " ++ show n
  | otherwise = x

instance Enum RTag where
  toEnum i = ensureRTag "toEnum" i . RTag $ toEnum i
  fromEnum (RTag w) = fromEnum w

instance Enum CTag where
  toEnum i = ensureCTag "toEnum" i . CTag $ toEnum i
  fromEnum (CTag w) = fromEnum w

instance Num RTag where
  fromInteger i = ensureRTag "fromInteger" i . RTag $ fromInteger i
  (+) = internalBug "RTag: +"
  (*) = internalBug "RTag: *"
  abs = internalBug "RTag: abs"
  signum = internalBug "RTag: signum"
  negate = internalBug "RTag: negate"

instance Num CTag where
  fromInteger i = ensureCTag "fromInteger" i . CTag $ fromInteger i
  (+) = internalBug "CTag: +"
  (*) = internalBug "CTag: *"
  abs = internalBug "CTag: abs"
  signum = internalBug "CTag: signum"
  negate = internalBug "CTag: negate"

floatTag :: PackedTag
floatTag = mkSimpleTag "floatTag" Ty.floatRef

natTag :: PackedTag
natTag = mkSimpleTag "natTag" Ty.natRef

intTag :: PackedTag
intTag = mkSimpleTag "intTag" Ty.intRef

charTag :: PackedTag
charTag = mkSimpleTag "charTag" Ty.charRef

unitTag :: PackedTag
unitTag = mkSimpleTag "unitTag" Ty.unitRef

leftTag, rightTag :: PackedTag
(leftTag, rightTag)
  | Just n <- Map.lookup Ty.eitherRef builtinTypeNumbering,
    et <- toEnum (fromIntegral n),
    lt <- toEnum (fromIntegral Ty.eitherLeftId),
    rt <- toEnum (fromIntegral Ty.eitherRightId) =
      (packTags et lt, packTags et rt)
  | otherwise = error "internal error: either tags"

-- | Construct a tag for a single-constructor builtin type
mkSimpleTag :: String -> Reference -> PackedTag
mkSimpleTag msg r
  | Just n <- Map.lookup r builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = internalBug $ "internal error: " <> msg
