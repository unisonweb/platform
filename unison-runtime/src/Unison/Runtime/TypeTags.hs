module Unison.Runtime.TypeTags
  ( Tag (..),
    RTag (..),
    CTag (..),
    PackedTag (..),
    packTags,
    unpackTags,
    maskTags,
    anyTag,
    floatTag,
    natTag,
    intTag,
    charTag,
    unitTag,
    leftTag,
    rightTag,
    noneTag,
    someTag,
    falseTag,
    trueTag,
    pairTag,
    failureTag,
    noBufTag,
    lineBufTag,
    blockBufTag,
    sizedBlockBufTag,
    readModeTag,
    writeModeTag,
    appendModeTag,
    readWriteModeTag,
    seekAbsoluteTag,
    seekRelativeTag,
    seekEndTag,
    exceptionTag,
    exceptionRaiseTag,
    stdInTag,
    stdOutTag,
    stdErrTag,
    pureEffectTag,
    seqViewElemTag,
    seqViewEmptyTag,
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

falseTag :: PackedTag
falseTag = mkEnumTag "falseTag" Ty.booleanRef 0

trueTag :: PackedTag
trueTag = mkEnumTag "trueTag" Ty.booleanRef 1

anyTag :: PackedTag
anyTag = mkEnumTag "anyTag" Ty.anyRef 0

failureTag :: PackedTag
failureTag = mkEnumTag "failureTag" Ty.failureRef 0

noneTag, someTag :: PackedTag
(noneTag, someTag)
  | [nt, st] <-
      mkTags "optional tags" Ty.optionalRef
        [Ty.noneId, Ty.someId] = (nt, st)
  | otherwise = error "internal error: optional tags"

leftTag, rightTag :: PackedTag
(leftTag, rightTag)
  | [lt, rt] <-
      mkTags "either tags" Ty.eitherRef
        [Ty.eitherLeftId, Ty.eitherRightId] = (lt, rt)
  | otherwise = error "internal error: either tags"

noBufTag, lineBufTag, blockBufTag, sizedBlockBufTag :: PackedTag
(noBufTag, lineBufTag, blockBufTag, sizedBlockBufTag)
  | [nt,lt,bt,st] <-
      mkTags "buffer mode tags" Ty.bufferModeRef
        [ Ty.bufferModeNoBufferingId,
          Ty.bufferModeLineBufferingId,
          Ty.bufferModeBlockBufferingId,
          Ty.bufferModeSizedBlockBufferingId ] = (nt, lt, bt, st)
  | otherwise = error "internal error: buffer mode tags"

readModeTag, writeModeTag, appendModeTag, readWriteModeTag :: PackedTag
(readModeTag, writeModeTag, appendModeTag, readWriteModeTag)
  | [rt,wt,at,rwt] <-
      mkTags "file mode tags" Ty.fileModeRef
        [ Ty.fileModeReadId,
          Ty.fileModeWriteId,
          Ty.fileModeAppendId,
          Ty.fileModeReadWriteId ] = (rt, wt, at, rwt)
  | otherwise = error "internal error: file mode tags"

seekAbsoluteTag, seekRelativeTag, seekEndTag :: PackedTag
(seekAbsoluteTag, seekRelativeTag, seekEndTag)
  | [at, rt, et] <-
      mkTags "seek mode tags" Ty.seekModeRef
        [ Ty.seekModeAbsoluteId,
          Ty.seekModeRelativeId,
          Ty.seekModeEndId ] = (at, rt, et)
  | otherwise = error "internal error: seek mode tags"

stdInTag, stdOutTag, stdErrTag :: PackedTag
(stdInTag, stdOutTag, stdErrTag)
  | [it, ot, et] <-
      mkTags "standard handle tags" Ty.stdHandleRef
        [ Ty.stdInId,
          Ty.stdOutId,
          Ty.stdErrId ] = (it, ot, et)
  | otherwise = error "internal error: standard handle tags"

exceptionTag :: Word64
exceptionRaiseTag :: PackedTag
(exceptionTag, exceptionRaiseTag)
  | Just n <- Map.lookup Ty.exceptionRef builtinTypeNumbering,
    et <- toEnum $ fromIntegral n,
    rt <- toEnum $ fromIntegral Ty.exceptionRaiseId =
      (n, packTags et rt)
  | otherwise = internalBug $ "internal error: Exception tag"

pairTag :: PackedTag
pairTag
  | Just n <- Map.lookup Ty.pairRef builtinTypeNumbering,
    pt <- toEnum (fromIntegral n) =
      packTags pt 0
  | otherwise = internalBug "internal error: pairTag"

seqViewEmptyTag, seqViewElemTag :: PackedTag
(seqViewEmptyTag, seqViewElemTag)
  | [emt, elt] <-
      mkTags "seq view tags" Ty.seqViewRef
        [ Ty.seqViewEmpty,
          Ty.seqViewElem ] = (emt, elt)
  | otherwise = error "internal error: seq view tags"

-- | A tag we use to represent the 'pure' effect case.
pureEffectTag :: PackedTag
pureEffectTag = PackedTag 0

-- | Construct a tag for a single-constructor builtin type
mkSimpleTag :: String -> Reference -> PackedTag
mkSimpleTag msg r = mkEnumTag msg r 0

mkEnumTag :: String -> Reference -> Int -> PackedTag
mkEnumTag msg r i
  | Just n <- Map.lookup r builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt (toEnum i)
  | otherwise = internalBug $ "internal error: " <> msg

mkTags :: String -> Reference -> [Word64] -> [PackedTag]
mkTags msg r cs
  | Just n <- Map.lookup r builtinTypeNumbering,
    tt <- toEnum $ fromIntegral n =
      packTags tt . toEnum . fromIntegral <$> cs
  | otherwise = error $ "internal error: " ++ msg
