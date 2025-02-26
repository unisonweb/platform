
module Unison.Runtime.Machine.Primops where

import Control.Exception
import Control.Concurrent.STM as STM
import Data.Atomics qualified as Atomic
import Data.Bits
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Word
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text)
import Unison.Reference (Reference)
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF (PackedTag (..), maskTags)
import Unison.Runtime.Array as PA
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign.Function
import Unison.Runtime.Machine.Types
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as Ty
import Unison.Runtime.Util
import Unison.Type qualified as Rf
import Unison.Util.Bytes (Bytes)
import Unison.Util.Bytes qualified as By
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Text as UText

prim1wrap ::
  ForeignConvention x =>
  (Stack -> x -> IO ()) ->
  Stack -> Int -> IO Stack
prim1wrap f stk i = do
  x <- readAtIndex stk i
  stk <- bump stk
  stk <$ f stk x
{-# inline prim1wrap #-}

prim1wrapl ::
  ForeignConvention x =>
  (Stack -> x -> IO ()) ->
  Stack -> MLit -> IO Stack
prim1wrapl f stk l = do
  x <- readLit l
  stk <- bump stk
  stk <$ f stk x
{-# inline prim1wrapl #-}

prim1 :: CCache -> Stack -> Prim1 -> Int -> IO Stack
prim1 !_env !stk DECI !i = prim1wrap deci stk i
prim1 !_env !stk DECN !i = prim1wrap decn stk i
prim1 !_env !stk INCI !i = prim1wrap inci stk i
prim1 !_env !stk INCN !i = prim1wrap incn stk i
prim1 !_env !stk TRNC !i = prim1wrap trnc stk i
prim1 !_env !stk NEGI !i = prim1wrap negi stk i
prim1 !_env !stk SGNI !i = prim1wrap sgni stk i
prim1 !_env !stk ABSF !i = prim1wrap absf stk i
prim1 !_env !stk CEIL !i = prim1wrap ceil stk i
prim1 !_env !stk FLOR !i = prim1wrap flor stk i
prim1 !_env !stk TRNF !i = prim1wrap trnf stk i
prim1 !_env !stk RNDF !i = prim1wrap rndf stk i
prim1 !_env !stk EXPF !i = prim1wrap expf stk i
prim1 !_env !stk LOGF !i = prim1wrap logf stk i
prim1 !_env !stk SQRT !i = prim1wrap sqtf stk i
prim1 !_env !stk COSF !i = prim1wrap cosf stk i
prim1 !_env !stk SINF !i = prim1wrap sinf stk i
prim1 !_env !stk TANF !i = prim1wrap tanf stk i
prim1 !_env !stk COSH !i = prim1wrap cshf stk i
prim1 !_env !stk SINH !i = prim1wrap snhf stk i
prim1 !_env !stk TANH !i = prim1wrap tnhf stk i
prim1 !_env !stk ACOS !i = prim1wrap acsf stk i
prim1 !_env !stk ASIN !i = prim1wrap asnf stk i
prim1 !_env !stk ATAN !i = prim1wrap atnf stk i
prim1 !_env !stk ASNH !i = prim1wrap asnh stk i
prim1 !_env !stk ACSH !i = prim1wrap acsh stk i
prim1 !_env !stk ATNH !i = prim1wrap atnh stk i
prim1 !_env !stk ITOF !i = prim1wrap itof stk i
prim1 !_env !stk NTOF !i = prim1wrap ntof stk i
prim1 !_env !stk LZRO !i = prim1wrap lzro stk i
prim1 !_env !stk TZRO !i = prim1wrap tzro stk i
prim1 !_env !stk POPC !i = prim1wrap popc stk i
prim1 !_env !stk COMN !i = prim1wrap comn stk i
prim1 !_env !stk COMI !i = prim1wrap comi stk i
prim1 !_env !stk NOTB !i = prim1wrap notb stk i
prim1 !_env !stk SIZT i = prim1wrap sizt stk i
prim1 !_env !stk SIZS i = prim1wrap sizs stk i
prim1 !_env !stk ITOT i = prim1wrap itot stk i
prim1 !_env !stk NTOT i = prim1wrap ntot stk i
prim1 !_env !stk FTOT i = prim1wrap ftot stk i
prim1 !_env !stk USNC i = prim1wrap usnc stk i
prim1 !_env !stk UCNS i = prim1wrap ucns stk i
prim1 !_env !stk TTOI i = prim1wrap ttoi stk i
prim1 !_env !stk TTON i = prim1wrap tton stk i
prim1 !_env !stk TTOF i = prim1wrap ttof stk i
prim1 !_env !stk VWLS i = prim1wrap vwls stk i
prim1 !_env !stk VWRS i = prim1wrap vwrs stk i
prim1 !_env !stk PAKT i = prim1wrap pakt stk i
prim1 !_env !stk UPKT i = prim1wrap upkt stk i
prim1 !_env !stk PAKB i = prim1wrap pakb stk i
prim1 !_env !stk UPKB i = prim1wrap upkb stk i
prim1 !_env !stk SIZB i = prim1wrap sizb stk i
prim1 !_env !stk FLTB i = prim1wrap fltb stk i
prim1 !_env !stk REFR i = prim1wrap refr stk i
prim1 !_env !stk REFN i = prim1wrap refn stk i
prim1 !env !stk RRFC i = prim1wrap (rrfc env) stk i
prim1 !_env !stk TIKR i = prim1wrap tikr stk i

prim1 !env !stk MISS i = prim1wrap (miss env) stk i
prim1 !env !stk SDBL i = prim1wrap (sdbl env) stk i
prim1 !env !stk LKUP i = prim1wrap (lkup env) stk i

-- handled elsewhere
prim1 !_env !stk CACH _ = pure stk
prim1 !_env !stk CVLD _ = pure stk
prim1 !_env !stk TLTT _ = pure stk
prim1 !_env !stk LOAD _ = pure stk
prim1 !_env !stk VALU _ = pure stk
prim1 !_env !stk DBTX _ = pure stk
{-# inline prim1 #-}

priml :: CCache -> Stack -> Prim1 -> MLit -> IO Stack
priml !_env !stk DECI !l = prim1wrapl deci stk l
priml !_env !stk DECN !l = prim1wrapl decn stk l
priml !_env !stk INCI !l = prim1wrapl inci stk l
priml !_env !stk INCN !l = prim1wrapl incn stk l
priml !_env !stk TRNC !l = prim1wrapl trnc stk l
priml !_env !stk NEGI !l = prim1wrapl negi stk l
priml !_env !stk SGNI !l = prim1wrapl sgni stk l
priml !_env !stk ABSF !l = prim1wrapl absf stk l
priml !_env !stk CEIL !l = prim1wrapl ceil stk l
priml !_env !stk FLOR !l = prim1wrapl flor stk l
priml !_env !stk TRNF !l = prim1wrapl trnf stk l
priml !_env !stk RNDF !l = prim1wrapl rndf stk l
priml !_env !stk EXPF !l = prim1wrapl expf stk l
priml !_env !stk LOGF !l = prim1wrapl logf stk l
priml !_env !stk SQRT !l = prim1wrapl sqtf stk l
priml !_env !stk COSF !l = prim1wrapl cosf stk l
priml !_env !stk SINF !l = prim1wrapl sinf stk l
priml !_env !stk TANF !l = prim1wrapl tanf stk l
priml !_env !stk COSH !l = prim1wrapl cshf stk l
priml !_env !stk SINH !l = prim1wrapl snhf stk l
priml !_env !stk TANH !l = prim1wrapl tnhf stk l
priml !_env !stk ACOS !l = prim1wrapl acsf stk l
priml !_env !stk ASIN !l = prim1wrapl asnf stk l
priml !_env !stk ATAN !l = prim1wrapl atnf stk l
priml !_env !stk ASNH !l = prim1wrapl asnh stk l
priml !_env !stk ACSH !l = prim1wrapl acsh stk l
priml !_env !stk ATNH !l = prim1wrapl atnh stk l
priml !_env !stk ITOF !l = prim1wrapl itof stk l
priml !_env !stk NTOF !l = prim1wrapl ntof stk l
priml !_env !stk LZRO !l = prim1wrapl lzro stk l
priml !_env !stk TZRO !l = prim1wrapl tzro stk l
priml !_env !stk POPC !l = prim1wrapl popc stk l
priml !_env !stk COMN !l = prim1wrapl comn stk l
priml !_env !stk COMI !l = prim1wrapl comi stk l
priml !_env !stk NOTB !l = prim1wrapl notb stk l
priml !_env !stk SIZT l = prim1wrapl sizt stk l
priml !_env !stk SIZS l = prim1wrapl sizs stk l
priml !_env !stk ITOT l = prim1wrapl itot stk l
priml !_env !stk NTOT l = prim1wrapl ntot stk l
priml !_env !stk FTOT l = prim1wrapl ftot stk l
priml !_env !stk USNC l = prim1wrapl usnc stk l
priml !_env !stk UCNS l = prim1wrapl ucns stk l
priml !_env !stk TTOI l = prim1wrapl ttoi stk l
priml !_env !stk TTON l = prim1wrapl tton stk l
priml !_env !stk TTOF l = prim1wrapl ttof stk l
priml !_env !stk VWLS l = prim1wrapl vwls stk l
priml !_env !stk VWRS l = prim1wrapl vwrs stk l
priml !_env !stk PAKT l = prim1wrapl pakt stk l
priml !_env !stk UPKT l = prim1wrapl upkt stk l
priml !_env !stk PAKB l = prim1wrapl pakb stk l
priml !_env !stk UPKB l = prim1wrapl upkb stk l
priml !_env !stk SIZB l = prim1wrapl sizb stk l
priml !_env !stk FLTB l = prim1wrapl fltb stk l
priml !_env !stk REFR l = prim1wrapl refr stk l
priml !_env !stk REFN l = prim1wrapl refn stk l

priml !env !stk MISS l = prim1wrapl (miss env) stk l
priml !env !stk SDBL l = prim1wrapl (sdbl env) stk l
priml !env !stk LKUP l = prim1wrapl (lkup env) stk l

priml _ _ op _ = throwIO $ Panic msg Nothing
  where
    msg = "priml: operation `" ++ show op ++ "` applied to invalid literal"
{-# inline priml #-}

-- Wrap an implementation to act on an index on two indices
prim2wrap2 ::
  ForeignConvention x =>
  ForeignConvention y =>
  (Stack -> x -> y -> IO ()) ->
  Stack -> Int -> Int -> IO Stack
prim2wrap2 f stk i j = do
  x <- readAtIndex stk i
  y <- readAtIndex stk j
  stk <- bump stk
  stk <$ f stk x y
{-# inline prim2wrap2 #-}

-- Wrap an implementation to act on an index on the left
prim2wrapl ::
  ForeignConvention x =>
  (Stack -> x -> y -> IO ()) ->
  Stack -> Int -> y -> IO Stack
prim2wrapl f stk i y = do
  x <- readAtIndex stk i
  stk <- bump stk
  stk <$ f stk x y
{-# inline prim2wrapl #-}

-- Wrap an implementation to act on an index on the right
prim2wrapr ::
  ForeignConvention y =>
  (Stack -> x -> y -> IO ()) ->
  Stack -> x -> Int -> IO Stack
prim2wrapr f stk x j = do
  y <- readAtIndex stk j
  stk <- bump stk
  stk <$ f stk x y
{-# inline prim2wrapr #-}

prim2wrapll ::
  ForeignConvention x =>
  ForeignConvention y =>
  (Stack -> x -> y -> IO ()) ->
  Stack -> MLit -> MLit -> IO Stack
prim2wrapll f stk l r = do
  x <- readLit l
  y <- readLit r
  stk <- bump stk
  stk <$ f stk x y
{-# inline prim2wrapll #-}

-- Primops applied to two stack indices
primxx :: Stack -> Prim2 -> Int -> Int -> IO Stack
primxx stk ADDI i j = prim2wrap2 addi stk i j
primxx stk SUBI i j = prim2wrap2 subi stk i j
primxx stk MULI i j = prim2wrap2 muli stk i j
primxx stk DIVI i j = prim2wrap2 divi stk i j
primxx stk MODI i j = prim2wrap2 modi stk i j
primxx stk EQLI i j = prim2wrap2 eqli stk i j
primxx stk NEQI i j = prim2wrap2 neqi stk i j
primxx stk LEQI i j = prim2wrap2 leqi stk i j
primxx stk LESI i j = prim2wrap2 lesi stk i j
primxx stk ANDI i j = prim2wrap2 andi stk i j
primxx stk IORI i j = prim2wrap2 iori stk i j
primxx stk XORI i j = prim2wrap2 xori stk i j
primxx stk SHLI i j = prim2wrap2 shli stk i j
primxx stk SHRI i j = prim2wrap2 shri stk i j
primxx stk POWI i j = prim2wrap2 powi stk i j

primxx stk ADDN i j = prim2wrap2 addn stk i j
primxx stk SUBN i j = prim2wrap2 subn stk i j
primxx stk MULN i j = prim2wrap2 muln stk i j
primxx stk DIVN i j = prim2wrap2 divn stk i j
primxx stk MODN i j = prim2wrap2 modn stk i j
primxx stk SHLN i j = prim2wrap2 shln stk i j
primxx stk SHRN i j = prim2wrap2 shrn stk i j
primxx stk POWN i j = prim2wrap2 pown stk i j
primxx stk EQLN i j = prim2wrap2 eqln stk i j
primxx stk NEQN i j = prim2wrap2 neqn stk i j
primxx stk LEQN i j = prim2wrap2 leqn stk i j
primxx stk LESN i j = prim2wrap2 lesn stk i j
primxx stk ANDN i j = prim2wrap2 andn stk i j
primxx stk IORN i j = prim2wrap2 iorn stk i j
primxx stk XORN i j = prim2wrap2 xorn stk i j
primxx stk DRPN i j = prim2wrap2 drpn stk i j

primxx stk EQLF i j = prim2wrap2 eqlf stk i j
primxx stk NEQF i j = prim2wrap2 neqf stk i j
primxx stk LEQF i j = prim2wrap2 leqf stk i j
primxx stk LESF i j = prim2wrap2 lesf stk i j
primxx stk ADDF i j = prim2wrap2 addf stk i j
primxx stk SUBF i j = prim2wrap2 subf stk i j
primxx stk MULF i j = prim2wrap2 mulf stk i j
primxx stk DIVF i j = prim2wrap2 divf stk i j
primxx stk ATN2 i j = prim2wrap2 atn2 stk i j
primxx stk POWF i j = prim2wrap2 powf stk i j
primxx stk LOGB i j = prim2wrap2 logb stk i j
primxx stk MAXF i j = prim2wrap2 maxf stk i j
primxx stk MINF i j = prim2wrap2 minf stk i j
primxx stk DRPT i j = prim2wrap2 drpt stk i j
primxx stk TAKT i j = prim2wrap2 takt stk i j
primxx stk CATT i j = prim2wrap2 catt stk i j
primxx stk IXOT i j = prim2wrap2 ixot stk i j
primxx stk EQLT i j = prim2wrap2 eqlt stk i j
primxx stk LEQT i j = prim2wrap2 leqt stk i j
primxx stk LEST i j = prim2wrap2 lest stk i j
primxx stk EQLU i j = prim2wrap2 eqlu stk i j
primxx stk CMPU i j = prim2wrap2 cmpu stk i j
primxx stk LEQU i j = prim2wrap2 lequ stk i j
primxx stk LESU i j = prim2wrap2 lesu stk i j
primxx stk DRPS i j = prim2wrap2 drps stk i j
primxx stk TAKS i j = prim2wrap2 taks stk i j
primxx stk CONS i j = prim2wrap2 cons stk i j
primxx stk SNOC i j = prim2wrap2 snoc stk i j
primxx stk IDXS i j = prim2wrap2 idxs stk i j
primxx stk SPLL i j = prim2wrap2 spll stk i j
primxx stk SPLR i j = prim2wrap2 splr stk i j
primxx stk CATS i j = prim2wrap2 cats stk i j
primxx stk TAKB i j = prim2wrap2 takb stk i j
primxx stk DRPB i j = prim2wrap2 drpb stk i j
primxx stk IDXB i j = prim2wrap2 idxb stk i j
primxx stk CATB i j = prim2wrap2 catb stk i j
primxx stk IXOB i j = prim2wrap2 ixob stk i j
primxx stk REFW i j = prim2wrap2 refw stk i j
primxx stk CAST i j = prim2wrap2 cast stk i j
primxx stk ANDB i j = prim2wrap2 andb stk i j
primxx stk IORB i j = prim2wrap2 iorb stk i j

-- handled elsewhere
primxx stk THRO _ _ = pure stk
primxx stk SDBX _ _ = pure stk
primxx stk SDBV _ _ = pure stk
primxx stk TRCE _ _ = pure stk
{-# inline primxx #-}

primll :: Stack -> Prim2 -> MLit -> MLit -> IO Stack
primll stk ADDI i j = prim2wrapll addi stk i j
primll stk SUBI i j = prim2wrapll subi stk i j
primll stk MULI i j = prim2wrapll muli stk i j
primll stk DIVI i j = prim2wrapll divi stk i j
primll stk MODI i j = prim2wrapll modi stk i j
primll stk EQLI i j = prim2wrapll eqli stk i j
primll stk NEQI i j = prim2wrapll neqi stk i j
primll stk LEQI i j = prim2wrapll leqi stk i j
primll stk LESI i j = prim2wrapll lesi stk i j
primll stk ANDI i j = prim2wrapll andi stk i j
primll stk IORI i j = prim2wrapll iori stk i j
primll stk XORI i j = prim2wrapll xori stk i j
primll stk SHLI i j = prim2wrapll shli stk i j
primll stk SHRI i j = prim2wrapll shri stk i j
primll stk POWI i j = prim2wrapll powi stk i j

primll stk ADDN i j = prim2wrapll addn stk i j
primll stk SUBN i j = prim2wrapll subn stk i j
primll stk MULN i j = prim2wrapll muln stk i j
primll stk DIVN i j = prim2wrapll divn stk i j
primll stk MODN i j = prim2wrapll modn stk i j
primll stk SHLN i j = prim2wrapll shln stk i j
primll stk SHRN i j = prim2wrapll shrn stk i j
primll stk POWN i j = prim2wrapll pown stk i j
primll stk EQLN i j = prim2wrapll eqln stk i j
primll stk NEQN i j = prim2wrapll neqn stk i j
primll stk LEQN i j = prim2wrapll leqn stk i j
primll stk LESN i j = prim2wrapll lesn stk i j
primll stk ANDN i j = prim2wrapll andn stk i j
primll stk IORN i j = prim2wrapll iorn stk i j
primll stk XORN i j = prim2wrapll xorn stk i j
primll stk DRPN i j = prim2wrapll drpn stk i j

primll stk EQLF i j = prim2wrapll eqlf stk i j
primll stk NEQF i j = prim2wrapll neqf stk i j
primll stk LEQF i j = prim2wrapll leqf stk i j
primll stk LESF i j = prim2wrapll lesf stk i j
primll stk ADDF i j = prim2wrapll addf stk i j
primll stk SUBF i j = prim2wrapll subf stk i j
primll stk MULF i j = prim2wrapll mulf stk i j
primll stk DIVF i j = prim2wrapll divf stk i j
primll stk ATN2 i j = prim2wrapll atn2 stk i j
primll stk POWF i j = prim2wrapll powf stk i j
primll stk LOGB i j = prim2wrapll logb stk i j
primll stk MAXF i j = prim2wrapll maxf stk i j
primll stk MINF i j = prim2wrapll minf stk i j
primll stk DRPT i j = prim2wrapll drpt stk i j
primll stk TAKT i j = prim2wrapll takt stk i j
primll stk CATT i j = prim2wrapll catt stk i j
primll stk IXOT i j = prim2wrapll ixot stk i j
primll stk EQLT i j = prim2wrapll eqlt stk i j
primll stk LEQT i j = prim2wrapll leqt stk i j
primll stk LEST i j = prim2wrapll lest stk i j
primll stk EQLU i j = prim2wrapll eqlu stk i j
primll stk CMPU i j = prim2wrapll cmpu stk i j
primll stk LEQU i j = prim2wrapll lequ stk i j
primll stk LESU i j = prim2wrapll lesu stk i j
primll stk DRPS i j = prim2wrapll drps stk i j
primll stk TAKS i j = prim2wrapll taks stk i j
primll stk CONS i j = prim2wrapll cons stk i j
primll stk SNOC i j = prim2wrapll snoc stk i j
primll stk IDXS i j = prim2wrapll idxs stk i j
primll stk SPLL i j = prim2wrapll spll stk i j
primll stk SPLR i j = prim2wrapll splr stk i j
primll stk CATS i j = prim2wrapll cats stk i j
primll stk TAKB i j = prim2wrapll takb stk i j
primll stk DRPB i j = prim2wrapll drpb stk i j
primll stk IDXB i j = prim2wrapll idxb stk i j
primll stk CATB i j = prim2wrapll catb stk i j
primll stk REFW i j = prim2wrapll refw stk i j
primll stk CAST i j = prim2wrapll cast stk i j
primll stk ANDB i j = prim2wrapll andb stk i j
primll stk IORB i j = prim2wrapll iorb stk i j

primll _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primll: operation `" ++ show op ++ "` applied to invalid literal"


-- Primops applied to two stack indices
primix :: Stack -> Prim2 -> UnboxedTypeTag -> Int -> Int -> IO Stack
primix stk ADDI _ i j = prim2wrapr addi stk i j
primix stk SUBI _ i j = prim2wrapr subi stk i j
primix stk MULI _ i j = prim2wrapr muli stk i j
primix stk DIVI _ i j = prim2wrapr divi stk i j
primix stk MODI _ i j = prim2wrapr modi stk i j
primix stk EQLI _ i j = prim2wrapr eqli stk i j
primix stk NEQI _ i j = prim2wrapr neqi stk i j
primix stk LEQI _ i j = prim2wrapr leqi stk i j
primix stk LESI _ i j = prim2wrapr lesi stk i j
primix stk ANDI _ i j = prim2wrapr andi stk i j
primix stk IORI _ i j = prim2wrapr iori stk i j
primix stk XORI _ i j = prim2wrapr xori stk i j
primix stk SHLI _ i j = prim2wrapr shli stk i j
primix stk SHRI _ i j = prim2wrapr shri stk i j
primix stk POWI _ i j = prim2wrapr powi stk i j

primix stk ADDN _ i j = prim2wrapr addn stk (fromIntegral i) j
primix stk SUBN _ i j = prim2wrapr subn stk (fromIntegral i) j
primix stk MULN _ i j = prim2wrapr muln stk (fromIntegral i) j
primix stk DIVN _ i j = prim2wrapr divn stk (fromIntegral i) j
primix stk MODN _ i j = prim2wrapr modn stk (fromIntegral i) j
primix stk SHLN _ i j = prim2wrapr shln stk (fromIntegral i) j
primix stk SHRN _ i j = prim2wrapr shrn stk (fromIntegral i) j
primix stk POWN _ i j = prim2wrapr pown stk (fromIntegral i) j
primix stk EQLN _ i j = prim2wrapr eqln stk (fromIntegral i) j
primix stk NEQN _ i j = prim2wrapr neqn stk (fromIntegral i) j
primix stk LEQN _ i j = prim2wrapr leqn stk (fromIntegral i) j
primix stk LESN _ i j = prim2wrapr lesn stk (fromIntegral i) j
primix stk ANDN _ i j = prim2wrapr andn stk (fromIntegral i) j
primix stk IORN _ i j = prim2wrapr iorn stk (fromIntegral i) j
primix stk XORN _ i j = prim2wrapr xorn stk (fromIntegral i) j
primix stk DRPN _ i j = prim2wrapr drpn stk (fromIntegral i) j

primix stk DRPT _ i j = prim2wrapr drpt stk i j
primix stk TAKT _ i j = prim2wrapr takt stk i j
primix stk EQLU t i j = prim2wrapr eqlu stk (UnboxedVal i t) j
primix stk CMPU t i j = prim2wrapr cmpu stk (UnboxedVal i t) j
primix stk LEQU t i j = prim2wrapr lequ stk (UnboxedVal i t) j
primix stk LESU t i j = prim2wrapr lesu stk (UnboxedVal i t) j
primix stk DRPS _ i j = prim2wrapr drps stk i j
primix stk TAKS _ i j = prim2wrapr taks stk i j
primix stk CONS t i j = prim2wrapr cons stk (UnboxedVal i t) j
primix stk IDXS _ i j = prim2wrapr idxs stk i j
primix stk SPLL _ i j = prim2wrapr spll stk i j
primix stk SPLR _ i j = prim2wrapr splr stk i j
primix stk TAKB _ i j = prim2wrapr takb stk i j
primix stk DRPB _ i j = prim2wrapr drpb stk i j
primix stk IDXB _ i j = prim2wrapr idxb stk i j
primix stk CAST _ i j = prim2wrapr cast stk i j

-- invalid literals
primix _ op _ _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primix: operation `" ++ show op ++ "` applied to invalid literal"
{-# inline primix #-}

primxi :: Stack -> Prim2 -> Int -> UnboxedTypeTag -> Int -> IO Stack
primxi stk ADDI i _ n = prim2wrapl addi stk i n
primxi stk SUBI i _ n = prim2wrapl subi stk i n
primxi stk MULI i _ n = prim2wrapl muli stk i n
primxi stk DIVI i _ n = prim2wrapl divi stk i n
primxi stk MODI i _ n = prim2wrapl modi stk i n
primxi stk EQLI i _ n = prim2wrapl eqli stk i n
primxi stk NEQI i _ n = prim2wrapl neqi stk i n
primxi stk LEQI i _ n = prim2wrapl leqi stk i n
primxi stk LESI i _ n = prim2wrapl lesi stk i n
primxi stk ANDI i _ n = prim2wrapl andi stk i n
primxi stk IORI i _ n = prim2wrapl iori stk i n
primxi stk XORI i _ n = prim2wrapl xori stk i n
primxi stk SHLI i _ n = prim2wrapl shli stk i n
primxi stk SHRI i _ n = prim2wrapl shri stk i n
primxi stk POWI i _ n = prim2wrapl powi stk i (fromIntegral n)

primxi stk ADDN i _ n = prim2wrapl addn stk i (fromIntegral n)
primxi stk SUBN i _ n = prim2wrapl subn stk i (fromIntegral n)
primxi stk MULN i _ n = prim2wrapl muln stk i (fromIntegral n)
primxi stk DIVN i _ n = prim2wrapl divn stk i (fromIntegral n)
primxi stk MODN i _ n = prim2wrapl modn stk i (fromIntegral n)
primxi stk SHLN i _ n = prim2wrapl shln stk i (fromIntegral n)
primxi stk SHRN i _ n = prim2wrapl shrn stk i (fromIntegral n)
primxi stk POWN i _ n = prim2wrapl pown stk i (fromIntegral n)
primxi stk EQLN i _ n = prim2wrapl eqln stk i (fromIntegral n)
primxi stk NEQN i _ n = prim2wrapl neqn stk i (fromIntegral n)
primxi stk LEQN i _ n = prim2wrapl leqn stk i (fromIntegral n)
primxi stk LESN i _ n = prim2wrapl lesn stk i (fromIntegral n)
primxi stk ANDN i _ n = prim2wrapl andn stk i (fromIntegral n)
primxi stk IORN i _ n = prim2wrapl iorn stk i (fromIntegral n)
primxi stk XORN i _ n = prim2wrapl xorn stk i (fromIntegral n)
primxi stk DRPN i _ n = prim2wrapl drpn stk i (fromIntegral n)

primxi stk EQLU i t n = prim2wrapl eqlu stk i (UnboxedVal n t)
primxi stk CMPU i t n = prim2wrapl cmpu stk i (UnboxedVal n t)
primxi stk LEQU i t n = prim2wrapl lequ stk i (UnboxedVal n t)
primxi stk LESU i t n = prim2wrapl lesu stk i (UnboxedVal n t)
primxi stk SNOC i t n = prim2wrapl snoc stk i (UnboxedVal n t)
primxi stk REFW i t n = prim2wrapl refw stk i (UnboxedVal n t)
primxi stk CAST i _ n = prim2wrapl cast stk i n
-- literal values aren't sandboxed
primxi stk SDBX _ _ _ = do
  stk <- bump stk
  stk <$ pokeBool stk True

-- bad literals
primxi _ op _ _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primxi: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primxi #-}

primdx :: Stack -> Prim2 -> Double -> Int -> IO Stack
primdx stk EQLF d j = prim2wrapr eqlf stk d j
primdx stk NEQF d j = prim2wrapr neqf stk d j
primdx stk LEQF d j = prim2wrapr leqf stk d j
primdx stk LESF d j = prim2wrapr lesf stk d j
primdx stk ADDF d j = prim2wrapr addf stk d j
primdx stk SUBF d j = prim2wrapr subf stk d j
primdx stk MULF d j = prim2wrapr mulf stk d j
primdx stk DIVF d j = prim2wrapr divf stk d j
primdx stk ATN2 d j = prim2wrapr atn2 stk d j
primdx stk POWF d j = prim2wrapr powf stk d j
primdx stk LOGB d j = prim2wrapr logb stk d j
primdx stk MAXF d j = prim2wrapr maxf stk d j
primdx stk MINF d j = prim2wrapr minf stk d j
primdx stk EQLU d j = prim2wrapr eqlu stk (DoubleVal d) j
primdx stk CMPU d j = prim2wrapr cmpu stk (DoubleVal d) j
primdx stk LEQU d j = prim2wrapr lequ stk (DoubleVal d) j
primdx stk LESU d j = prim2wrapr lesu stk (DoubleVal d) j
primdx stk CONS d j = prim2wrapr cons stk (DoubleVal d) j
primdx stk CAST d j = prim2wrapr cast stk (doubleToInt d) j

-- bad literals
primdx _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primdx: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primdx #-}

primxd :: Stack -> Prim2 -> Int -> Double -> IO Stack
primxd stk EQLF i d = prim2wrapl eqlf stk i d
primxd stk NEQF i d = prim2wrapl neqf stk i d
primxd stk LEQF i d = prim2wrapl leqf stk i d
primxd stk LESF i d = prim2wrapl lesf stk i d
primxd stk ADDF i d = prim2wrapl addf stk i d
primxd stk SUBF i d = prim2wrapl subf stk i d
primxd stk MULF i d = prim2wrapl mulf stk i d
primxd stk DIVF i d = prim2wrapl divf stk i d
primxd stk ATN2 i d = prim2wrapl atn2 stk i d
primxd stk POWF i d = prim2wrapl powf stk i d
primxd stk LOGB i d = prim2wrapl logb stk i d
primxd stk MAXF i d = prim2wrapl maxf stk i d
primxd stk MINF i d = prim2wrapl minf stk i d
primxd stk EQLU i d = prim2wrapl eqlu stk i (DoubleVal d)
primxd stk CMPU i d = prim2wrapl cmpu stk i (DoubleVal d)
primxd stk LEQU i d = prim2wrapl lequ stk i (DoubleVal d)
primxd stk LESU i d = prim2wrapl lesu stk i (DoubleVal d)
primxd stk SNOC i d = prim2wrapl snoc stk i (DoubleVal d)
primxd stk REFW i d = prim2wrapl refw stk i (DoubleVal d)
-- literal values aren't sandboxed
primxd stk SDBX _ _ = do
  stk <- bump stk
  stk <$ pokeBool stk True

-- bad literals
primxd _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primxd: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primxd #-}

primtx :: Stack -> Prim2 -> Text -> Int -> IO Stack
primtx stk CATT t j = prim2wrapr catt stk t j
primtx stk IXOT t j = prim2wrapr ixot stk t j
primtx stk EQLT t j = prim2wrapr eqlt stk t j
primtx stk LEQT t j = prim2wrapr leqt stk t j
primtx stk LEST t j = prim2wrapr lest stk t j
primtx stk EQLU t j = prim2wrapr eqlu stk (TextVal t) j
primtx stk CMPU t j = prim2wrapr cmpu stk (TextVal t) j
primtx stk LEQU t j = prim2wrapr lequ stk (TextVal t) j
primtx stk LESU t j = prim2wrapr lesu stk (TextVal t) j
primtx stk CONS t j = prim2wrapr cons stk (TextVal t) j

-- bad literals
primtx _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primtx: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primtx #-}

primxt :: Stack -> Prim2 -> Int -> Text -> IO Stack
primxt stk DRPT i t = prim2wrapl drpt stk i t
primxt stk TAKT i t = prim2wrapl takt stk i t
primxt stk CATT i t = prim2wrapl catt stk i t
primxt stk IXOT i t = prim2wrapl ixot stk i t
primxt stk EQLT i t = prim2wrapl eqlt stk i t
primxt stk LEQT i t = prim2wrapl leqt stk i t
primxt stk LEST i t = prim2wrapl lest stk i t
primxt stk EQLU i t = prim2wrapl eqlu stk i (TextVal t)
primxt stk CMPU i t = prim2wrapl cmpu stk i (TextVal t)
primxt stk LEQU i t = prim2wrapl lequ stk i (TextVal t)
primxt stk LESU i t = prim2wrapl lesu stk i (TextVal t)
primxt stk SNOC i t = prim2wrapl snoc stk i (TextVal t)
primxt stk REFW i t = prim2wrapl refw stk i (TextVal t)
-- literal values aren't sandboxed
primxt stk SDBX _ _ = do
  stk <- bump stk
  stk <$ pokeBool stk True

-- bad literals
primxt _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primxt: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primxt #-}

primxm :: Stack -> Prim2 -> Int -> Referent -> IO Stack
primxm stk EQLU i m = prim2wrapl eqlu stk i (termLinkVal m)
primxm stk CMPU i m = prim2wrapl cmpu stk i (termLinkVal m)
primxm stk LEQU i m = prim2wrapl lequ stk i (termLinkVal m)
primxm stk LESU i m = prim2wrapl lesu stk i (termLinkVal m)
primxm stk SNOC i m = prim2wrapl snoc stk i (termLinkVal m)
-- literal values aren't sandboxed
primxm stk SDBX _ _ = do
  stk <- bump stk
  stk <$ pokeBool stk True

-- bad literals
primxm _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primxm: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primxm #-}

primmx :: Stack -> Prim2 -> Referent -> Int -> IO Stack
primmx stk EQLU m j = prim2wrapr eqlu stk (termLinkVal m) j
primmx stk CMPU m j = prim2wrapr cmpu stk (termLinkVal m) j
primmx stk LEQU m j = prim2wrapr lequ stk (termLinkVal m) j
primmx stk LESU m j = prim2wrapr lesu stk (termLinkVal m) j
primmx stk CONS m j = prim2wrapr cons stk (termLinkVal m) j

-- bad literals
primmx _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primmx: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primmx #-}

termLinkVal :: Referent -> Val
termLinkVal = BoxedVal . Foreign . Wrap Rf.termLinkRef

primxy :: Stack -> Prim2 -> Int -> Reference -> IO Stack
primxy stk EQLU i y = prim2wrapl eqlu stk i (typeLinkVal y)
primxy stk CMPU i y = prim2wrapl cmpu stk i (typeLinkVal y)
primxy stk LEQU i y = prim2wrapl lequ stk i (typeLinkVal y)
primxy stk LESU i y = prim2wrapl lesu stk i (typeLinkVal y)
primxy stk SNOC i y = prim2wrapl snoc stk i (typeLinkVal y)
-- literal values aren't sandboxed
primxy stk SDBX _ _ = do
  stk <- bump stk
  stk <$ pokeBool stk True

-- bad literals
primxy _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primxy: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primxy #-}

primyx :: Stack -> Prim2 -> Reference -> Int -> IO Stack
primyx stk EQLU y j = prim2wrapr eqlu stk (typeLinkVal y) j
primyx stk CMPU y j = prim2wrapr cmpu stk (typeLinkVal y) j
primyx stk LEQU y j = prim2wrapr lequ stk (typeLinkVal y) j
primyx stk LESU y j = prim2wrapr lesu stk (typeLinkVal y) j
primyx stk CONS y j = prim2wrapr cons stk (typeLinkVal y) j

-- bad literals
primyx _ op _ _ = throwIO $ Panic msg Nothing
  where
    msg = "primyx: operation `" ++ show op ++ "` applied to bad literal"
{-# inline primyx #-}

typeLinkVal :: Reference -> Val
typeLinkVal = BoxedVal . Foreign . Wrap Rf.typeLinkRef

-- Evaluation and writeback portion of primops.
--
-- These are defined so that they can be common, and just referenced
-- in the various permutations of their application to literals or
-- variables.
--
-- The bodies always accept a stack that has already been bumped to
-- the appropriate location for their return result.
deci :: Stack -> Int -> IO ()
deci stk m = pokeI stk (m - 1)

decn :: Stack -> Word64 -> IO ()
decn stk m = pokeN stk (m - 1)

inci :: Stack -> Int -> IO ()
inci stk m = pokeI stk (m + 1)

incn :: Stack -> Word64 -> IO ()
incn stk m = pokeN stk (m + 1)

trnc :: Stack -> Int -> IO ()
trnc stk v = unsafePokeIasN stk (max 0 v)

negi :: Stack -> Int -> IO ()
negi stk m = pokeI stk (-m)

sgni :: Stack -> Int -> IO ()
sgni stk m = pokeI stk (signum m)

absf :: Stack -> Double -> IO ()
absf stk d = pokeD stk (abs d)

ceil :: Stack -> Double -> IO ()
ceil stk d = pokeI stk (ceiling d)

flor :: Stack -> Double -> IO ()
flor stk d = pokeI stk (floor d)

trnf :: Stack -> Double -> IO ()
trnf stk d = pokeI stk (truncate d)

rndf :: Stack -> Double -> IO ()
rndf stk d = pokeI stk (round d)

expf :: Stack -> Double -> IO ()
expf stk d = pokeD stk (exp d)

logf :: Stack -> Double -> IO ()
logf stk d = pokeD stk (log d)

sqtf :: Stack -> Double -> IO ()
sqtf stk d = pokeD stk (sqrt d)

cosf :: Stack -> Double -> IO ()
cosf stk d = pokeD stk (cos d)

sinf :: Stack -> Double -> IO ()
sinf stk d = pokeD stk (sin d)

tanf :: Stack -> Double -> IO ()
tanf stk d = pokeD stk (tan d)

cshf :: Stack -> Double -> IO ()
cshf stk d = pokeD stk (cosh d)

snhf :: Stack -> Double -> IO ()
snhf stk d = pokeD stk (sinh d)

tnhf :: Stack -> Double -> IO ()
tnhf stk d = pokeD stk (tanh d)

acsf :: Stack -> Double -> IO ()
acsf stk d = pokeD stk (acos d)

asnf :: Stack -> Double -> IO ()
asnf stk d = pokeD stk (asin d)

atnf :: Stack -> Double -> IO ()
atnf stk d = pokeD stk (atan d)

asnh :: Stack -> Double -> IO ()
asnh stk d = pokeD stk (asinh d)

acsh :: Stack -> Double -> IO ()
acsh stk d = pokeD stk (acosh d)

atnh :: Stack -> Double -> IO ()
atnh stk d = pokeD stk (atanh d)

itof :: Stack -> Int -> IO ()
itof stk n = pokeD stk (fromIntegral n)

ntof :: Stack -> Word64 -> IO ()
ntof stk n = pokeD stk (fromIntegral n)

lzro :: Stack -> Word64 -> IO ()
lzro stk n = unsafePokeIasN stk (countLeadingZeros n)

tzro :: Stack -> Word64 -> IO ()
tzro stk n = unsafePokeIasN stk (countTrailingZeros n)

popc :: Stack -> Word64 -> IO ()
popc stk n = unsafePokeIasN stk (popCount n)

comn :: Stack -> Word64 -> IO ()
comn stk n = pokeN stk (complement n)

comi :: Stack -> Int -> IO ()
comi stk n = pokeI stk (complement n)

notb :: Stack -> Bool -> IO ()
notb stk b = pokeBool stk (not b)

sizt :: Stack -> Text -> IO ()
sizt stk t = unsafePokeIasN stk $ UText.size t

sizs :: Stack -> USeq -> IO ()
sizs stk s = unsafePokeIasN stk $ Sq.length s

itot :: Stack -> Int -> IO ()
itot stk n = pokeBi stk . UText.pack $ show n

ntot :: Stack -> Word64 -> IO ()
ntot stk n = pokeBi stk . UText.pack $ show n

ftot :: Stack -> Double -> IO ()
ftot stk f = pokeBi stk . UText.pack $ show f

usnc :: Stack -> Text -> IO ()
usnc stk t = writeBack stk (UText.unsnoc t)

ucns :: Stack -> Text -> IO ()
ucns stk t = writeBack stk (UText.uncons t)

ttoi :: Stack -> Text -> IO ()
ttoi stk t = writeBack stk (readi $ UText.unpack t)
  where
    readi :: String -> Maybe Int
    readi ('+' : s) = readMaybe s
    readi s = readMaybe s

tton :: Stack -> Text -> IO ()
tton stk t = writeBack stk (readn $ UText.unpack t)
  where
    readn :: String -> Maybe Word64
    readn = readMaybe

ttof :: Stack -> Text -> IO ()
ttof stk t = writeBack stk (readd $ UText.unpack t)
  where
    readd :: String -> Maybe Double
    readd = readMaybe

vwls :: Stack -> USeq -> IO ()
vwls stk s = writeBack stk result
  where
    result = case s of
      Sq.Empty -> SeqViewEmpty
      x Sq.:<| xs -> SeqViewElem x xs

vwrs :: Stack -> USeq -> IO ()
vwrs stk s = writeBack stk result
  where
    result = case s of
      Sq.Empty -> SeqViewEmpty
      xs Sq.:|> x -> SeqViewElem xs x

pakt :: Stack -> USeq -> IO ()
pakt stk s = pokeBi stk . UText.pack . toList $ val2char <$> s
  where
    val2char :: Val -> Char
    val2char (CharVal c) = c
    val2char c = error $ "pack text: non-character closure: " ++ show c

upkt :: Stack -> Text -> IO ()
upkt stk t =
  pokeS stk
    . Sq.fromList
    . fmap CharVal
    . UText.unpack
    $ t

pakb :: Stack -> USeq -> IO ()
pakb stk s = pokeBi stk . By.fromWord8s . fmap val2w8 $ toList s
  where
    -- TODO: Should we have a tag for bytes specifically?
    val2w8 :: Val -> Word8
    val2w8 (NatVal n) = toEnum . fromEnum $ n
    val2w8 c = error $ "pack bytes: non-natural closure: " ++ show c

upkb :: Stack -> Bytes -> IO ()
upkb stk b =
  pokeS stk . Sq.fromList . fmap (NatVal . toEnum @Word64 . fromEnum @Word8) $
    By.toWord8s b

sizb :: Stack -> Bytes -> IO ()
sizb stk b = unsafePokeIasN stk $ By.size b

fltb :: Stack -> Bytes -> IO ()
fltb stk b = pokeBi stk $ By.flatten b

-- The docs for IORef state that IORef operations can be observed
-- out of order ([1]) but actually GHC does emit the appropriate
-- load and store barriers nowadays ([2], [3]).
--
-- [1] https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-IORef.html#g:2
-- [2] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L286
-- [3] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L298
refr :: Stack -> IORef Val -> IO ()
refr stk ref = IORef.readIORef ref >>= poke stk

refn :: Stack -> Val -> IO ()
refn stk v = do
  -- Note that the CAS machinery is extremely fussy w/r to whether things are forced because it
  -- uses unsafe pointer equality. The only way we've gotten it to work as expected is with liberal
  -- forcing of the values and tickets.
  !v <- evaluate v
  ref <- IORef.newIORef v
  pokeBi stk ref

rrfc :: CCache -> Stack -> IORef Val -> IO ()
rrfc env stk ref
  | sandboxed env = die "attempted to use sandboxed operation: Ref.readForCAS"
  | otherwise = do
      ticket <- Atomic.readForCAS ref
      pokeBi stk ticket

tikr :: Stack -> Atomic.Ticket Val -> IO ()
tikr stk t = poke stk (Atomic.peekTicket t)

miss :: CCache -> Stack -> Referent -> IO ()
miss env stk tl
  | sandboxed env = die "attempted to use sandboxed operation: isMissing"
  | otherwise = case tl of
      Ref link -> do
        m <- readTVarIO (intermed env)
        pokeBool stk (link `M.member` m)
      _ -> die "exec:prim1:MISS: expected Ref"
{-# inline miss #-}

sdbl :: CCache -> Stack -> Referent -> IO ()
sdbl env stk tl = writeBack stk =<< sandboxList env tl
{-# inline sdbl #-}

sandboxList :: CCache -> Referent -> IO [Reference]
sandboxList cc (Ref r) = do
  sands <- readTVarIO $ sandbox cc
  pure . maybe [] S.toList $ M.lookup r sands
sandboxList _ _ = pure []

lkup :: CCache -> Stack -> Referent -> IO ()
lkup env stk tl
  | sandboxed env = die "attempted to use sandboxed operation: lookup"
  | otherwise = writeBack stk =<< lookupCode env tl
{-# inline lkup #-}

addi :: Stack -> Int -> Int -> IO ()
addi stk m n = pokeI stk (m + n)
{-# inline addi #-}

subi :: Stack -> Int -> Int -> IO ()
subi stk m n = pokeI stk (m - n)
{-# inline subi #-}

muli :: Stack -> Int -> Int -> IO ()
muli stk m n = pokeI stk (m * n)
{-# inline muli #-}

divi :: Stack -> Int -> Int -> IO ()
divi stk m n = pokeI stk (m `div` n)
{-# inline divi #-}

modi :: Stack -> Int -> Int -> IO ()
modi stk m n = pokeI stk (m `mod` n)
{-# inline modi #-}

eqli :: Stack -> Int -> Int -> IO ()
eqli stk m n = pokeBool stk (m == n)
{-# inline eqli #-}

neqi :: Stack -> Int -> Int -> IO ()
neqi stk m n = pokeBool stk (m /= n)
{-# inline neqi #-}

leqi :: Stack -> Int -> Int -> IO ()
leqi stk m n = pokeBool stk (m <= n)
{-# inline leqi #-}

lesi :: Stack -> Int -> Int -> IO ()
lesi stk m n = pokeBool stk (m < n)
{-# inline lesi #-}

andi :: Stack -> Int -> Int -> IO ()
andi stk m n = pokeI stk (m .&. n)
{-# inline andi #-}

iori :: Stack -> Int -> Int -> IO ()
iori stk m n = pokeI stk (m .|. n)
{-# inline iori #-}

xori :: Stack -> Int -> Int -> IO ()
xori stk m n = pokeI stk (m `xor` n)
{-# inline xori #-}

shli :: Stack -> Int -> Int -> IO ()
shli stk m n = pokeI stk (m `shiftL` n)
{-# inline shli #-}

shri :: Stack -> Int -> Int -> IO ()
shri stk m n = pokeI stk (m `shiftR` n)
{-# inline shri #-}

powi :: Stack -> Int -> Word64 -> IO ()
powi stk m n = pokeI stk (m ^ n)
{-# inline powi #-}

addn :: Stack -> Word64 -> Word64 -> IO ()
addn stk m n = pokeN stk (m + n)
{-# inline addn #-}

subn :: Stack -> Word64 -> Word64 -> IO ()
subn stk m n = pokeI stk . fromIntegral $ m - n
{-# inline subn #-}

muln :: Stack -> Word64 -> Word64 -> IO ()
muln stk m n = pokeN stk (m * n)
{-# inline muln #-}

divn :: Stack -> Word64 -> Word64 -> IO ()
divn stk m n = pokeN stk (m `div` n)
{-# inline divn #-}

modn :: Stack -> Word64 -> Word64 -> IO ()
modn stk m n = pokeN stk (m `mod` n)
{-# inline modn #-}

shln :: Stack -> Word64 -> Int -> IO ()
shln stk m n = pokeN stk (m `shiftL` n)
{-# inline shln #-}

shrn :: Stack -> Word64 -> Int -> IO ()
shrn stk m n = pokeN stk (m `shiftR` n)
{-# inline shrn #-}

pown :: Stack -> Word64 -> Word64 -> IO ()
pown stk m n = pokeN stk (m ^ n)
{-# inline pown #-}

eqln :: Stack -> Word64 -> Word64 -> IO ()
eqln stk m n = pokeBool stk (m == n)
{-# inline eqln #-}

neqn :: Stack -> Word64 -> Word64 -> IO ()
neqn stk m n = pokeBool stk (m /= n)
{-# inline neqn #-}

leqn :: Stack -> Word64 -> Word64 -> IO ()
leqn stk m n = pokeBool stk (m <= n)
{-# inline leqn #-}

lesn :: Stack -> Word64 -> Word64 -> IO ()
lesn stk m n = pokeBool stk (m < n)
{-# inline lesn #-}

andn :: Stack -> Word64 -> Word64 -> IO ()
andn stk m n = pokeN stk (m .&. n)
{-# inline andn #-}

iorn :: Stack -> Word64 -> Word64 -> IO ()
iorn stk m n = pokeN stk (m .|. n)
{-# inline iorn #-}

xorn :: Stack -> Word64 -> Word64 -> IO ()
xorn stk m n = pokeN stk (m `xor` n)
{-# inline xorn #-}

drpn :: Stack -> Word64 -> Word64 -> IO ()
drpn stk m n = pokeN stk $ if n >= m then 0 else m - n
{-# inline drpn #-}

eqlf :: Stack -> Double -> Double -> IO ()
eqlf stk x y = pokeBool stk (x == y)
{-# inline eqlf #-}

neqf :: Stack -> Double -> Double -> IO ()
neqf stk x y = pokeBool stk (x /= y)
{-# inline neqf #-}

leqf :: Stack -> Double -> Double -> IO ()
leqf stk x y = pokeBool stk (x <= y)
{-# inline leqf #-}

lesf :: Stack -> Double -> Double -> IO ()
lesf stk x y = pokeBool stk (x < y)
{-# inline lesf #-}

addf :: Stack -> Double -> Double -> IO ()
addf stk x y = pokeD stk (x + y)
{-# inline addf #-}

subf :: Stack -> Double -> Double -> IO ()
subf stk x y = pokeD stk (x - y)
{-# inline subf #-}

mulf :: Stack -> Double -> Double -> IO ()
mulf stk x y = pokeD stk (x * y)
{-# inline mulf #-}

divf :: Stack -> Double -> Double -> IO ()
divf stk x y = pokeD stk (x / y)
{-# inline divf #-}

atn2 :: Stack -> Double -> Double -> IO ()
atn2 stk x y = pokeD stk (atan2 x y)
{-# inline atn2 #-}

powf :: Stack -> Double -> Double -> IO ()
powf stk x y = pokeD stk (x ** y)
{-# inline powf #-}

logb :: Stack -> Double -> Double -> IO ()
logb stk x y = pokeD stk (logBase x y)
{-# inline logb #-}

maxf :: Stack -> Double -> Double -> IO ()
maxf stk x y = pokeD stk (max x y)
{-# inline maxf #-}

minf :: Stack -> Double -> Double -> IO ()
minf stk x y = pokeD stk (min x y)
{-# inline minf #-}

drpt :: Stack -> Int -> Text -> IO ()
drpt stk n t0 = pokeBi stk t
  where
    -- Note; if n < 0, the Nat argument was greater than the maximum
    -- signed integer. As an approximation, just return the empty
    -- string, as a string larger than this would require an absurd
    -- amount of memory.
    t | n < 0 = UText.empty
      | otherwise = UText.drop n t0

takt :: Stack -> Int -> Text -> IO ()
takt stk n t0 = pokeBi stk t
  where
    -- Note: if n < 0, the Nat argument was greater than the maximum
    -- signed integer. As an approximation, we just return the
    -- original string, because it's unlikely such a large string
    -- exists.
    t | n < 0 = t0
      | otherwise = UText.take n t0

catt :: Stack -> Text -> Text -> IO ()
catt stk x y = pokeBi stk (x <> y :: UText.Text)
{-# inline catt #-}

ixot :: Stack -> Text -> Text -> IO ()
ixot stk x y = writeBack stk $ UText.indexOf x y
{-# inline ixot #-}

eqlt :: Stack -> Text -> Text -> IO ()
eqlt stk x y = pokeBool stk $ x == y
{-# inline eqlt #-}

leqt :: Stack -> Text -> Text -> IO ()
leqt stk x y = pokeBool stk $ x <= y
{-# inline leqt #-}

lest :: Stack -> Text -> Text -> IO ()
lest stk x y = pokeBool stk $ x < y
{-# inline lest #-}

eqlu :: Stack -> Val -> Val -> IO ()
eqlu stk x y = pokeBool stk $ universalEq (==) x y
{-# inline eqlu #-}

cmpu :: Stack -> Val -> Val -> IO ()
cmpu stk x y = pokeI stk . pred . fromEnum $ universalCompare compare x y
{-# inline cmpu #-}

lequ :: Stack -> Val -> Val -> IO ()
lequ stk x y = pokeBool stk $ universalCompare compare x y /= GT
{-# inline lequ #-}

lesu :: Stack -> Val -> Val -> IO ()
lesu stk x y = pokeBool stk $ universalCompare compare x y == LT
{-# inline lesu #-}

-- Note: if n < 0, then the Nat argument was larger than the largest
-- signed integer. Seq actually doesn't handle this well, despite it
-- being possible to build (lazy) sequences this large. So,
-- approximate by yielding the empty sequence.
drps :: Stack -> Int -> USeq -> IO ()
drps stk n s = pokeS stk $ if n < 0 then Sq.empty else Sq.drop n s
{-# inline drps #-}

taks :: Stack -> Int -> USeq -> IO ()
taks stk n s = pokeS stk $ if n < 0 then s else Sq.take n s
{-# inline taks #-}

cons :: Stack -> Val -> USeq -> IO ()
cons stk x s = pokeS stk $ x Sq.<| s
{-# inline cons #-}

snoc :: Stack -> USeq -> Val -> IO ()
snoc stk s x = pokeS stk $ s Sq.|> x
{-# inline snoc #-}

idxs :: Stack -> Int -> USeq -> IO ()
idxs stk n s = writeBack stk $ Sq.lookup n s
{-# inline idxs #-}

data SeqView a b = SeqViewEmpty | SeqViewElem a b

decodeSeqView ::
  ForeignConvention a =>
  ForeignConvention b =>
  Closure -> IO (SeqView a b)
decodeSeqView (Enum _ t)
  | t == Ty.seqViewEmptyTag = pure SeqViewEmpty
decodeSeqView (Data2 _ t x y)
  | t == Ty.seqViewElemTag = SeqViewElem <$> decodeVal x <*> decodeVal y
decodeSeqView v = foreignConventionError "Either" (BoxedVal v)

seqViewE :: Closure
seqViewE = Enum Ty.seqViewRef Ty.seqViewEmptyTag

encodeSeqView ::
  ForeignConvention a =>
  ForeignConvention b =>
  SeqView a b -> Closure
encodeSeqView SeqViewEmpty = seqViewE
encodeSeqView (SeqViewElem x y) =
  Data2 Ty.seqViewRef Ty.seqViewElemTag (encodeVal x) (encodeVal y)

instance ( ForeignConvention a
         , ForeignConvention b
         ) => ForeignConvention (SeqView a b) where
  readAtIndex stk i = decodeSeqView =<< bpeekOff stk i

  decodeVal (BoxedVal c) = decodeSeqView c
  decodeVal v = foreignConventionError "SeqView" v

  readsAt stk (VArg1 i) = readArg stk i
  readsAt _ args = readsAtError "one argument" args

  encodeVal = BoxedVal . encodeSeqView

  writeBack stk v = bpoke stk $ encodeSeqView v

spll :: Stack -> Int -> USeq -> IO ()
spll stk n s = writeBack stk result
  where
    result | Sq.length s < n = SeqViewEmpty
           | (l, r) <- Sq.splitAt n s = SeqViewElem l r
{-# inline spll #-}

splr :: Stack -> Int -> USeq -> IO ()
splr stk n s = writeBack stk result
  where
    result | Sq.length s < n = SeqViewEmpty
           | (l, r) <- Sq.splitAt (Sq.length s - n) s = SeqViewElem l r
{-# inline splr #-}

cats :: Stack -> USeq -> USeq -> IO ()
cats stk x y = pokeS stk $ x Sq.>< y
{-# inline cats #-}

-- If n < 0, the Nat argument was larger than the maximum signed
-- integer. Building a value this large would reuire an absurd
-- amount of memory, so just assume n is larger.
takb :: Stack -> Int -> Bytes -> IO ()
takb stk n b = pokeBi stk $ if n < 0 then b else By.take n b
{-# inline takb #-}

-- See above for n < 0
drpb :: Stack -> Int -> Bytes -> IO ()
drpb stk n b = pokeBi stk $ if n < 0 then By.empty else By.drop n b
{-# inline drpb #-}

idxb :: Stack -> Int -> Bytes -> IO ()
idxb stk n b = writeBack stk $ By.at n b
{-# inline idxb #-}

catb :: Stack -> Bytes -> Bytes -> IO ()
catb stk l r = writeBack stk $ l <> r
{-# inline catb #-}

ixob :: Stack -> Bytes -> Bytes -> IO ()
ixob stk l r = writeBack stk $ By.indexOf l r
{-# inline ixob #-}

refw :: Stack -> IORef Val -> Val -> IO ()
refw stk ref v = IORef.writeIORef ref v *> bpoke stk unitClosure
{-# inline refw #-}

cast :: Stack -> Int -> Int -> IO ()
cast stk n tag = poke stk $ UnboxedVal n (unboxedTypeTagFromInt tag)
{-# inline cast #-}

andb :: Stack -> Bool -> Bool -> IO ()
andb stk x y = pokeBool stk $ x && y
{-# inline andb #-}

iorb :: Stack -> Bool -> Bool -> IO ()
iorb stk x y = pokeBool stk $ x || y
{-# inline iorb #-}

-- Universal comparison functions

closureNum :: Closure -> Int
closureNum PAp {} = 0
closureNum DataC {} = 1
closureNum Captured {} = 2
closureNum Foreign {} = 3
closureNum UnboxedTypeTag {} = 4
closureNum BlackHole {} = 5

universalEq ::
  (Foreign -> Foreign -> Bool) ->
  Val ->
  Val ->
  Bool
universalEq frn = eqVal
  where
    eql :: (a -> b -> Bool) -> [a] -> [b] -> Bool
    eql cm l r = length l == length r && and (zipWith cm l r)
    eqVal :: Val -> Val -> Bool
    eqVal (UnboxedVal v1 t1) (UnboxedVal v2 t2) = matchUnboxedTypes t1 t2 && v1 == v2
    eqVal (BoxedVal x) (BoxedVal y) = eqc x y
    eqVal _ _ = False
    eqc :: Closure -> Closure -> Bool
    eqc (DataC _ ct1 [w1]) (DataC _ ct2 [w2]) =
      matchTags ct1 ct2 && eqVal w1 w2
    eqc (DataC _ ct1 vs1) (DataC _ ct2 vs2) =
      ct1 == ct2
        && eqValList vs1 vs2
    eqc (PApV cix1 _ segs1) (PApV cix2 _ segs2) =
      cix1 == cix2
        && eqValList segs1 segs2
    eqc (CapV k1 a1 vs1) (CapV k2 a2 vs2) =
      eqK k1 k2
        && a1 == a2
        && eqValList vs1 vs2
    eqc (Foreign fl) (Foreign fr)
      | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
        Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr =
          arrayEq eqVal al ar
      | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
        Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr =
          length sl == length sr && and (Sq.zipWith eqVal sl sr)
      | otherwise = frn fl fr
    eqc c d = closureNum c == closureNum d

    eqValList :: [Val] -> [Val] -> Bool
    eqValList vs1 vs2 = eql eqVal vs1 vs2

    eqK :: K -> K -> Bool
    eqK KE KE = True
    eqK (CB cb) (CB cb') = cb == cb'
    eqK (Mark a ps m k) (Mark a' ps' m' k') =
      a == a' && ps == ps' && liftEq eqVal m m' && eqK k k'
    eqK (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') =
      f == f' && a == a' && ci == ci' && eqK k k'
    eqK _ _ = False

-- IEEE floating point layout is such that comparison as integers
-- somewhat works. Positive floating values map to positive integers
-- and negatives map to negatives. The corner cases are:
--
--   1. If both numbers are negative, ordering is flipped.
--   2. There is both +0 and -0, with -0 being represented as the
--      minimum signed integer.
--   3. NaN does weird things.
--
-- So, the strategy here is to compare normally if one argument is
-- positive, since positive numbers compare normally to others.
-- Otherwise, the sign bit is cleared and the numbers are compared
-- backwards. Clearing the sign bit maps -0 to +0 and maps a negative
-- number to its absolute value (including infinities). The multiple
-- NaN values are just handled according to bit patterns, rather than
-- IEEE specified behavior.
--
-- Transitivity is somewhat non-obvious for this implementation.
--
--   if i <= j and j <= k
--     if j > 0 then k > 0, so all 3 comparisons use `compare`
--     if k > 0 then k > i, since i <= j <= 0
--     if all 3 are <= 0, all 3 comparisons use the alternate
--       comparison, which is transitive via `compare`
compareAsFloat :: Int -> Int -> Ordering
compareAsFloat i j
  | i > 0 || j > 0 = compare i j
  | otherwise = compare (clear j) (clear i)
  where
    clear k = clearBit k 64

universalCompare ::
  (Foreign -> Foreign -> Ordering) ->
  Val ->
  Val ->
  Ordering
universalCompare frn = cmpVal False
  where
    cmpVal :: Bool -> Val -> Val -> Ordering
    cmpVal tyEq = \cases
      (BoxedVal c1) (BoxedVal c2) -> cmpc tyEq c1 c2
      (UnboxedVal {}) (BoxedVal {}) -> LT
      (BoxedVal {}) (UnboxedVal {}) -> GT
      (NatVal i) (NatVal j) -> compare i j
      (UnboxedVal v1 t1) (UnboxedVal v2 t2) -> cmpUnboxed tyEq (t1, v1) (t2, v2)
    cmpl :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
    cmpl cm l r =
      compare (length l) (length r) <> fold (zipWith cm l r)
    cmpc :: Bool -> Closure -> Closure -> Ordering
    cmpc tyEq = \cases
      (DataC rf1 ct1 vs1) (DataC rf2 ct2 vs2) ->
        (if tyEq && ct1 /= ct2 then compare rf1 rf2 else EQ)
          <> compare (maskTags ct1) (maskTags ct2)
          -- when comparing corresponding `Any` values, which have
          -- existentials inside check that type references match
          <> cmpValList (tyEq || rf1 == Rf.anyRef) vs1 vs2
      (PApV cix1 _ segs1) (PApV cix2 _ segs2) ->
        compare cix1 cix2
          <> cmpValList tyEq segs1 segs2
      (CapV k1 a1 vs1) (CapV k2 a2 vs2) ->
        cmpK tyEq k1 k2
          <> compare a1 a2
          <> cmpValList True vs1 vs2
      (Foreign fl) (Foreign fr)
        | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
          Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr ->
            fold (Sq.zipWith (cmpVal tyEq) sl sr)
              <> compare (length sl) (length sr)
        | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
          Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr ->
            arrayCmp (cmpVal tyEq) al ar
        | otherwise -> frn fl fr
      (UnboxedTypeTag t1) (UnboxedTypeTag t2) -> compare t1 t2
      (BlackHole) (BlackHole) -> EQ
      c d -> comparing closureNum c d

    cmpUnboxed :: Bool -> (UnboxedTypeTag, Int) -> (UnboxedTypeTag, Int) -> Ordering
    cmpUnboxed tyEq = \cases
      -- Need to cast to Nat or else maxNat == -1 and it flips comparisons of large Nats.
      -- TODO: Investigate whether bit-twiddling is faster than using Haskell's fromIntegral.
      (IntTag, n1) (IntTag, n2) -> compare n1 n2
      (NatTag, n1) (NatTag, n2) -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (NatTag, n1) (IntTag, n2)
        | n2 < 0 -> GT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (IntTag, n1) (NatTag, n2)
        | n1 < 0 -> LT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (FloatTag, n1) (FloatTag, n2) -> compareAsFloat n1 n2
      (t1, v1) (t2, v2) ->
        Monoid.whenM tyEq (compare t1 t2)
          <> compare v1 v2

    cmpValList :: Bool -> [Val] -> [Val] -> Ordering
    cmpValList tyEq vs1 vs2 = cmpl (cmpVal tyEq) vs1 vs2

    cmpK :: Bool -> K -> K -> Ordering
    cmpK tyEq = \cases
      KE KE -> EQ
      (CB cb) (CB cb') -> compare cb cb'
      (Mark a ps m k) (Mark a' ps' m' k') ->
        compare a a'
          <> compare ps ps'
          <> liftCompare (cmpVal tyEq) m m'
          <> cmpK tyEq k k'
      (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') ->
        compare f f'
          <> compare a a'
          <> compare ci ci'
          <> cmpK tyEq k k'
      KE _ -> LT
      _ KE -> GT
      (CB {}) _ -> LT
      _ (CB {}) -> GT
      (Mark {}) _ -> LT
      _ (Mark {}) -> GT

arrayCmp ::
  (a -> a -> Ordering) ->
  PA.Array a ->
  PA.Array a ->
  Ordering
arrayCmp cmpVal l r =
  comparing PA.sizeofArray l r <> go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = EQ
      | otherwise = cmpVal (PA.indexArray l i) (PA.indexArray r i) <> go (i - 1)

arrayEq :: (a -> a -> Bool) -> PA.Array a -> PA.Array a -> Bool
arrayEq eqc l r
  | PA.sizeofArray l /= PA.sizeofArray r = False
  | otherwise = go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = True
      | otherwise = eqc (PA.indexArray l i) (PA.indexArray r i) && go (i - 1)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchTags :: PackedTag -> PackedTag -> Bool
matchTags ct1 ct2 =
  ct1 == ct2
    || (ct1 == Ty.intTag && ct2 == Ty.natTag)
    || (ct1 == Ty.natTag && ct2 == Ty.intTag)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchUnboxedTypes :: UnboxedTypeTag -> UnboxedTypeTag -> Bool
matchUnboxedTypes ct1 ct2 =
  ct1 == ct2
    || (ct1 == IntTag && ct2 == NatTag)
    || (ct1 == NatTag && ct2 == IntTag)

