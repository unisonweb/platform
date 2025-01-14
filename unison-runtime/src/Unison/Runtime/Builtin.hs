{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Unison.Runtime.Builtin
  ( builtinTermNumbering,
    builtinTypeNumbering,
    builtinTermBackref,
    builtinTypeBackref,
    builtinArities,
    builtinInlineInfo,
    numberedTermLookup,
    Sandbox (..),
    baseSandboxInfo,
    unitValue,
    natValue,
    builtinForeignNames,
    sandboxedForeignFuncs,
  )
where

import Control.Monad.State.Strict (State, execState, modify)
import Data.Map qualified as Map
import Data.Set (insert)
import Data.Set qualified as Set
import Data.Text qualified
import Unison.ABT.Normalized hiding (TTm)
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Runtime.ANF as ANF
import Unison.Runtime.Builtin.Types
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..), foreignFuncBuiltinName)
import Unison.Runtime.Stack (UnboxedTypeTag (..), Val (..), unboxedTypeTagToInt)
import Unison.Runtime.Stack qualified as Closure
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Unison.Util.Text qualified as Util.Text
import Unison.Var

freshes :: (Var v) => Int -> [v]
freshes = freshes' mempty

freshes' :: (Var v) => Set v -> Int -> [v]
freshes' avoid0 = go avoid0 []
  where
    go _ vs 0 = vs
    go avoid vs n =
      let v = freshIn avoid $ typed ANFBlank
       in go (insert v avoid) (v : vs) (n - 1)

class Fresh t where fresh :: t

fresh1 :: (Var v) => v
fresh1 = head $ freshes 1

instance (Var v) => Fresh (v, v) where
  fresh = (v1, v2)
    where
      [v1, v2] = freshes 2

instance (Var v) => Fresh (v, v, v) where
  fresh = (v1, v2, v3)
    where
      [v1, v2, v3] = freshes 3

instance (Var v) => Fresh (v, v, v, v) where
  fresh = (v1, v2, v3, v4)
    where
      [v1, v2, v3, v4] = freshes 4

instance (Var v) => Fresh (v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5)
    where
      [v1, v2, v3, v4, v5] = freshes 5

instance (Var v) => Fresh (v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6)
    where
      [v1, v2, v3, v4, v5, v6] = freshes 6

instance (Var v) => Fresh (v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7)
    where
      [v1, v2, v3, v4, v5, v6, v7] = freshes 7

instance (Var v) => Fresh (v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8] = freshes 8

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9] = freshes 9

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10] = freshes 10

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11] = freshes 11

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13] = freshes 13

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14] = freshes 14

fls, tru :: (Var v) => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

none :: (Var v) => ANormal v
none = TCon Ty.optionalRef (fromIntegral Ty.noneId) []

some, left, right :: (Var v) => v -> ANormal v
some a = TCon Ty.optionalRef (fromIntegral Ty.someId) [a]
left x = TCon Ty.eitherRef (fromIntegral Ty.eitherLeftId) [x]
right x = TCon Ty.eitherRef (fromIntegral Ty.eitherRightId) [x]

seqViewEmpty :: (Var v) => ANormal v
seqViewEmpty = TCon Ty.seqViewRef (fromIntegral Ty.seqViewEmpty) []

seqViewElem :: (Var v) => v -> v -> ANormal v
seqViewElem l r = TCon Ty.seqViewRef (fromIntegral Ty.seqViewElem) [l, r]

unop0 :: (Var v) => Int -> ([v] -> ANormal v) -> SuperNormal v
unop0 n f =
  Lambda [BX]
    . TAbss [x0]
    $ f xs
  where
    xs@(x0 : _) = freshes (1 + n)

binop0 :: (Var v) => Int -> ([v] -> ANormal v) -> SuperNormal v
binop0 n f =
  Lambda [BX, BX]
    . TAbss [x0, y0]
    $ f xs
  where
    xs@(x0 : y0 : _) = freshes (2 + n)

unop :: (Var v) => POp -> SuperNormal v
unop pop =
  unop0 0 $ \[x] ->
    (TPrm pop [x])

binop ::
  (Var v) =>
  POp ->
  SuperNormal v
binop pop =
  binop0 0 $ \[x, y] -> TPrm pop [x, y]

-- | Like `binop`, but swaps the arguments.
binopSwap :: (Var v) => POp -> SuperNormal v
binopSwap pop =
  binop0 0 $ \[x, y] -> TPrm pop [y, x]

addi, subi, muli, divi, modi, shli, shri, powi :: (Var v) => SuperNormal v
addi = binop ADDI
subi = binop SUBI
muli = binop MULI
divi = binop DIVI
modi = binop MODI
shli = binop SHLI
shri = binop SHRI
powi = binop POWI

addn, subn, muln, divn, modn, shln, shrn, pown, dropn :: (Var v) => SuperNormal v
addn = binop ADDN
subn = binop SUBN
muln = binop MULN
divn = binop DIVN
modn = binop MODN
shln = binop SHLN
shrn = binop SHRN
pown = binop POWN
dropn = binop DRPN

eqi, eqn, lti, ltn, lei, len :: (Var v) => SuperNormal v
eqi = binop EQLI
lti = binop LESI
lei = binop LEQI
eqn = binop EQLN
ltn = binop LESN
len = binop LEQN

gti, gtn, gei, gen :: (Var v) => SuperNormal v
gti = binopSwap LESI
gei = binopSwap LEQI
gtn = binopSwap LESN
gen = binopSwap LEQN

inci, incn :: (Var v) => SuperNormal v
inci = unop INCI
incn = unop INCN

sgni, negi :: (Var v) => SuperNormal v
sgni = unop SGNI
negi = unop NEGI

lzeron, tzeron, lzeroi, tzeroi, popn, popi :: (Var v) => SuperNormal v
lzeron = unop LZRO
tzeron = unop TZRO
popn = unop POPC
popi = unop POPC
lzeroi = unop LZRO
tzeroi = unop TZRO

andn, orn, xorn, compln, andi, ori, xori, compli :: (Var v) => SuperNormal v
andn = binop ANDN
orn = binop IORN
xorn = binop XORN
compln = unop COMN
andi = binop ANDI
ori = binop IORI
xori = binop XORI
compli = unop COMI

addf,
  subf,
  mulf,
  divf,
  powf,
  sqrtf,
  logf,
  logbf ::
    (Var v) => SuperNormal v
addf = binop ADDF
subf = binop SUBF
mulf = binop MULF
divf = binop DIVF
powf = binop POWF
sqrtf = unop SQRT
logf = unop LOGF
logbf = binop LOGB

expf, absf :: (Var v) => SuperNormal v
expf = unop EXPF
absf = unop ABSF

cosf, sinf, tanf, acosf, asinf, atanf :: (Var v) => SuperNormal v
cosf = unop COSF
sinf = unop SINF
tanf = unop TANF
acosf = unop ACOS
asinf = unop ASIN
atanf = unop ATAN

coshf,
  sinhf,
  tanhf,
  acoshf,
  asinhf,
  atanhf,
  atan2f ::
    (Var v) => SuperNormal v
coshf = unop COSH
sinhf = unop SINH
tanhf = unop TANH
acoshf = unop ACSH
asinhf = unop ASNH
atanhf = unop ATNH
atan2f = binop ATN2

ltf, gtf, lef, gef, eqf, neqf :: (Var v) => SuperNormal v
ltf = binop LESF
gtf = binopSwap LESF
lef = binop LEQF
gef = binopSwap LEQF
eqf = binop EQLF
neqf = binop NEQF

minf, maxf :: (Var v) => SuperNormal v
minf = binop MINF
maxf = binop MAXF

ceilf, floorf, truncf, roundf, i2f, n2f :: (Var v) => SuperNormal v
ceilf = unop CEIL
floorf = unop FLOR
truncf = unop TRNF
roundf = unop RNDF
i2f = unop ITOF
n2f = unop NTOF

trni :: (Var v) => SuperNormal v
trni = unop TRNC

modular :: (Var v) => POp -> (Bool -> ANormal v) -> SuperNormal v
modular pop ret =
  unop0 2 $ \[x, m, t] ->
    TLetD t UN (TLit $ I 2)
      . TLetD m UN (TPrm pop [x, t])
      . TMatch m
      $ MatchIntegral
        (mapSingleton 1 $ ret True)
        (Just $ ret False)

evni, evnn, oddi, oddn :: (Var v) => SuperNormal v
evni = modular MODI (\b -> if b then fls else tru)
oddi = modular MODI (\b -> if b then tru else fls)
evnn = modular MODN (\b -> if b then fls else tru)
oddn = modular MODN (\b -> if b then tru else fls)

appendt, taket, dropt, indext, indexb, sizet, unconst, unsnoct :: (Var v) => SuperNormal v
appendt = binop0 0 $ \[x, y] -> TPrm CATT [x, y]
taket = binop0 0 $ \[x, y] ->
  TPrm TAKT [x, y]
dropt = binop0 0 $ \[x, y] ->
  TPrm DRPT [x, y]

atb = binop0 2 $ \[n, b, t, r] ->
  TLetD t UN (TPrm IDXB [n, b])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

indext = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IXOT [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

indexb = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IXOB [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

sizet = unop0 0 $ \[x] -> TPrm SIZT [x]

unconst = unop0 6 $ \[x, t, c, y, p, u, yp] ->
  TLetD t UN (TPrm UCNS [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN, BX],
            TAbss [c, y]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD yp BX (TCon Ty.pairRef 0 [y, u])
              . TLetD p BX (TCon Ty.pairRef 0 [c, yp])
              $ some p
          )
        )
      ]

unsnoct = unop0 6 $ \[x, t, c, y, p, u, cp] ->
  TLetD t UN (TPrm USNC [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [BX, UN],
            TAbss [y, c]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD cp BX (TCon Ty.pairRef 0 [c, u])
              . TLetD p BX (TCon Ty.pairRef 0 [y, cp])
              $ some p
          )
        )
      ]

appends, conss, snocs :: (Var v) => SuperNormal v
appends = binop0 0 $ \[x, y] -> TPrm CATS [x, y]
conss = binop0 0 $ \[x, y] -> TPrm CONS [x, y]
snocs = binop0 0 $ \[x, y] -> TPrm SNOC [x, y]

takes, drops, sizes, ats, emptys :: (Var v) => SuperNormal v
takes = binop0 0 $ \[x, y] -> TPrm TAKS [x, y]
drops = binop0 0 $ \[x, y] -> TPrm DRPS [x, y]
sizes = unop0 0 $ \[x] -> (TPrm SIZS [x])
ats = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IDXS [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        (1, ([BX], TAbs r $ some r))
      ]
emptys = Lambda [] $ TPrm BLDS []

viewls, viewrs :: (Var v) => SuperNormal v
viewls = unop0 3 $ \[s, u, h, t] ->
  TLetD u UN (TPrm VWLS [s])
    . TMatch u
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [h, t] $ seqViewElem h t))
      ]
viewrs = unop0 3 $ \[s, u, i, l] ->
  TLetD u UN (TPrm VWRS [s])
    . TMatch u
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [i, l] $ seqViewElem i l))
      ]

splitls, splitrs :: (Var v) => SuperNormal v
splitls = binop0 3 $ \[n, s, t, l, r] ->
  TLetD t UN (TPrm SPLL [n, s])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [l, r] $ seqViewElem l r))
      ]
splitrs = binop0 3 $ \[n, s, t, l, r] ->
  TLetD t UN (TPrm SPLR [n, s])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [l, r] $ seqViewElem l r))
      ]

eqt, neqt, leqt, geqt, lesst, great :: SuperNormal Symbol
eqt = binop EQLT
neqt = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm EQLT [x, y]) $
    TPrm NOTB [b]
leqt = binop LEQT
geqt = binopSwap LEQT
lesst = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [y, x]) $
    TPrm NOTB [b]
great = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [x, y]) $
    TPrm NOTB [b]

packt, unpackt :: SuperNormal Symbol
packt = unop0 0 $ \[s] -> TPrm PAKT [s]
unpackt = unop0 0 $ \[t] -> TPrm UPKT [t]

packb, unpackb, emptyb, appendb :: SuperNormal Symbol
packb = unop0 0 $ \[s] -> TPrm PAKB [s]
unpackb = unop0 0 $ \[b] -> TPrm UPKB [b]
emptyb =
  Lambda []
    . TLetD es BX (TPrm BLDS [])
    $ TPrm PAKB [es]
  where
    es = fresh1
appendb = binop0 0 $ \[x, y] -> TPrm CATB [x, y]

takeb, dropb, atb, sizeb, flattenb :: SuperNormal Symbol
takeb = binop0 0 $ \[n, b] -> TPrm TAKB [n, b]
dropb = binop0 0 $ \[n, b] -> TPrm DRPB [n, b]
sizeb = unop0 0 $ \[b] -> (TPrm SIZB [b])
flattenb = unop0 0 $ \[b] -> TPrm FLTB [b]

i2t, n2t, f2t :: SuperNormal Symbol
i2t = unop0 0 $ \[n] -> TPrm ITOT [n]
n2t = unop0 0 $ \[n] -> TPrm NTOT [n]
f2t = unop0 0 $ \[f] -> TPrm FTOT [f]

t2i, t2n, t2f :: SuperNormal Symbol
t2i = unop0 2 $ \[x, t, n] ->
  TLetD t UN (TPrm TTOI [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs n $ some n
          )
        )
      ]
t2n = unop0 2 $ \[x, t, n] ->
  TLetD t UN (TPrm TTON [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs n $ some n
          )
        )
      ]
t2f = unop0 2 $ \[x, t, f] ->
  TLetD t UN (TPrm TTOF [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs f $ some f
          )
        )
      ]

equ :: SuperNormal Symbol
equ = binop EQLU

cmpu :: SuperNormal Symbol
cmpu = binop CMPU

ltu :: SuperNormal Symbol
ltu = binop LESU

gtu :: SuperNormal Symbol
gtu = binopSwap LESU

geu :: SuperNormal Symbol
geu = binopSwap LEQU

leu :: SuperNormal Symbol
leu = binop LEQU

notb :: SuperNormal Symbol
notb = unop NOTB

orb :: SuperNormal Symbol
orb = binop IORB

andb :: SuperNormal Symbol
andb = binop ANDB

-- A runtime type-cast. Used to unsafely coerce between unboxed
-- types at runtime without changing their representation.
coerceType :: UnboxedTypeTag -> SuperNormal Symbol
coerceType destType =
  unop0 1 $ \[v, tag] ->
    TLetD tag UN (TLit $ I $ fromIntegral $ unboxedTypeTagToInt destType) $
      TPrm CAST [v, tag]

-- This version of unsafeCoerce is the identity function. It works
-- only if the two types being coerced between are actually the same,
-- because it keeps the same representation. It is not capable of
-- e.g. correctly translating between two types with compatible bit
-- representations, because tagging information will be retained.
poly'coerce :: SuperNormal Symbol
poly'coerce = unop0 0 $ \[x] -> TVar x

jumpk :: SuperNormal Symbol
jumpk = binop0 0 $ \[k, a] -> TKon k [a]

scope'run :: SuperNormal Symbol
scope'run =
  unop0 1 $ \[e, un] ->
    TLetD un BX (TCon Ty.unitRef 0 []) $
      TApp (FVar e) [un]

fork'comp :: SuperNormal Symbol
fork'comp =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    $ TPrm FORK [lz]
  where
    (act, unit, lz) = fresh

try'eval :: SuperNormal Symbol
try'eval =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    . TLetD ta UN (TPrm TFRC [lz])
    . TMatch ta
    . MatchSum
    $ mapFromList
      [ exnCase lnk msg xtra any fail,
        (1, ([BX], TAbs r (TVar r)))
      ]
  where
    (act, unit, lz, ta, lnk, msg, xtra, any, fail, r) = fresh

bug :: Util.Text.Text -> SuperNormal Symbol
bug name =
  unop0 1 $ \[x, n] ->
    TLetD n BX (TLit $ T name) $
      TPrm EROR [n, x]

watch :: SuperNormal Symbol
watch =
  binop0 0 $ \[t, v] ->
    TLets Direct [] [] (TPrm PRNT [t]) $
      TVar v

raise :: SuperNormal Symbol
raise =
  unop0 3 $ \[r, f, n, k] ->
    TMatch r
      . flip MatchRequest (TAbs f $ TVar f)
      . Map.singleton Ty.exceptionRef
      $ mapSingleton
        0
        ( [BX],
          TAbs f
            . TShift Ty.exceptionRef k
            . TLetD n BX (TLit $ T "builtin.raise")
            $ TPrm EROR [n, f]
        )

gen'trace :: SuperNormal Symbol
gen'trace =
  binop0 0 $ \[t, v] ->
    TLets Direct [] [] (TPrm TRCE [t, v]) $
      TCon Ty.unitRef 0 []

debug'text :: SuperNormal Symbol
debug'text =
  unop0 3 $ \[c, r, t, e] ->
    TLetD r UN (TPrm DBTX [c])
      . TMatch r
      . MatchSum
      $ mapFromList
        [ (0, ([], none)),
          (1, ([BX], TAbs t . TLetD e BX (left t) $ some e)),
          (2, ([BX], TAbs t . TLetD e BX (right t) $ some e))
        ]

code'missing :: SuperNormal Symbol
code'missing = unop MISS

code'cache :: SuperNormal Symbol
code'cache = unop0 0 $ \[new] -> TPrm CACH [new]

code'lookup :: SuperNormal Symbol
code'lookup =
  unop0 2 $ \[link, t, r] ->
    TLetD t UN (TPrm LKUP [link])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ (0, ([], none)),
          (1, ([BX], TAbs r $ some r))
        ]

code'validate :: SuperNormal Symbol
code'validate =
  unop0 6 $ \[item, t, ref, msg, extra, any, fail] ->
    TLetD t UN (TPrm CVLD [item])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ ( 1,
            ([BX, BX, BX],)
              . TAbss [ref, msg, extra]
              . TLetD any BX (TCon Ty.anyRef 0 [extra])
              . TLetD fail BX (TCon Ty.failureRef 0 [ref, msg, any])
              $ some fail
          ),
          ( 0,
            ([],) $
              none
          )
        ]

term'link'to'text :: SuperNormal Symbol
term'link'to'text =
  unop0 0 $ \[link] -> TPrm TLTT [link]

value'load :: SuperNormal Symbol
value'load =
  unop0 2 $ \[vlu, t, r] ->
    TLetD t UN (TPrm LOAD [vlu])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ (0, ([BX], TAbs r $ left r)),
          (1, ([BX], TAbs r $ right r))
        ]

value'create :: SuperNormal Symbol
value'create = unop0 0 $ \[x] -> TPrm VALU [x]

check'sandbox :: SuperNormal Symbol
check'sandbox = binop SDBX

sandbox'links :: SuperNormal Symbol
sandbox'links = Lambda [BX] . TAbs ln $ TPrm SDBL [ln]
  where
    ln = fresh1

value'sandbox :: SuperNormal Symbol
value'sandbox =
  Lambda [BX, BX]
    . TAbss [refs, val]
    $ TPrm SDBV [refs, val]
  where
    (refs, val) = fresh

stm'atomic :: SuperNormal Symbol
stm'atomic =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    $ TPrm ATOM [lz]
  where
    (act, unit, lz) = fresh

type ForeignOp = ForeignFunc -> ([Mem], ANormal Symbol)

any'construct :: SuperNormal Symbol
any'construct =
  unop0 0 $ \[v] ->
    TCon Ty.anyRef 0 [v]

any'extract :: SuperNormal Symbol
any'extract =
  unop0 1 $
    \[v, v1] ->
      TMatch v $
        MatchData Ty.anyRef (mapSingleton 0 $ ([BX], TAbs v1 (TVar v1))) Nothing

-- Refs

-- The docs for IORef state that IORef operations can be observed
-- out of order ([1]) but actually GHC does emit the appropriate
-- load and store barriers nowadays ([2], [3]).
--
-- [1] https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-IORef.html#g:2
-- [2] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L286
-- [3] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L298
ref'read :: SuperNormal Symbol
ref'read =
  unop0 0 $ \[ref] -> (TPrm REFR [ref])

ref'write :: SuperNormal Symbol
ref'write =
  binop0 0 $ \[ref, val] -> (TPrm REFW [ref, val])

-- In GHC, CAS returns both a Boolean and the current value of the
-- IORef, which can be used to retry a failed CAS.
-- This strategy is more efficient than returning a Boolean only
-- because it uses a single call to cmpxchg in assembly (see [1]) to
-- avoid an extra read per CAS iteration, however it's not supported
-- in Scheme.
-- Therefore, we adopt the more common signature that only returns a
-- Boolean, which doesn't even suffer from spurious failures because
-- GHC issues loads of mutable variables with memory_order_acquire
-- (see [2])
--
-- [1]: https://github.com/ghc/ghc/blob/master/rts/PrimOps.cmm#L697
-- [2]: https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L285
ref'cas :: SuperNormal Symbol
ref'cas =
  Lambda [BX, BX, BX]
    . TAbss [x, y, z]
    $ TPrm RCAS [x, y, z]
  where
    (x, y, z) = fresh

ref'ticket'read :: SuperNormal Symbol
ref'ticket'read = unop0 0 $ TPrm TIKR

ref'readForCas :: SuperNormal Symbol
ref'readForCas = unop0 0 $ TPrm RRFC

ref'new :: SuperNormal Symbol
ref'new = unop0 0 $ TPrm REFN

crypto'hash :: ForeignOp
crypto'hash instr =
  ([BX, BX],)
    . TAbss [alg, x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [alg, vl]
  where
    (alg, x, vl) = fresh

murmur'hash :: ForeignOp
murmur'hash instr =
  ([BX],)
    . TAbss [x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [vl]
  where
    (x, vl) = fresh

crypto'hmac :: ForeignOp
crypto'hmac instr =
  ([BX, BX, BX],)
    . TAbss [alg, by, x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [alg, by, vl]
  where
    (alg, by, x, vl) = fresh

exnCase ::
  (Var v) => v -> v -> v -> v -> v -> (Word64, ([Mem], ANormal v))
exnCase stack1 stack2 stack3 any fail =
  (0,)
    . ([BX, BX, BX],)
    . TAbss [stack1, stack2, stack3]
    . TLetD any BX (TCon Ty.anyRef 0 [stack3])
    . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2, any])
    $ TReq Ty.exceptionRef 0 [fail]

-- Input / Output glue
--
-- These are pairings of input and output functions to handle a
-- foreign call.  The input function represents the numbers and types
-- of the inputs to a foreign call.  The output function takes the
-- result of the foreign call and turns it into a Unison type.
--

-- a
direct :: ForeignOp
direct instr = ([], TFOp instr [])

argNDirect :: Int -> ForeignOp
argNDirect n instr =
  (replicate n BX,)
    . TAbss args
    $ TFOp instr args
  where
    args = freshes n

--  () -> a
--
--  Unit is unique in that we don't actually pass it as an arg
unitDirect :: ForeignOp
unitDirect instr = ([BX],) . TAbs arg $ TFOp instr [] where arg = fresh1

builtinLookup :: Map.Map Reference (Sandbox, SuperNormal Symbol)
builtinLookup =
  Map.fromList
    . map (\(t, f) -> (Builtin t, f))
    $ [ ("Int.+", (Untracked, addi)),
        ("Int.-", (Untracked, subi)),
        ("Int.*", (Untracked, muli)),
        ("Int./", (Untracked, divi)),
        ("Int.mod", (Untracked, modi)),
        ("Int.==", (Untracked, eqi)),
        ("Int.<", (Untracked, lti)),
        ("Int.<=", (Untracked, lei)),
        ("Int.>", (Untracked, gti)),
        ("Int.>=", (Untracked, gei)),
        ("Int.fromRepresentation", (Untracked, coerceType IntTag)),
        ("Int.toRepresentation", (Untracked, coerceType NatTag)),
        ("Int.increment", (Untracked, inci)),
        ("Int.signum", (Untracked, sgni)),
        ("Int.negate", (Untracked, negi)),
        ("Int.truncate0", (Untracked, trni)),
        ("Int.isEven", (Untracked, evni)),
        ("Int.isOdd", (Untracked, oddi)),
        ("Int.shiftLeft", (Untracked, shli)),
        ("Int.shiftRight", (Untracked, shri)),
        ("Int.trailingZeros", (Untracked, tzeroi)),
        ("Int.leadingZeros", (Untracked, lzeroi)),
        ("Int.and", (Untracked, andi)),
        ("Int.or", (Untracked, ori)),
        ("Int.xor", (Untracked, xori)),
        ("Int.complement", (Untracked, compli)),
        ("Int.pow", (Untracked, powi)),
        ("Int.toText", (Untracked, i2t)),
        ("Int.fromText", (Untracked, t2i)),
        ("Int.toFloat", (Untracked, i2f)),
        ("Int.popCount", (Untracked, popi)),
        ("Nat.+", (Untracked, addn)),
        ("Nat.-", (Untracked, subn)),
        ("Nat.sub", (Untracked, subn)),
        ("Nat.*", (Untracked, muln)),
        ("Nat./", (Untracked, divn)),
        ("Nat.mod", (Untracked, modn)),
        ("Nat.==", (Untracked, eqn)),
        ("Nat.<", (Untracked, ltn)),
        ("Nat.<=", (Untracked, len)),
        ("Nat.>", (Untracked, gtn)),
        ("Nat.>=", (Untracked, gen)),
        ("Nat.increment", (Untracked, incn)),
        ("Nat.isEven", (Untracked, evnn)),
        ("Nat.isOdd", (Untracked, oddn)),
        ("Nat.shiftLeft", (Untracked, shln)),
        ("Nat.shiftRight", (Untracked, shrn)),
        ("Nat.trailingZeros", (Untracked, tzeron)),
        ("Nat.leadingZeros", (Untracked, lzeron)),
        ("Nat.and", (Untracked, andn)),
        ("Nat.or", (Untracked, orn)),
        ("Nat.xor", (Untracked, xorn)),
        ("Nat.complement", (Untracked, compln)),
        ("Nat.pow", (Untracked, pown)),
        ("Nat.drop", (Untracked, dropn)),
        ("Nat.toInt", (Untracked, coerceType IntTag)),
        ("Nat.toFloat", (Untracked, n2f)),
        ("Nat.toText", (Untracked, n2t)),
        ("Nat.fromText", (Untracked, t2n)),
        ("Nat.popCount", (Untracked, popn)),
        ("Float.+", (Untracked, addf)),
        ("Float.-", (Untracked, subf)),
        ("Float.*", (Untracked, mulf)),
        ("Float./", (Untracked, divf)),
        ("Float.pow", (Untracked, powf)),
        ("Float.log", (Untracked, logf)),
        ("Float.logBase", (Untracked, logbf)),
        ("Float.sqrt", (Untracked, sqrtf)),
        ("Float.fromRepresentation", (Untracked, coerceType FloatTag)),
        ("Float.toRepresentation", (Untracked, coerceType NatTag)),
        ("Float.min", (Untracked, minf)),
        ("Float.max", (Untracked, maxf)),
        ("Float.<", (Untracked, ltf)),
        ("Float.>", (Untracked, gtf)),
        ("Float.<=", (Untracked, lef)),
        ("Float.>=", (Untracked, gef)),
        ("Float.==", (Untracked, eqf)),
        ("Float.!=", (Untracked, neqf)),
        ("Float.acos", (Untracked, acosf)),
        ("Float.asin", (Untracked, asinf)),
        ("Float.atan", (Untracked, atanf)),
        ("Float.cos", (Untracked, cosf)),
        ("Float.sin", (Untracked, sinf)),
        ("Float.tan", (Untracked, tanf)),
        ("Float.acosh", (Untracked, acoshf)),
        ("Float.asinh", (Untracked, asinhf)),
        ("Float.atanh", (Untracked, atanhf)),
        ("Float.cosh", (Untracked, coshf)),
        ("Float.sinh", (Untracked, sinhf)),
        ("Float.tanh", (Untracked, tanhf)),
        ("Float.exp", (Untracked, expf)),
        ("Float.abs", (Untracked, absf)),
        ("Float.ceiling", (Untracked, ceilf)),
        ("Float.floor", (Untracked, floorf)),
        ("Float.round", (Untracked, roundf)),
        ("Float.truncate", (Untracked, truncf)),
        ("Float.atan2", (Untracked, atan2f)),
        ("Float.toText", (Untracked, f2t)),
        ("Float.fromText", (Untracked, t2f)),
        -- text
        ("Text.empty", (Untracked, Lambda [] $ TLit (T ""))),
        ("Text.++", (Untracked, appendt)),
        ("Text.take", (Untracked, taket)),
        ("Text.drop", (Untracked, dropt)),
        ("Text.indexOf", (Untracked, indext)),
        ("Text.size", (Untracked, sizet)),
        ("Text.==", (Untracked, eqt)),
        ("Text.!=", (Untracked, neqt)),
        ("Text.<=", (Untracked, leqt)),
        ("Text.>=", (Untracked, geqt)),
        ("Text.<", (Untracked, lesst)),
        ("Text.>", (Untracked, great)),
        ("Text.uncons", (Untracked, unconst)),
        ("Text.unsnoc", (Untracked, unsnoct)),
        ("Text.toCharList", (Untracked, unpackt)),
        ("Text.fromCharList", (Untracked, packt)),
        ("Boolean.not", (Untracked, notb)),
        ("Boolean.or", (Untracked, orb)),
        ("Boolean.and", (Untracked, andb)),
        ("bug", (Untracked, bug "builtin.bug")),
        ("todo", (Untracked, bug "builtin.todo")),
        ("Debug.watch", (Tracked, watch)),
        ("Debug.trace", (Tracked, gen'trace)),
        ("Debug.toText", (Tracked, debug'text)),
        ("unsafe.coerceAbilities", (Untracked, poly'coerce)),
        ("Char.toNat", (Untracked, coerceType NatTag)),
        ("Char.fromNat", (Untracked, coerceType CharTag)),
        ("Bytes.empty", (Untracked, emptyb)),
        ("Bytes.fromList", (Untracked, packb)),
        ("Bytes.toList", (Untracked, unpackb)),
        ("Bytes.++", (Untracked, appendb)),
        ("Bytes.take", (Untracked, takeb)),
        ("Bytes.drop", (Untracked, dropb)),
        ("Bytes.at", (Untracked, atb)),
        ("Bytes.indexOf", (Untracked, indexb)),
        ("Bytes.size", (Untracked, sizeb)),
        ("Bytes.flatten", (Untracked, flattenb)),
        ("List.take", (Untracked, takes)),
        ("List.drop", (Untracked, drops)),
        ("List.size", (Untracked, sizes)),
        ("List.++", (Untracked, appends)),
        ("List.at", (Untracked, ats)),
        ("List.cons", (Untracked, conss)),
        ("List.snoc", (Untracked, snocs)),
        ("List.empty", (Untracked, emptys)),
        ("List.viewl", (Untracked, viewls)),
        ("List.viewr", (Untracked, viewrs)),
        ("List.splitLeft", (Untracked, splitls)),
        ("List.splitRight", (Untracked, splitrs)),
        --
        --   , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
        ("Universal.==", (Untracked, equ)),
        ("Universal.compare", (Untracked, cmpu)),
        ("Universal.>", (Untracked, gtu)),
        ("Universal.<", (Untracked, ltu)),
        ("Universal.>=", (Untracked, geu)),
        ("Universal.<=", (Untracked, leu)),
        -- internal stuff
        ("jumpCont", (Untracked, jumpk)),
        ("raise", (Untracked, raise)),
        ("IO.forkComp.v2", (Tracked, fork'comp)),
        ("Scope.run", (Untracked, scope'run)),
        ("Code.isMissing", (Tracked, code'missing)),
        ("Code.cache_", (Tracked, code'cache)),
        ("Code.lookup", (Tracked, code'lookup)),
        ("Code.validate", (Tracked, code'validate)),
        ("Value.load", (Tracked, value'load)),
        ("Value.value", (Tracked, value'create)),
        ("Any.Any", (Untracked, any'construct)),
        ("Any.unsafeExtract", (Untracked, any'extract)),
        ("Link.Term.toText", (Untracked, term'link'to'text)),
        ("STM.atomically", (Tracked, stm'atomic)),
        ("validateSandboxed", (Untracked, check'sandbox)),
        ("Value.validateSandboxed", (Tracked, value'sandbox)),
        ("sandboxLinks", (Tracked, sandbox'links)),
        ("IO.tryEval", (Tracked, try'eval)),
        ("Ref.read", (Untracked, ref'read)),
        ("Ref.write", (Untracked, ref'write)),
        ("Ref.cas", (Tracked, ref'cas)),
        ("Ref.Ticket.read", (Tracked, ref'ticket'read)),
        ("Ref.readForCas", (Tracked, ref'readForCas)),
        ("Scope.ref", (Untracked, ref'new)),
        ("IO.ref", (Tracked, ref'new))
      ]
      ++ foreignWrappers

type FDecl v = State (Map ForeignFunc (Sandbox, SuperNormal v))

-- Data type to determine whether a builtin should be tracked for
-- sandboxing. Untracked means that it can be freely used, and Tracked
-- means that the sandboxing check will by default consider them
-- disallowed.
data Sandbox = Tracked | Untracked
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

declareForeignWrap ::
  Sandbox ->
  ForeignOp ->
  ForeignFunc ->
  FDecl Symbol ()
declareForeignWrap sand wrap func =
  modify $ Map.insert func (sand, code)
  where
    code = uncurry Lambda (wrap func)

declareForeign ::
  Sandbox ->
  Int ->
  ForeignFunc ->
  FDecl Symbol ()
declareForeign sand arity func = declareForeignWrap sand wrap func
  where
  -- Special case: turn 0-arg foreigns into unit-accepting functions
  wrap | 0 == arity = unitDirect
       | otherwise = argNDirect arity

unitValue :: Val
unitValue = BoxedVal $ Closure.Enum Ty.unitRef TT.unitTag

natValue :: Word64 -> Val
natValue w = NatVal w

declareUdpForeigns :: FDecl Symbol ()
declareUdpForeigns = do
  declareForeign Tracked 2 IO_UDP_clientSocket_impl_v1

  declareForeign Tracked 1 IO_UDP_UDPSocket_recv_impl_v1

  declareForeign Tracked 2 IO_UDP_UDPSocket_send_impl_v1
  declareForeign Tracked 1 IO_UDP_UDPSocket_close_impl_v1

  declareForeign Tracked 1 IO_UDP_ListenSocket_close_impl_v1

  declareForeign Tracked 1 IO_UDP_UDPSocket_toText_impl_v1

  declareForeign Tracked 2 IO_UDP_serverSocket_impl_v1

  declareForeign Tracked 1 IO_UDP_ListenSocket_toText_impl_v1

  declareForeign Tracked 1 IO_UDP_ListenSocket_recvFrom_impl_v1

  declareForeign Tracked 1 IO_UDP_ClientSockAddr_toText_v1

  declareForeign Tracked 3 IO_UDP_ListenSocket_sendTo_impl_v1

declareForeigns :: FDecl Symbol ()
declareForeigns = do
  declareUdpForeigns
  declareForeign Tracked 2 IO_openFile_impl_v3

  declareForeign Tracked 1 IO_closeFile_impl_v3
  declareForeign Tracked 1 IO_isFileEOF_impl_v3
  declareForeign Tracked 1 IO_isFileOpen_impl_v3
  declareForeign Tracked 1 IO_getEcho_impl_v1
  declareForeign Tracked 1 IO_ready_impl_v1
  declareForeign Tracked 1 IO_getChar_impl_v1
  declareForeign Tracked 1 IO_isSeekable_impl_v3

  declareForeign Tracked 3 IO_seekHandle_impl_v3

  declareForeign Tracked 1 IO_handlePosition_impl_v3

  declareForeign Tracked 1 IO_getBuffering_impl_v3

  declareForeign Tracked 2 IO_setBuffering_impl_v3

  declareForeign Tracked 2 IO_setEcho_impl_v1

  declareForeign Tracked 1 IO_getLine_impl_v1

  declareForeign Tracked 2 IO_getBytes_impl_v3
  declareForeign Tracked 2 IO_getSomeBytes_impl_v1
  declareForeign Tracked 2 IO_putBytes_impl_v3
  declareForeign Tracked 0 IO_systemTime_impl_v3

  declareForeign Tracked 0 IO_systemTimeMicroseconds_v1

  declareForeign Tracked 0 Clock_internals_monotonic_v1

  declareForeign Tracked 0 Clock_internals_realtime_v1

  declareForeign Tracked 0 Clock_internals_processCPUTime_v1

  declareForeign Tracked 0 Clock_internals_threadCPUTime_v1

  declareForeign Tracked 1 Clock_internals_sec_v1

  -- A TimeSpec that comes from getTime never has negative nanos,
  -- so we can safely cast to Nat
  declareForeign Tracked 1 Clock_internals_nsec_v1

  declareForeign Tracked 1 Clock_internals_systemTimeZone_v1

  declareForeign Tracked 0 IO_getTempDirectory_impl_v3

  declareForeign Tracked 1 IO_createTempDirectory_impl_v3

  declareForeign Tracked 0 IO_getCurrentDirectory_impl_v3

  declareForeign Tracked 1 IO_setCurrentDirectory_impl_v3

  declareForeign Tracked 1 IO_fileExists_impl_v3

  declareForeign Tracked 1 IO_getEnv_impl_v1

  declareForeign Tracked 0 IO_getArgs_impl_v1

  declareForeign Tracked 1 IO_isDirectory_impl_v3

  declareForeign Tracked 1 IO_createDirectory_impl_v3

  declareForeign Tracked 1 IO_removeDirectory_impl_v3

  declareForeign Tracked 2 IO_renameDirectory_impl_v3

  declareForeign Tracked 1 IO_directoryContents_impl_v3

  declareForeign Tracked 1 IO_removeFile_impl_v3

  declareForeign Tracked 2 IO_renameFile_impl_v3

  declareForeign Tracked 1 IO_getFileTimestamp_impl_v3

  declareForeign Tracked 1 IO_getFileSize_impl_v3

  declareForeign Tracked 2 IO_serverSocket_impl_v3

  declareForeign Tracked 1 Socket_toText

  declareForeign Tracked 1 Handle_toText

  declareForeign Tracked 1 ThreadId_toText

  declareForeign Tracked 1 IO_socketPort_impl_v3

  declareForeign Tracked 1 IO_listen_impl_v3

  declareForeign Tracked 2 IO_clientSocket_impl_v3

  declareForeign Tracked 1 IO_closeSocket_impl_v3

  declareForeign Tracked 1 IO_socketAccept_impl_v3

  declareForeign Tracked 2 IO_socketSend_impl_v3

  declareForeign Tracked 2 IO_socketReceive_impl_v3

  declareForeign Tracked 1 IO_kill_impl_v3

  declareForeign Tracked 1 IO_delay_impl_v3

  declareForeign Tracked 1 IO_stdHandle

  declareForeign Tracked 2 IO_process_call

  declareForeign Tracked 2 IO_process_start

  declareForeign Tracked 1 IO_process_kill

  declareForeign Tracked 1 IO_process_wait

  declareForeign Tracked 1 IO_process_exitCode
  declareForeign Tracked 1 MVar_new

  declareForeign Tracked 0 MVar_newEmpty_v2

  declareForeign Tracked 1 MVar_take_impl_v3

  declareForeign Tracked 1 MVar_tryTake

  declareForeign Tracked 2 MVar_put_impl_v3

  declareForeign Tracked 2 MVar_tryPut_impl_v3

  declareForeign Tracked 2 MVar_swap_impl_v3

  declareForeign Tracked 1 MVar_isEmpty

  declareForeign Tracked 1 MVar_read_impl_v3

  declareForeign Tracked 1 MVar_tryRead_impl_v3

  declareForeign Untracked 1 Char_toText
  declareForeign Untracked 2 Text_repeat
  declareForeign Untracked 1 Text_reverse
  declareForeign Untracked 1 Text_toUppercase
  declareForeign Untracked 1 Text_toLowercase
  declareForeign Untracked 1 Text_toUtf8
  declareForeign Untracked 1 Text_fromUtf8_impl_v3
  declareForeign Tracked 2 Tls_ClientConfig_default
  declareForeign Tracked 2 Tls_ServerConfig_default
  declareForeign Tracked 2 Tls_ClientConfig_certificates_set

  declareForeign Tracked 2 Tls_ServerConfig_certificates_set

  declareForeign Tracked 1 TVar_new

  declareForeign Tracked 1 TVar_read
  declareForeign Tracked 2 TVar_write
  declareForeign Tracked 1 TVar_newIO

  declareForeign Tracked 1 TVar_readIO
  declareForeign Tracked 2 TVar_swap
  declareForeign Tracked 0 STM_retry
  declareForeign Tracked 0 Promise_new
  -- the only exceptions from Promise.read are async and shouldn't be caught
  declareForeign Tracked 1 Promise_read
  declareForeign Tracked 1 Promise_tryRead

  declareForeign Tracked 2 Promise_write
  declareForeign Tracked 2 Tls_newClient_impl_v3
  declareForeign Tracked 2 Tls_newServer_impl_v3
  declareForeign Tracked 1 Tls_handshake_impl_v3
  declareForeign Tracked 2 Tls_send_impl_v3
  declareForeign Tracked 1 Tls_decodeCert_impl_v3

  declareForeign Tracked 1 Tls_encodeCert

  declareForeign Tracked 1 Tls_decodePrivateKey
  declareForeign Tracked 1 Tls_encodePrivateKey

  declareForeign Tracked 1 Tls_receive_impl_v3

  declareForeign Tracked 1 Tls_terminate_impl_v3
  declareForeign Untracked 1 Code_validateLinks
  declareForeign Untracked 1 Code_dependencies
  declareForeign Untracked 1 Code_serialize
  declareForeign Untracked 1 Code_deserialize
  declareForeign Untracked 2 Code_display
  declareForeign Untracked 1 Value_dependencies
  declareForeign Untracked 1 Value_serialize
  declareForeign Untracked 1 Value_deserialize
  -- Hashing functions
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Sha3_512
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Sha3_256
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Sha2_512
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Sha2_256
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Sha1
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Blake2b_512
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Blake2b_256
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Blake2s_256
  declareForeignWrap Untracked direct Crypto_HashAlgorithm_Md5

  declareForeign Untracked 2 Crypto_hashBytes
  declareForeign Untracked 3 Crypto_hmacBytes

  declareForeignWrap Untracked crypto'hash Crypto_hash
  declareForeignWrap Untracked crypto'hmac Crypto_hmac
  declareForeign Untracked 3 Crypto_Ed25519_sign_impl

  declareForeign Untracked 3 Crypto_Ed25519_verify_impl

  declareForeign Untracked 2 Crypto_Rsa_sign_impl

  declareForeign Untracked 3 Crypto_Rsa_verify_impl

  declareForeignWrap Untracked murmur'hash Universal_murmurHash
  declareForeign Tracked 1 IO_randomBytes
  declareForeign Untracked 1 Bytes_zlib_compress
  declareForeign Untracked 1 Bytes_gzip_compress
  declareForeign Untracked 1 Bytes_zlib_decompress
  declareForeign Untracked 1 Bytes_gzip_decompress

  declareForeign Untracked 1 Bytes_toBase16
  declareForeign Untracked 1 Bytes_toBase32
  declareForeign Untracked 1 Bytes_toBase64
  declareForeign Untracked 1 Bytes_toBase64UrlUnpadded

  declareForeign Untracked 1 Bytes_fromBase16
  declareForeign Untracked 1 Bytes_fromBase32
  declareForeign Untracked 1 Bytes_fromBase64
  declareForeign Untracked 1 Bytes_fromBase64UrlUnpadded

  declareForeign Untracked 1 Bytes_decodeNat64be
  declareForeign Untracked 1 Bytes_decodeNat64le
  declareForeign Untracked 1 Bytes_decodeNat32be
  declareForeign Untracked 1 Bytes_decodeNat32le
  declareForeign Untracked 1 Bytes_decodeNat16be
  declareForeign Untracked 1 Bytes_decodeNat16le

  declareForeign Untracked 1 Bytes_encodeNat64be
  declareForeign Untracked 1 Bytes_encodeNat64le
  declareForeign Untracked 1 Bytes_encodeNat32be
  declareForeign Untracked 1 Bytes_encodeNat32le
  declareForeign Untracked 1 Bytes_encodeNat16be
  declareForeign Untracked 1 Bytes_encodeNat16le

  declareForeign Untracked 5 MutableArray_copyTo_force

  declareForeign Untracked 5 MutableByteArray_copyTo_force

  declareForeign Untracked 5 ImmutableArray_copyTo_force

  declareForeign Untracked 1 ImmutableArray_size
  declareForeign Untracked 1 MutableArray_size
  declareForeign Untracked 1 ImmutableByteArray_size
  declareForeign Untracked 1 MutableByteArray_size

  declareForeign Untracked 5 ImmutableByteArray_copyTo_force

  declareForeign Untracked 2 MutableArray_read
  declareForeign Untracked 2 MutableByteArray_read8
  declareForeign Untracked 2 MutableByteArray_read16be
  declareForeign Untracked 2 MutableByteArray_read24be
  declareForeign Untracked 2 MutableByteArray_read32be
  declareForeign Untracked 2 MutableByteArray_read40be
  declareForeign Untracked 2 MutableByteArray_read64be

  declareForeign Untracked 3 MutableArray_write
  declareForeign Untracked 3 MutableByteArray_write8
  declareForeign Untracked 3 MutableByteArray_write16be
  declareForeign Untracked 3 MutableByteArray_write32be
  declareForeign Untracked 3 MutableByteArray_write64be

  declareForeign Untracked 2 ImmutableArray_read
  declareForeign Untracked 2 ImmutableByteArray_read8
  declareForeign Untracked 2 ImmutableByteArray_read16be
  declareForeign Untracked 2 ImmutableByteArray_read24be
  declareForeign Untracked 2 ImmutableByteArray_read32be
  declareForeign Untracked 2 ImmutableByteArray_read40be
  declareForeign Untracked 2 ImmutableByteArray_read64be

  declareForeign Untracked 1 MutableByteArray_freeze_force
  declareForeign Untracked 1 MutableArray_freeze_force

  declareForeign Untracked 3 MutableByteArray_freeze
  declareForeign Untracked 3 MutableArray_freeze

  declareForeign Untracked 1 MutableByteArray_length

  declareForeign Untracked 1 ImmutableByteArray_length

  declareForeign Tracked 1 IO_array
  declareForeign Tracked 2 IO_arrayOf
  declareForeign Tracked 1 IO_bytearray
  declareForeign Tracked 2 IO_bytearrayOf

  declareForeign Untracked 1 Scope_array
  declareForeign Untracked 2 Scope_arrayOf
  declareForeign Untracked 1 Scope_bytearray
  declareForeign Untracked 2 Scope_bytearrayOf

  declareForeign Untracked 1 Text_patterns_literal
  declareForeignWrap Untracked direct Text_patterns_digit
  declareForeignWrap Untracked direct Text_patterns_letter
  declareForeignWrap Untracked direct Text_patterns_space
  declareForeignWrap Untracked direct Text_patterns_punctuation
  declareForeignWrap Untracked direct Text_patterns_anyChar
  declareForeignWrap Untracked direct Text_patterns_eof
  declareForeign Untracked 2 Text_patterns_charRange
  declareForeign Untracked 2 Text_patterns_notCharRange
  declareForeign Untracked 1 Text_patterns_charIn
  declareForeign Untracked 1 Text_patterns_notCharIn
  declareForeign Untracked 1 Pattern_many
  declareForeign Untracked 1 Pattern_many_corrected
  declareForeign Untracked 1 Pattern_capture
  declareForeign Untracked 2 Pattern_captureAs
  declareForeign Untracked 1 Pattern_join
  declareForeign Untracked 2 Pattern_or
  declareForeign Untracked 3 Pattern_replicate

  declareForeign Untracked 2 Pattern_run

  declareForeign Untracked 2 Pattern_isMatch

  declareForeignWrap Untracked direct Char_Class_any
  declareForeign Untracked 1 Char_Class_not
  declareForeign Untracked 2 Char_Class_and
  declareForeign Untracked 2 Char_Class_or
  declareForeign Untracked 2 Char_Class_range
  declareForeign Untracked 1 Char_Class_anyOf
  declareForeignWrap Untracked direct Char_Class_alphanumeric
  declareForeignWrap Untracked direct Char_Class_upper
  declareForeignWrap Untracked direct Char_Class_lower
  declareForeignWrap Untracked direct Char_Class_whitespace
  declareForeignWrap Untracked direct Char_Class_control
  declareForeignWrap Untracked direct Char_Class_printable
  declareForeignWrap Untracked direct Char_Class_mark
  declareForeignWrap Untracked direct Char_Class_number
  declareForeignWrap Untracked direct Char_Class_punctuation
  declareForeignWrap Untracked direct Char_Class_symbol
  declareForeignWrap Untracked direct Char_Class_separator
  declareForeignWrap Untracked direct Char_Class_letter
  declareForeign Untracked 2 Char_Class_is
  declareForeign Untracked 1 Text_patterns_char

foreignDeclResults :: (Map ForeignFunc (Sandbox, SuperNormal Symbol))
foreignDeclResults =
  execState declareForeigns mempty

foreignWrappers :: [(Data.Text.Text, (Sandbox, SuperNormal Symbol))]
foreignWrappers =
  Map.toList foreignDeclResults
    <&> \(ff, (sand, code)) -> (foreignFuncBuiltinName ff, (sand, code))

numberedTermLookup :: EnumMap Word64 (SuperNormal Symbol)
numberedTermLookup =
  mapFromList . zip [1 ..] . Map.elems . fmap snd $ builtinLookup

builtinTermNumbering :: Map Reference Word64
builtinTermNumbering =
  Map.fromList (zip (Map.keys $ builtinLookup) [1 ..])

builtinTermBackref :: EnumMap Word64 Reference
builtinTermBackref =
  mapFromList . zip [1 ..] . Map.keys $ builtinLookup

builtinForeignNames :: Map ForeignFunc Data.Text.Text
builtinForeignNames =
  foreignDeclResults
    & Map.keys
    & map (\f -> (f, foreignFuncBuiltinName f))
    & Map.fromList

-- Bootstrapping for sandbox check. The eventual map will be one with
-- associations `r -> s` where `s` is all the 'sensitive' base
-- functions that `r` calls.
baseSandboxInfo :: Map Reference (Set Reference)
baseSandboxInfo =
  Map.fromList $
    [ (r, Set.singleton r)
      | (r, (sb, _)) <- Map.toList builtinLookup,
        sb == Tracked
    ]

builtinArities :: Map Reference Int
builtinArities =
  Map.fromList $
    [(r, arity s) | (r, (_, s)) <- Map.toList builtinLookup]

builtinInlineInfo :: Map Reference (Int, ANormal Symbol)
builtinInlineInfo =
  ANF.buildInlineMap $ fmap (Rec [] . snd) builtinLookup

sandboxedForeignFuncs :: Set ForeignFunc
sandboxedForeignFuncs =
  Map.keysSet $
    Map.filter (\(sb, _) -> sb == Tracked) foreignDeclResults
