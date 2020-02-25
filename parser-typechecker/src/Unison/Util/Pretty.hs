{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.Util.Pretty (
   Pretty,
   ColorText,
   align,
   alternations,
   backticked,
   boxForkLeft,
   boxLeft,
   boxLeftM,
   boxRight,
   boxRightM,
   bulleted,
   bracket,
   -- breakable
   callout,
   excerptSep,
   excerptSep',
   excerptColumn2,
   excerptColumn2Headed,
   warnCallout, blockedCallout, fatalCallout, okCallout,
   column2,
   column2M,
   column2UnzippedM,
   column3,
   column3M,
   column3UnzippedM,
   column3sep,
   commas,
   commented,
   oxfordCommas,
   oxfordCommasWith,
   plural,
   dashed,
   flatMap,
   group,
   hang',
   hang,
   hangUngrouped',
   hangUngrouped,
   indent,
   indentAfterNewline,
   indentN,
   indentNonEmptyN,
   indentNAfterNewline,
   leftPad,
   lines,
   linesNonEmpty,
   linesSpaced,
   lit,
   map,
   mayColumn2,
   nest,
   num,
   newline,
   lineSkip,
   nonEmpty,
   numbered,
   numberedColumn2,
   numberedList,
   orElse,
   orElses,
   paragraphyText,
   parenthesize,
   parenthesizeCommas,
   parenthesizeIf,
   preferredWidth,
   preferredHeight,
   render,
   renderUnbroken,
   rightPad,
   sep,
   sepNonEmpty,
   sepSpaced,
   shown,
   softbreak,
   spaceIfBreak,
   spaced,
   spacedMap,
   spacesIfBreak,
   string,
   endSentence,
   surroundCommas,
   syntaxToColor,
   text,
   toANSI,
   toAnsiUnbroken,
   toHTML,
   toPlain,
   toPlainUnbroken,
   underline,
   withSyntax,
   wrap,
   wrapColumn2,
   wrapString,
   black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold,
   border,
   Width
  ) where

import Unison.Prelude

import           Data.Bifunctor                 ( second )
import           Data.Char                      ( isSpace )
import           Data.List                      ( foldr1, intersperse )
import           Prelude                 hiding ( lines , map )
import           Unison.Util.AnnotatedText      ( annotateMaybe )
import qualified Unison.Util.ColorText         as CT
import qualified Unison.Util.SyntaxText        as ST
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Data.ListLike                 as LL
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text
import Control.Monad.Identity (runIdentity, Identity(..))

type Width = Int
type ColorText = CT.ColorText

data Pretty s = Pretty { delta :: Delta, out :: F s (Pretty s) } deriving Eq

instance Functor Pretty where
  fmap f (Pretty d o) = Pretty d (mapLit f $ fmap (fmap f) o)

data F s r
  = Empty | Group r | Lit s | Wrap (Seq r) | OrElse r r | Append (Seq r)
  deriving (Eq, Show, Foldable, Traversable, Functor)

mapLit :: (s -> t) -> F s r -> F t r
mapLit f (Lit s) = Lit (f s)
mapLit _ Empty = Empty
mapLit _ (Group r) = Group r
mapLit _ (Wrap s) = Wrap s
mapLit _ (OrElse r s) = OrElse r s
mapLit _ (Append s) = Append s

lit :: (IsString s, LL.ListLike s Char) => s -> Pretty s
lit s = lit' (foldMap chDelta $ LL.toList s) s

lit' :: Delta -> s -> Pretty s
lit' d s = Pretty d (Lit s)

orElse :: Pretty s -> Pretty s -> Pretty s
orElse p1 p2 = Pretty (delta p1) (OrElse p1 p2)

orElses :: [Pretty s] -> Pretty s
orElses [] = mempty
orElses ps = foldr1 orElse ps

wrapImpl :: IsString s => [Pretty s] -> Pretty s
wrapImpl [] = mempty
wrapImpl (p:ps) = wrap_ . Seq.fromList $
  p : fmap (\p -> (" " <> p) `orElse` (newline <> p)) ps

wrapImplPreserveSpaces :: (LL.ListLike s Char, IsString s) => [Pretty s] -> Pretty s
wrapImplPreserveSpaces = \case
  [] -> mempty
  (p:ps) -> wrap_ . Seq.fromList $ p : fmap f ps
  where
  startsWithSpace p = case out p of
    (Lit s) -> fromMaybe False (fmap (isSpaceNotNewline . fst) $ LL.uncons s)
    _ -> False
  f p | startsWithSpace p = p `orElse` newline
  f p = p

isSpaceNotNewline :: Char -> Bool
isSpaceNotNewline c = isSpace c && not (c == '\n')

wrapString :: (LL.ListLike s Char, IsString s) => String -> Pretty s
wrapString s = wrap (lit $ fromString s)

-- Wrap text, preserving whitespace (apart from at the wrap points.)
-- Used in particular for viewing/displaying doc literals.
-- Should be understood in tandem with TermParser.docNormalize.
-- See also unison-src/transcripts/doc-formatting.md.
paragraphyText :: (LL.ListLike s Char, IsString s) => Text -> Pretty s
paragraphyText = sep "\n" . fmap (wrapPreserveSpaces . text) . Text.splitOn "\n"

wrap :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
wrap p = wrapImpl (toLeaves [p]) where
  toLeaves [] = []
  toLeaves (hd:tl) = case out hd of
    Empty -> toLeaves tl
    Lit s -> wordify s ++ toLeaves tl
    Group _ -> hd : toLeaves tl
    OrElse a _ -> toLeaves (a:tl)
    Wrap _ -> hd : toLeaves tl
    Append hds -> toLeaves (toList hds ++ tl)
  wordify s0 = let s = LL.dropWhile isSpace s0 in
    if LL.null s then []
    else case LL.break isSpace s of (word1, s) -> lit word1 : wordify s

-- Does not insert spaces where none were present, and does not collapse
-- sequences of spaces into one.
-- It'd be a bit painful to just replace wrap with the following version, because
-- lots of OutputMessages code depends on wrap's behaviour of sometimes adding
-- extra spaces.
wrapPreserveSpaces :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
wrapPreserveSpaces p = wrapImplPreserveSpaces (toLeaves [p]) where
  toLeaves [] = []
  toLeaves (hd:tl) = case out hd of
    Empty -> toLeaves tl
    Lit s -> (fmap lit $ alternations isSpaceNotNewline s) ++ toLeaves tl
    Group _ -> hd : toLeaves tl
    OrElse a _ -> toLeaves (a:tl)
    Wrap _ -> hd : toLeaves tl
    Append hds -> toLeaves (toList hds ++ tl)

-- Cut a list every time a predicate changes.  Produces a list of
-- non-empty lists.
alternations :: (LL.ListLike s c) => (c -> Bool) -> s -> [s]
alternations p s = reverse $ go True s [] where
  go _ s acc | LL.null s = acc
  go w s acc = go (not w) rest acc' where
    (t, rest) = LL.span p' s
    p' = if w then p else (\x -> not (p x))
    acc' = if (LL.null t) then acc else t : acc

wrap_ :: Seq (Pretty s) -> Pretty s
wrap_ ps = Pretty (foldMap delta ps) (Wrap ps)

group :: Pretty s -> Pretty s
group p = Pretty (delta p) (Group p)

toANSI :: Width -> Pretty CT.ColorText -> String
toANSI avail p = CT.toANSI (render avail p)

toAnsiUnbroken :: Pretty ColorText -> String
toAnsiUnbroken p = CT.toANSI (renderUnbroken p)

toPlain :: Width -> Pretty CT.ColorText -> String
toPlain avail p = CT.toPlain (render avail p)

toHTML :: String -> Width -> Pretty CT.ColorText -> String
toHTML cssPrefix avail p = CT.toHTML cssPrefix (render avail p)

toPlainUnbroken :: Pretty ColorText -> String
toPlainUnbroken p = CT.toPlain (renderUnbroken p)

syntaxToColor :: Pretty ST.SyntaxText -> Pretty ColorText
syntaxToColor = fmap $ annotateMaybe . fmap CT.defaultColors

withSyntax :: ST.Element -> Pretty ST.SyntaxText -> Pretty ST.SyntaxText
withSyntax e = fmap $ ST.syntax e

renderUnbroken :: (Monoid s, IsString s) => Pretty s -> s
renderUnbroken = render maxBound

render :: (Monoid s, IsString s) => Width -> Pretty s -> s
render availableWidth p = go mempty [Right p] where
  go _   []       = mempty
  go cur (p:rest) = case p of
    Right p -> -- `p` might fit, let's try it!
      if p `fits` cur then flow p <> go (cur <> delta p) rest
      else go cur (Left p : rest) -- nope, switch to breaking mode
    Left p -> case out p of -- `p` requires breaking
      Append ps  -> go cur ((Left <$> toList ps) <> rest)
      Empty      -> go cur rest
      Group p    -> go cur (Right p : rest)
      -- Note: literals can't be broken further so they're
      -- added to output unconditionally
      Lit l      -> l <> go (cur <> delta p) rest
      OrElse _ p -> go cur (Right p : rest)
      Wrap ps    -> go cur ((Right <$> toList ps) <> rest)

  flow p = case out p of
    Append ps -> foldMap flow ps
    Empty -> mempty
    Group p -> flow p
    Lit s -> s
    OrElse p _ -> flow p
    Wrap ps -> foldMap flow ps

  fits p cur =
    let cur' = cur { maxCol = col cur }
    in maxCol (cur' <> delta p) < availableWidth

newline :: IsString s => Pretty s
newline = "\n"

lineSkip :: IsString s => Pretty s
lineSkip = newline <> newline

spaceIfBreak :: IsString s => Pretty s
spaceIfBreak = "" `orElse` " "

spacesIfBreak :: IsString s => Int -> Pretty s
spacesIfBreak n = "" `orElse` fromString (replicate n ' ')

softbreak :: IsString s => Pretty s
softbreak = " " `orElse` newline

spaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
spaced = intercalateMap softbreak id

spacedMap :: (Foldable f, IsString s) => (a -> Pretty s) -> f a -> Pretty s
spacedMap f as = spaced . fmap f $ toList as

commas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
commas = intercalateMap ("," <> softbreak) id

oxfordCommas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
oxfordCommas = oxfordCommasWith ""

-- Like `oxfordCommas`, but attaches `end` at the end (without a space).
-- For example, `oxfordCommasWith "."` will attach a period.
oxfordCommasWith
  :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
oxfordCommasWith end xs = case toList xs of
  []     -> ""
  [x]    -> group (x <> end)
  [x, y] -> x <> " and " <> group (y <> end)
  xs ->
    intercalateMap ("," <> softbreak) id (init xs)
      <> ","
      <> softbreak
      <> "and"
      <> softbreak
      <> group (last xs <> end)

parenthesizeCommas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
parenthesizeCommas = surroundCommas "(" ")"

surroundCommas
  :: (Foldable f, IsString s)
  => Pretty s
  -> Pretty s
  -> f (Pretty s)
  -> Pretty s
surroundCommas start stop fs =
  group
    $  start
    <> spaceIfBreak
    <> intercalateMap ("," <> softbreak <> align) id fs
    <> stop
  where align = spacesIfBreak (preferredWidth start + 1)

sepSpaced :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sepSpaced between = sep (between <> softbreak)

sep :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sep between = intercalateMap between id

sepNonEmpty :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sepNonEmpty between ps = sep between (nonEmpty ps)

-- if list is too long, adds `... 22 more` to the end
excerptSep :: IsString s => Maybe Int -> Pretty s -> [Pretty s] -> Pretty s
excerptSep maxCount =
  excerptSep' maxCount (\i -> group ("... " <> shown i <> " more"))

excerptSep'
  :: IsString s
  => Maybe Int
  -> (Int -> Pretty s)
  -> Pretty s
  -> [Pretty s]
  -> Pretty s
excerptSep' maxCount summarize s ps = case maxCount of
  Just max | length ps > max ->
    sep s (take max ps) <> summarize (length ps - max)
  _ -> sep s ps

nonEmpty :: (Foldable f, IsString s) => f (Pretty s) -> [Pretty s]
nonEmpty (toList -> l) = case l of
  (out -> Empty) : t -> nonEmpty t
  h : t -> h : nonEmpty t
  [] -> []

parenthesize :: IsString s => Pretty s -> Pretty s
parenthesize p = group $ "(" <> p <> ")"

parenthesizeIf :: IsString s => Bool -> Pretty s -> Pretty s
parenthesizeIf False s = s
parenthesizeIf True s = parenthesize s

lines :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
lines = intercalateMap (append newline) id where
  append p = Pretty (delta p) (Append $ Seq.singleton p)

linesNonEmpty :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
linesNonEmpty = lines . nonEmpty

linesSpaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
linesSpaced ps = lines (intersperse "" $ toList ps)

prefixed :: (Foldable f, LL.ListLike s Char, IsString s)
         => Pretty s -> Pretty s -> f (Pretty s) -> Pretty s
prefixed first rest =
  intercalateMap newline (\b -> first <> indentAfterNewline rest b)

bulleted
  :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
bulleted = prefixed "* " "  "

dashed
  :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
dashed = prefixed "- " "  "

commented
  :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
commented = prefixed "-- " "-- "

numbered
  :: (Foldable f, LL.ListLike s Char, IsString s)
  => (Int -> Pretty s)
  -> f (Pretty s)
  -> Pretty s
numbered num ps = column2 (fmap num [1 ..] `zip` toList ps)

-- Like `column2` but with the lines numbered. For instance:
--
-- 1. one thing     : this is a thing
-- 2. another thing : this is another thing
-- 3. and another   : yet one more thing
numberedColumn2
  :: (Foldable f, LL.ListLike s Char, IsString s)
  => (Int -> Pretty s)
  -> f (Pretty s, Pretty s)
  -> Pretty s
numberedColumn2 num ps = numbered num (align $ toList ps)

-- Opinionated `numbered` that uses bold numbers in front
numberedList :: Foldable f => f (Pretty ColorText) -> Pretty ColorText
numberedList = numbered (\i -> hiBlack . fromString $ show i <> ".")

leftPad, rightPad :: IsString s => Int -> Pretty s -> Pretty s
leftPad n p =
  let rem = n - preferredWidth p
  in  if rem > 0 then fromString (replicate rem ' ') <> p else p
rightPad n p =
  let rem = n - preferredWidth p
  in  if rem > 0 then p <> fromString (replicate rem ' ') else p

excerptColumn2Headed
  :: (LL.ListLike s Char, IsString s)
  => Maybe Int
  -> (Pretty s, Pretty s)
  -> [(Pretty s, Pretty s)]
  -> Pretty s
excerptColumn2Headed max hd cols = case max of
  Just max | len > max ->
    lines [column2 (hd : take max cols), "... " <> shown (len - max) <> " more"]
  _ -> column2 (hd : cols)
  where len = length cols

excerptColumn2
  :: (LL.ListLike s Char, IsString s)
  => Maybe Int
  -> [(Pretty s, Pretty s)]
  -> Pretty s
excerptColumn2 max cols = case max of
  Just max | len > max -> lines [column2 cols, "... " <> shown (len - max)]
  _                    -> column2 cols
  where len = length cols

column2
  :: (LL.ListLike s Char, IsString s) => [(Pretty s, Pretty s)] -> Pretty s
column2 = lines . (group <$>) . align

column2M
  :: (Applicative m, LL.ListLike s Char, IsString s)
  => [m (Pretty s, Pretty s)]
  -> m (Pretty s)
column2M = fmap column2 . sequenceA

mayColumn2
  :: (LL.ListLike s Char, IsString s)
  => [(Pretty s, Maybe (Pretty s))]
  -> Pretty s
mayColumn2 = lines . (group <$>) . align'

column3
  :: (LL.ListLike s Char, IsString s)
  => [(Pretty s, Pretty s, Pretty s)]
  -> Pretty s
column3 = column3sep ""

column3M
  :: (LL.ListLike s Char, IsString s, Monad m)
  => [m (Pretty s, Pretty s, Pretty s)]
  -> m (Pretty s)
column3M = fmap column3 . sequence

column3UnzippedM
  :: forall m s . (LL.ListLike s Char, IsString s, Monad m)
  => Pretty s
  -> [m (Pretty s)]
  -> [m (Pretty s)]
  -> [m (Pretty s)]
  -> m (Pretty s)
column3UnzippedM bottomPadding left mid right = let
  rowCount = maximum (fmap length [left, mid, right])
  pad :: [m (Pretty s)] -> [m (Pretty s)]
  pad a = a ++ replicate (rowCount - length a) (pure bottomPadding)
  (pleft, pmid, pright) = (pad left, pad mid, pad right)
  in column3M $ zipWith3 (liftA3 (,,)) pleft pmid pright

column2UnzippedM
  :: forall m s . (LL.ListLike s Char, IsString s, Monad m)
  => Pretty s
  -> [m (Pretty s)]
  -> [m (Pretty s)]
  -> m (Pretty s)
column2UnzippedM bottomPadding left right = let
  rowCount = length left `max` length right
  pad :: [m (Pretty s)] -> [m (Pretty s)]
  pad a = a ++ replicate (rowCount - length a) (pure bottomPadding)
  sep :: [m (Pretty s)] -> [m (Pretty s)]
  sep = fmap (fmap (" " <>))
  (pleft, pright) = (pad left, sep $ pad right)
  in column2M $ zipWith (liftA2 (,)) pleft pright

column3sep
  :: (LL.ListLike s Char, IsString s) => Pretty s -> [(Pretty s, Pretty s, Pretty s)] -> Pretty s
column3sep sep rows = let
  bc = align [(b,sep <> c) | (_,b,c) <- rows ]
  abc = group <$> align [(a,sep <> bc) | ((a,_,_),bc) <- rows `zip` bc ]
  in lines abc

wrapColumn2 ::
  (LL.ListLike s Char, IsString s) => [(Pretty s, Pretty s)] -> Pretty s
wrapColumn2 rows = lines (align rows) where
  align rows = let lwidth = foldl' max 0 (preferredWidth . fst <$> rows) + 2
    in [ group (rightPad lwidth l <> indentNAfterNewline lwidth (wrap r))
       | (l, r) <- rows]

align
  :: (LL.ListLike s Char, IsString s) => [(Pretty s, Pretty s)] -> [Pretty s]
align rows = align' (second Just <$> rows)

-- [("foo", Just "bar")
-- ,("barabaz", Nothing)
-- ,("qux","quux")]
--
-- results in:
--
-- foo bar
-- barabaz
-- qux quux
align'
  :: (LL.ListLike s Char, IsString s)
  => [(Pretty s, Maybe (Pretty s))]
  -> [Pretty s]
align' rows = alignedRows
 where
  col0Width = foldl' max 0 [ preferredWidth col1 | (col1, Just _) <- rows ] + 1
  alignedRows =
    [ case col1 of
        Just s  -> rightPad col0Width col0 <> indentNAfterNewline col0Width s
        Nothing -> col0
    | (col0, col1) <- rows
    ]

text :: IsString s => Text -> Pretty s
text t = fromString (Text.unpack t)

num :: (Show n, Num n, IsString s) => n -> Pretty s
num n = fromString (show n)

string :: IsString s => String -> Pretty s
string = fromString

endSentence :: IsString s => Pretty s -> Pretty s
endSentence p = group (p <> ".")

shown :: (Show a, IsString s) => a -> Pretty s
shown = fromString . show

hang'
  :: (LL.ListLike s Char, IsString s)
  => Pretty s
  -> Pretty s
  -> Pretty s
  -> Pretty s
hang' from by p = group $ if preferredHeight p > 0
  then from <> "\n" <> group (indent by p)
  else (from <> " " <> group p) `orElse` (from <> "\n" <> group (indent by p))

hangUngrouped'
  :: (LL.ListLike s Char, IsString s)
  => Pretty s
  -> Pretty s
  -> Pretty s
  -> Pretty s
hangUngrouped' from by p = if preferredHeight p > 0
  then from <> "\n" <> indent by p
  else (from <> " " <> p) `orElse` (from <> "\n" <> indent by p)

hangUngrouped
  :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
hangUngrouped from = hangUngrouped' from "  "

hang :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
hang from = hang' from "  "

nest :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
nest = hang' ""

indent :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
indent by p = by <> indentAfterNewline by p

indentN :: (LL.ListLike s Char, IsString s) => Width -> Pretty s -> Pretty s
indentN by = indent (fromString $ replicate by ' ')

indentNonEmptyN :: (LL.ListLike s Char, IsString s) => Width -> Pretty s -> Pretty s
indentNonEmptyN _ (out -> Empty) = mempty
indentNonEmptyN by p = indentN by p

indentNAfterNewline
  :: (LL.ListLike s Char, IsString s) => Width -> Pretty s -> Pretty s
indentNAfterNewline by = indentAfterNewline (fromString $ replicate by ' ')

indentAfterNewline
  :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
indentAfterNewline by = flatMap f
 where
  f s0 = case LL.break (== '\n') s0 of
    (hd, s) -> if LL.null s
      then lit s0
      -- use `take` and `drop` to preserve annotations or
      -- or other extra info attached to the original `s`
      else lit (LL.take (LL.length hd) s0) <> "\n" <> by <> f (LL.drop 1 s)

instance IsString s => IsString (Pretty s) where
  fromString s = lit' (foldMap chDelta s) (fromString s)

instance Semigroup (Pretty s) where (<>) = mappend
instance Monoid (Pretty s) where
  mempty = Pretty mempty Empty
  mappend p1 p2 = Pretty (delta p1 <> delta p2) .
    Append $ case (out p1, out p2) of
      (Append ps1, Append ps2) -> ps1 <> ps2
      (Append ps1, _) -> ps1 <> pure p2
      (_, Append ps2) -> pure p1 <> ps2
      (_,_) -> pure p1 <> pure p2

data Delta =
  Delta { line :: !Int, col :: !Int, maxCol :: !Int }
  deriving (Eq,Ord,Show)

instance Semigroup Delta where (<>) = mappend
instance Monoid Delta where
  mempty = Delta 0 0 0
  mappend (Delta l c mc) (Delta 0 c2 mc2) =
    Delta l (c + c2) (mc `max` mc2 `max` (c + c2))
  mappend (Delta l _ mc) (Delta l2 c2 mc2) = Delta (l + l2) c2 (mc `max` mc2)

chDelta :: Char -> Delta
chDelta '\n' = Delta 1 0 0
chDelta _ = Delta 0 1 1

preferredWidth :: Pretty s -> Width
preferredWidth p = col (delta p)

preferredHeight :: Pretty s -> Width
preferredHeight p = line (delta p)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold, underline
  :: Pretty CT.ColorText -> Pretty CT.ColorText
black = map CT.black
red = map CT.red
green = map CT.green
yellow = map CT.yellow
blue = map CT.blue
purple = map CT.purple
cyan = map CT.cyan
white = map CT.white
hiBlack = map CT.hiBlack
hiRed = map CT.hiRed
hiGreen = map CT.hiGreen
hiYellow = map CT.hiYellow
hiBlue = map CT.hiBlue
hiPurple = map CT.hiPurple
hiCyan = map CT.hiCyan
hiWhite = map CT.hiWhite
bold = map CT.bold
underline = map CT.underline

plural :: Foldable f
       => f a -> Pretty ColorText -> Pretty ColorText
plural f p = case length f of
  0 -> mempty
  1 -> p
  -- todo: consider use of plural package
  _ -> p <> case reverse (toPlainUnbroken p) of
    's' : _ -> "es"
    _ -> "s"

border :: (LL.ListLike s Char, IsString s) => Int -> Pretty s -> Pretty s
border n p = "\n" <> indentN n p <> "\n"

callout :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
callout header p = header <> "\n\n" <> p

bracket :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
bracket = indent "  "

boxForkLeft, boxLeft, boxRight ::
  forall s . (LL.ListLike s Char, IsString s) => [Pretty s] -> [Pretty s]
boxForkLeft = boxLeft' lBoxStyle1
boxLeft = boxLeft' lBoxStyle2
boxRight = boxRight' rBoxStyle2

boxLeft', boxRight' :: (LL.ListLike s Char, IsString s)
         => BoxStyle s -> [Pretty s] -> [Pretty s]
boxLeft' style = fmap runIdentity . boxLeftM' style . fmap Identity
boxRight' style = fmap runIdentity . boxRightM' style . fmap Identity

type BoxStyle s =
  ( (Pretty s, Pretty s) -- first (start, continue)
  , (Pretty s, Pretty s) -- middle
  , (Pretty s, Pretty s) -- last
  , (Pretty s, Pretty s) -- singleton
  )
lBoxStyle1, lBoxStyle2, rBoxStyle2 :: IsString s => BoxStyle s
lBoxStyle1 = (("┌ ", "│ ") -- first
             ,("├ ", "│ ") -- middle
             ,("└ ", "  ") -- last
             ,(""  , "" )) -- singleton
lBoxStyle2 = (("┌ ","  ")
             ,("│ ","  ")
             ,("└ ","  ")
             ,(""  ,""  ))
rBoxStyle2 = ((" ┐", " │")
             ,(" │", " │")
             ,(" ┘", "  ")
             ,("  ", "  "))

boxLeftM, boxRightM :: forall m s . (Monad m, LL.ListLike s Char, IsString s)
         => [m (Pretty s)] -> [m (Pretty s)]
boxLeftM = boxLeftM' lBoxStyle2
boxRightM = boxRightM' rBoxStyle2

boxLeftM' :: forall m s . (Monad m, LL.ListLike s Char, IsString s)
          => BoxStyle s -> [m (Pretty s)] -> [m (Pretty s)]
boxLeftM' (first, middle, last, singleton) ps = go (Seq.fromList ps) where
  go Seq.Empty = []
  go (p Seq.:<| Seq.Empty) = [decorate singleton <$> p]
  go (a Seq.:<| (mid Seq.:|> b)) =
    [decorate first <$> a]
      ++ toList (fmap (decorate middle) <$> mid)
      ++ [decorate last <$> b]
  decorate (first, mid) p = first <> indentAfterNewline mid p

-- this implementation doesn't work for multi-line inputs,
-- because i dunno how to inspect multi-line inputs


boxRightM' :: forall m s. (Monad m, LL.ListLike s Char, IsString s)
           => BoxStyle s -> [m (Pretty s)] -> [m (Pretty s)]
boxRightM' (first, middle, last, singleton) ps = go (Seq.fromList ps) where
  go :: Seq.Seq (m (Pretty s)) -> [m (Pretty s)]
  go Seq.Empty = []
  go (p Seq.:<| Seq.Empty) = [decorate singleton <$> p]
  go (a Seq.:<| (mid Seq.:|> b)) =
    [decorate first <$> a]
      ++ toList (fmap (decorate middle) <$> mid)
      ++ [decorate last <$> b]
  decorate (first, _mid) p = p <> first

warnCallout, blockedCallout, fatalCallout, okCallout
  :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
warnCallout = callout "⚠️"
fatalCallout = callout "❗️"
okCallout = callout "✅"
blockedCallout = callout "🚫"

backticked :: IsString s => Pretty s -> Pretty s
backticked p = group ("`" <> p <> "`")

instance Show s => Show (Pretty s) where
  show p = render 80 (metaPretty p)

metaPretty :: Show s => Pretty s -> Pretty String
metaPretty = go (0::Int) where
  go prec p = case out p of
    Lit s -> parenthesizeIf (prec > 0) $ "Lit" `hang` lit (show s)
    Empty -> "Empty"
    Group g -> parenthesizeIf (prec > 0) $ "Group" `hang` go 1 g
    Wrap s -> parenthesizeIf (prec > 0) $ "Wrap" `hang`
      surroundCommas "[" "]" (go 1 <$> s)
    OrElse a b -> parenthesizeIf (prec > 0) $
      "OrElse" `hang` spaced [go 1 a, go 1 b]
    Append s -> surroundCommas "[" "]" (go 1 <$> s)

map :: LL.ListLike s2 Char => (s -> s2) -> Pretty s -> Pretty s2
map f p = case out p of
  Append ps -> foldMap (map f) ps
  Empty -> mempty
  Group p -> group (map f p)
  Lit s -> lit' (foldMap chDelta $ LL.toList s2) s2 where s2 = f s
  OrElse p1 p2 -> orElse (map f p1) (map f p2)
  Wrap p -> wrap_ (map f <$> p)

flatMap :: (s -> Pretty s2) -> Pretty s -> Pretty s2
flatMap f p = case out p of
  Append ps -> foldMap (flatMap f) ps
  Empty -> mempty
  Group p -> group (flatMap f p)
  Lit s -> f s
  OrElse p1 p2 -> orElse (flatMap f p1) (flatMap f p2)
  Wrap p -> wrap_ (flatMap f <$> p)
