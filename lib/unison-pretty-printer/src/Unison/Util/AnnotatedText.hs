{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Util.AnnotatedText where

import Data.Foldable qualified as Foldable
import Data.List qualified as L
import Data.ListLike qualified as LL
import Data.Map qualified as Map
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import GHC.Exts qualified
import Unison.Lexer.Pos (Line, Pos (..))
import Unison.Prelude
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Range (Range (..), inRange)

data Segment a = Segment {segment :: String, annotation :: Maybe a}
  deriving (Eq, Show, Ord, Functor, Foldable, Generic)

toPair :: Segment a -> (String, Maybe a)
toPair (Segment s a) = (s, a)

newtype AnnotatedText a = AnnotatedText (Seq (Segment a))
  deriving (Eq, Functor, Foldable, Show, Ord, Generic)

instance Semigroup (AnnotatedText a) where
  AnnotatedText (as :|> Segment "" _) <> bs = AnnotatedText as <> bs
  as <> AnnotatedText (Segment "" _ :<| bs) = as <> AnnotatedText bs
  AnnotatedText as <> AnnotatedText bs = AnnotatedText (as <> bs)

instance Monoid (AnnotatedText a) where
  mempty = AnnotatedText Seq.empty

instance LL.FoldableLL (AnnotatedText a) Char where
  foldl' f z (AnnotatedText at) = Foldable.foldl' f' z at
    where
      f' z (Segment str _) = L.foldl' f z str
  foldl = LL.foldl
  foldr f z (AnnotatedText at) = Foldable.foldr f' z at
    where
      f' (Segment str _) z = L.foldr f z str

instance LL.ListLike (AnnotatedText a) Char where
  singleton ch = fromString [ch]
  uncons (AnnotatedText at) = case at of
    Segment s a :<| tl -> case L.uncons s of
      Nothing -> LL.uncons (AnnotatedText tl)
      Just (hd, s) -> Just (hd, AnnotatedText $ Segment s a :<| tl)
    Seq.Empty -> Nothing
  break f at = (LL.takeWhile (not . f) at, LL.dropWhile (not . f) at)
  takeWhile f (AnnotatedText at) = case at of
    Seq.Empty -> AnnotatedText Seq.Empty
    Segment s a :<| tl ->
      let s' = L.takeWhile f s
       in if length s' == length s
            then
              AnnotatedText (pure $ Segment s a)
                <> LL.takeWhile f (AnnotatedText tl)
            else AnnotatedText (pure $ Segment s' a)
  dropWhile f (AnnotatedText at) = case at of
    Seq.Empty -> AnnotatedText Seq.Empty
    Segment s a :<| tl -> case L.dropWhile f s of
      [] -> LL.dropWhile f (AnnotatedText tl)
      s -> AnnotatedText $ (Segment s a) :<| tl
  take n (AnnotatedText at) = case at of
    Seq.Empty -> AnnotatedText Seq.Empty
    Segment s a :<| tl ->
      if n <= length s
        then AnnotatedText $ pure (Segment (take n s) a)
        else
          AnnotatedText (pure (Segment s a))
            <> LL.take (n - length s) (AnnotatedText tl)
  drop n (AnnotatedText at) = case at of
    Seq.Empty -> AnnotatedText Seq.Empty
    Segment s a :<| tl ->
      if n <= length s
        then AnnotatedText $ (Segment (drop n s) a) :<| tl
        else LL.drop (n - length s) (AnnotatedText tl)
  null (AnnotatedText at) = all (null . segment) at

-- Quoted text (indented, with source line numbers) with annotated portions.
data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset :: Line,
    text :: String,
    annotations :: Map Range a
  }
  deriving (Functor)

annotate' :: Maybe b -> AnnotatedText a -> AnnotatedText b
annotate' a (AnnotatedText at) =
  AnnotatedText $ (\(Segment s _) -> Segment s a) <$> at

deannotate :: AnnotatedText a -> AnnotatedText b
deannotate = annotate' Nothing

-- Replace the annotation (whether existing or no) with the given annotation
annotate :: a -> AnnotatedText a -> AnnotatedText a
annotate a (AnnotatedText at) =
  AnnotatedText $ (\(Segment s _) -> Segment s (Just a)) <$> at

annotateMaybe :: AnnotatedText (Maybe a) -> AnnotatedText a
annotateMaybe (AnnotatedText segments) =
  AnnotatedText (fmap (\(Segment s a) -> Segment s (join a)) segments)

trailingNewLine :: AnnotatedText a -> Bool
trailingNewLine (AnnotatedText (init :|> (Segment s _))) =
  case lastMay s of
    Just '\n' -> True
    Just _ -> False
    _ -> trailingNewLine (AnnotatedText init)
trailingNewLine _ = False

markup :: AnnotatedExcerpt a -> Map Range a -> AnnotatedExcerpt a
markup a r = a {annotations = r `Map.union` annotations a}

-- renderTextUnstyled :: AnnotatedText a -> Rendered Void
-- renderTextUnstyled (AnnotatedText chunks) = foldl' go mempty chunks
--   where go r (text, _) = r <> fromString text

textLength :: AnnotatedText a -> Int
textLength (AnnotatedText chunks) = foldl' go 0 chunks
  where
    go len (toPair -> (text, _a)) = len + length text

textEmpty :: AnnotatedText a -> Bool
textEmpty = (== 0) . textLength

condensedExcerptToText :: Int -> AnnotatedExcerpt a -> AnnotatedText a
condensedExcerptToText margin e =
  intercalateMap "    .\n" excerptToText $ snipWithContext margin e

excerptToText :: forall a. AnnotatedExcerpt a -> AnnotatedText a
excerptToText e =
  track (Pos line1 1) [] (Map.toList $ annotations e) (renderLineNumber line1) (text e)
  where
    line1 :: Int
    line1 = lineOffset e
    renderLineNumber :: Int -> AnnotatedText a
    renderLineNumber n = fromString $ " " ++ spaces ++ sn ++ " | "
      where
        sn = show n
        spaces = replicate (lineNumberWidth - length sn) ' '
        lineNumberWidth = 4

    -- step through the source characters and annotations
    track _ _ _ rendered "" = rendered
    track _ _ _ rendered "\n" = rendered <> "\n"
    track pos@(Pos line col) stack annotations rendered _input@(c : rest) =
      let (poppedAnnotations, remainingAnnotations) = span (inRange pos . fst) annotations
          -- drop any stack entries that will be closed after this char
          -- and add new stack entries
          stack' = foldl' pushColor stack0 poppedAnnotations
            where
              pushColor s (Range _ end, style) = (style, end) : s
              stack0 = dropWhile ((<= pos) . snd) stack
          maybeColor = fst <$> headMay stack'
          -- on new line, advance pos' vertically and set up line header
          -- additions :: AnnotatedText (Maybe a)
          pos' :: Pos
          (additions, pos') =
            if c == '\n'
              then ("\n" <> renderLineNumber (line + 1), Pos (line + 1) 1)
              else (annotate' maybeColor (fromString [c]), Pos line (col + 1))
       in track pos' stack' remainingAnnotations (rendered <> additions) rest

snipWithContext :: Int -> AnnotatedExcerpt a -> [AnnotatedExcerpt a]
snipWithContext margin source =
  case foldl'
    whileWithinMargin
    (Nothing, mempty, mempty)
    (Map.toList $ annotations source) of
    (Nothing, _, _) -> []
    (Just (Range (Pos startLine' _) (Pos endLine' _)), group', rest') ->
      let dropLineCount = startLine' - lineOffset source
          takeLineCount = endLine' - startLine' + 1
          text', text2' :: [String]
          (text', text2') =
            splitAt takeLineCount (drop dropLineCount (lines (text source)))
       in AnnotatedExcerpt startLine' (unlines text') group'
            : snipWithContext
              margin
              (AnnotatedExcerpt (endLine' + 1) (unlines text2') rest')
  where
    withinMargin :: Range -> Range -> Bool
    withinMargin (Range _start1 (Pos end1 _)) (Range (Pos start2 _) _end2) =
      end1 + margin >= start2

    whileWithinMargin ::
      (Maybe Range, Map Range a, Map Range a) ->
      (Range, a) ->
      (Maybe Range, Map Range a, Map Range a)
    whileWithinMargin (r0, taken, rest) (r1, a1) =
      case r0 of
        Nothing ->
          -- haven't processed any annotations yet
          (Just r1, Map.singleton r1 a1, mempty)
        Just r0 ->
          -- if all annotations so far can be joined without .. separations
          if null rest
            then -- if this one can be joined to the new region without .. separation
              if withinMargin r0 r1
                then -- add it to the first set and grow the compare region
                  (Just $ r0 <> r1, Map.insert r1 a1 taken, mempty)
                else -- otherwise add it to the second set
                  (Just r0, taken, Map.singleton r1 a1)
            else -- once we've added to the second set, anything more goes there too
              (Just r0, taken, Map.insert r1 a1 rest)

instance IsString (AnnotatedText a) where
  fromString s = AnnotatedText . pure $ Segment s Nothing

instance IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty

instance GHC.Exts.IsList (AnnotatedText a) where
  type Item (AnnotatedText a) = Char
  fromList s = fromString s
  toList (AnnotatedText s) = join . Foldable.toList $ fmap segment s
