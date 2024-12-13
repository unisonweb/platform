module Unison.Codebase.Path.Parse
  ( -- * Path parsing functions
    parsePath,
    parsePath',
    parseSplit,
    parseSplit',
    parseHQSplit,
    parseHQSplit',
    parseHashOrHQSplit',

    -- * Path parsers
    pathP,
    pathP',
    splitP,
    splitP',
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P (char)
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.Codebase.Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Prelude hiding (empty, toList)
import Unison.Syntax.Lexer qualified as Lexer
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment (renderParseErr)
import Unison.Syntax.ShortHash qualified as ShortHash

------------------------------------------------------------------------------------------------------------------------
-- Path parsing functions

parsePath :: String -> Either Text Path
parsePath = runParser pathP

parsePath' :: String -> Either Text Path'
parsePath' = \case
  "" -> Right currentPath
  "." -> Right root'
  path -> unsplit <$> parseSplit' path

parseSplit :: String -> Either Text (Split Path)
parseSplit = runParser splitP

parseSplit' :: String -> Either Text (Split Path')
parseSplit' = runParser splitP'

parseHashOrHQSplit' :: String -> Either Text (HQ'.HashOrHQ (Split Path'))
parseHashOrHQSplit' = runParser shortHashOrHQSplitP'

parseHQSplit :: String -> Either Text (HQ'.HashQualified (Split Path))
parseHQSplit s =
  parseHQSplit' s >>= traverse \(path, seg) -> case path of
    RelativePath' (Relative p) -> pure (p, seg)
    AbsolutePath' (Absolute _) -> Left $ "Sorry, you can't use an absolute name like " <> Text.pack s <> " here."

parseHQSplit' :: String -> Either Text (HQ'.HashQualified (Split Path'))
parseHQSplit' = runParser hqSplitP'

runParser :: Parsec (Lexer.Token Text) [Char] a -> String -> Either Text a
runParser p =
  mapLeft (Text.pack . P.errorBundlePretty) . P.runParser (p <* P.eof) ""

------------------------------------------------------------------------------------------------------------------------
-- Path parsers

pathP :: Parsec (Lexer.Token Text) [Char] Path
pathP = (unsplit <$> splitP) <|> pure mempty

pathP' :: Parsec (Lexer.Token Text) [Char] Path'
pathP' =
  asum
    [ unsplit <$> splitP',
      P.char '.' $> root',
      pure currentPath
    ]

splitP :: Parsec (Lexer.Token Text) [Char] (Split Path)
splitP = splitFromName <$> P.withParsecT (fmap NameSegment.renderParseErr) Name.relativeNameP

splitP' :: Parsec (Lexer.Token Text) [Char] (Split Path')
splitP' = parentOfName <$> P.withParsecT (fmap NameSegment.renderParseErr) Name.nameP

shortHashOrHQSplitP' :: Parsec (Lexer.Token Text) [Char] (HQ'.HashOrHQ (Split Path'))
shortHashOrHQSplitP' = Left <$> ShortHash.shortHashP <|> Right <$> hqSplitP'

hqSplitP' :: Parsec (Lexer.Token Text) [Char] (HQ'.HashQualified (Split Path'))
hqSplitP' = do
  split <- splitP'
  P.optional (P.withParsecT (fmap ("invalid hash: " <>)) ShortHash.shortHashP) <&> \case
    Nothing -> HQ'.fromName split
    Just hash -> HQ'.HashQualified split hash
