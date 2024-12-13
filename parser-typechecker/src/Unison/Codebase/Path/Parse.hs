module Unison.Codebase.Path.Parse
  ( -- * Path parsing functions
    parsePath,
    parsePath',
    parseSplit,
    parseName,
    parseHQSplit,
    parseHQName,
    parseHashOrHQName,

    -- * Path parsers
    pathP,
    pathP',
    splitP,
    splitP',
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P (char)
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.Codebase.Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
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
  "" -> Right relativeEmpty'
  "." -> Right absoluteEmpty'
  path -> fromName' <$> parseName path

parseSplit :: String -> Either Text (Split Path)
parseSplit = runParser splitP

parseName :: String -> Either Text Name
parseName = runParser splitP'

parseHashOrHQName :: String -> Either Text (HQ'.HashOrHQ Name)
parseHashOrHQName = runParser shortHashOrHQNameP'

parseHQSplit :: String -> Either Text (HQ'.HashQualified (Split Path))
parseHQSplit s =
  parseHQName s >>= traverse \name ->
    if Name.isAbsolute name
      then Left $ "Sorry, you can't use an absolute name like " <> Text.pack s <> " here."
      else
        let h :| t = Name.reverseSegments name
         in pure (fromList $ reverse t, h)

parseHQName :: String -> Either Text (HQ'.HashQualified Name)
parseHQName = runParser hqNameP

runParser :: Parsec (Lexer.Token Text) [Char] a -> String -> Either Text a
runParser p =
  mapLeft (Text.pack . P.errorBundlePretty) . P.runParser (p <* P.eof) ""

------------------------------------------------------------------------------------------------------------------------
-- Path parsers

pathP :: Parsec (Lexer.Token Text) [Char] Path
pathP = (unsplit <$> splitP) <|> pure empty

pathP' :: Parsec (Lexer.Token Text) [Char] Path'
pathP' =
  asum
    [ fromName' <$> splitP',
      P.char '.' $> absoluteEmpty',
      pure relativeEmpty'
    ]

splitP :: Parsec (Lexer.Token Text) [Char] (Split Path)
splitP = splitFromName <$> P.withParsecT (fmap NameSegment.renderParseErr) Name.relativeNameP

splitP' :: Parsec (Lexer.Token Text) [Char] Name
splitP' = P.withParsecT (fmap NameSegment.renderParseErr) Name.nameP

shortHashOrHQNameP' :: Parsec (Lexer.Token Text) [Char] (HQ'.HashOrHQ Name)
shortHashOrHQNameP' = Left <$> ShortHash.shortHashP <|> Right <$> hqNameP

hqNameP :: Parsec (Lexer.Token Text) [Char] (HQ'.HashQualified Name)
hqNameP = do
  name <- splitP'
  P.optional (P.withParsecT (fmap ("invalid hash: " <>)) ShortHash.shortHashP) <&> \case
    Nothing -> HQ'.fromName name
    Just hash -> HQ'.HashQualified name hash
