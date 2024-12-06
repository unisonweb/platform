-- | Parse and print CommonMark (like Github-flavored Markdown) transcripts.
module Unison.Codebase.Transcript.Parser
  ( -- * printing
    formatAPIRequest,
    formatUcmLine,
    formatInfoString,
    formatStanzas,

    -- * parsing
    stanzas,
    ucmLine,
    apiRequest,
    fenced,
    hidden,
    expectingError,
    language,
  )
where

import CMark qualified
import Data.Bool (bool)
import Data.Char qualified as Char
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Unison.Codebase.Transcript hiding (expectingError, generated, hasBug, hidden)
import Unison.Prelude
import Unison.Project (fullyQualifiedProjectAndBranchNamesParser)

padIfNonEmpty :: Text -> Text
padIfNonEmpty line = if Text.null line then line else "  " <> line

formatAPIRequest :: APIRequest -> Text
formatAPIRequest = \case
  GetRequest txt -> "GET " <> txt <> "\n"
  APIComment txt -> "--" <> txt <> "\n"
  APIResponseLine txt -> Text.unlines . fmap padIfNonEmpty $ Text.lines txt

formatUcmLine :: UcmLine -> Text
formatUcmLine = \case
  UcmCommand context txt -> formatContext context <> "> " <> txt <> "\n"
  UcmComment txt -> "--" <> txt <> "\n"
  UcmOutputLine txt -> Text.unlines . fmap padIfNonEmpty $ Text.lines txt
  where
    formatContext (UcmContextProject projectAndBranch) = into @Text projectAndBranch

formatStanzas :: [Stanza] -> Text
formatStanzas =
  CMark.nodeToCommonmark [] Nothing . CMark.Node Nothing CMark.DOCUMENT . fmap (either id processedBlockToNode)

processedBlockToNode :: ProcessedBlock -> CMark.Node
processedBlockToNode = \case
  Ucm tags cmds -> mkNode (\() -> Nothing) "ucm" tags $ foldr ((<>) . formatUcmLine) "" cmds
  Unison tags txt -> mkNode id "unison" tags txt
  API tags apiRequests -> mkNode (\() -> Nothing) "api" tags $ foldr ((<>) . formatAPIRequest) "" apiRequests
  where
    mkNode formatA lang = CMarkCodeBlock Nothing . formatInfoString formatA lang

type P = P.Parsec Void Text

stanzas :: FilePath -> Text -> Either (P.ParseErrorBundle Text Void) [Stanza]
stanzas srcName =
  -- TODO: Internal warning if `_DOCUMENT` isn’t `CMark.DOCUMENT`.
  (\(CMark.Node _ _DOCUMENT blocks) -> traverse stanzaFromNode blocks)
    . CMark.commonmarkToNode [CMark.optSourcePos]
  where
    stanzaFromNode :: CMark.Node -> Either (P.ParseErrorBundle Text Void) Stanza
    stanzaFromNode node = case node of
      CMarkCodeBlock (Just CMark.PosInfo {startLine, startColumn}) info body ->
        maybe (Left node) pure <$> snd (P.runParser' fenced $ fencedState srcName startLine startColumn info body)
      _ -> pure $ Left node

ucmLine :: P UcmLine
ucmLine = ucmOutputLine <|> ucmComment <|> ucmCommand
  where
    ucmCommand :: P UcmLine
    ucmCommand =
      UcmCommand
        <$> fmap
          UcmContextProject
          (fullyQualifiedProjectAndBranchNamesParser <* lineToken (P.chunk ">") <* nonNewlineSpaces)
        <*> restOfLine

    ucmComment :: P UcmLine
    ucmComment =
      P.label "comment (delimited with “--”)" $
        UcmComment <$> (P.chunk "--" *> restOfLine)

    ucmOutputLine :: P UcmLine
    ucmOutputLine = UcmOutputLine <$> (P.chunk "  " *> restOfLine <|> "" <$ P.single '\n' <|> "" <$ P.chunk " \n")

restOfLine :: P Text
restOfLine = P.takeWhileP Nothing (/= '\n') <* P.single '\n'

apiRequest :: P APIRequest
apiRequest =
  GetRequest <$> (word "GET" *> spaces *> restOfLine)
    <|> APIComment <$> (P.chunk "--" *> restOfLine)
    <|> APIResponseLine <$> (P.chunk "  " *> restOfLine <|> "" <$ P.single '\n' <|> "" <$ P.chunk " \n")

formatInfoString :: (a -> Maybe Text) -> Text -> InfoTags a -> Text
formatInfoString formatA language infoTags =
  let infoTagText = formatInfoTags formatA infoTags
   in if Text.null infoTagText then language else language <> " " <> infoTagText

formatInfoTags :: (a -> Maybe Text) -> InfoTags a -> Text
formatInfoTags formatA (InfoTags hidden expectingError hasBug generated additionalTags) =
  Text.intercalate " " $
    catMaybes
      [ formatHidden hidden,
        formatExpectingError expectingError,
        formatHasBug hasBug,
        formatGenerated generated,
        formatA additionalTags
      ]

infoTags :: P a -> P (InfoTags a)
infoTags p =
  InfoTags
    <$> lineToken hidden
    <*> lineToken expectingError
    <*> lineToken hasBug
    <*> lineToken generated
    <*> p
    <* P.single '\n'

-- | Parses the info string and contents of a fenced code block.
fenced :: P (Maybe ProcessedBlock)
fenced = do
  fenceType <- lineToken language
  case fenceType of
    "ucm" -> fmap pure $ Ucm <$> infoTags (pure ()) <*> P.manyTill ucmLine P.eof
    "unison" -> fmap pure $ Unison <$> infoTags (optional untilSpace1) <*> P.getInput
    "api" -> fmap pure $ API <$> infoTags (pure ()) <*> P.manyTill apiRequest P.eof
    _ -> pure Nothing

word :: Text -> P Text
word text = P.chunk text <* P.notFollowedBy P.alphaNumChar

lineToken :: P a -> P a
lineToken p = p <* nonNewlineSpaces

nonNewlineSpaces :: P ()
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch == ' ' || ch == '\t')

formatHidden :: Hidden -> Maybe Text
formatHidden = \case
  HideAll -> pure ":hide:all"
  HideOutput -> pure ":hide"
  Shown -> Nothing

hidden :: P Hidden
hidden =
  (HideAll <$ word ":hide:all")
    <|> (HideOutput <$ word ":hide")
    <|> pure Shown

formatExpectingError :: ExpectingError -> Maybe Text
formatExpectingError = bool Nothing $ pure ":error"

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

formatHasBug :: HasBug -> Maybe Text
formatHasBug = bool Nothing $ pure ":bug"

hasBug :: P HasBug
hasBug = isJust <$> optional (word ":bug")

formatGenerated :: ExpectingError -> Maybe Text
formatGenerated = bool Nothing $ pure ":added-by-ucm"

generated :: P Bool
generated = isJust <$> optional (word ":added-by-ucm")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_')

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

-- | Create a parser state that has source locations that match the file (as opposed to being relative to the start of
--   the individual fenced code block).
--
--  __NB__: If a code block has a fence longer than the minimum (three backticks), the columns for parse errors in the
--          info string will be slightly off (but the printed code excerpt will match the reported positions).
--
--  __NB__: Creating custom states is likely simpler starting with Megaparsec 9.6.0.
fencedState ::
  -- | file containing the fenced code block
  FilePath ->
  -- | `CMark.startLine` for the block
  Int ->
  -- | `CMark.startColumn` for the block`
  Int ->
  -- | info string from the block
  Text ->
  -- | contents of the code block
  Text ->
  P.State Text e
fencedState name startLine startColumn info body =
  let -- This is the most common opening fence, so we assume it’s the right one. I don’t think there’s any way to get
      -- the actual size of the fence from "CMark", so this can be wrong sometimes, but it’s probably the approach
      -- that’s least likely to confuse users.
      openingFence = "``` "
      -- Glue the info string and body back together, as if they hadn’t been split by "CMark". This keeps the position
      -- info in sync.
      s = info <> "\n" <> body
   in P.State
        { stateInput = s,
          stateOffset = 0,
          statePosState =
            P.PosState
              { pstateInput = s,
                pstateOffset = 0,
                -- `CMark.startColumn` marks the beginning of the fence, not the beginning of the info string, so we
                -- adjust it for the fence that precedes it.
                pstateSourcePos = P.SourcePos name (P.mkPos startLine) . P.mkPos $ startColumn + length openingFence,
                pstateTabWidth = P.defaultTabWidth,
                -- Ensure we print the fence as part of the line if there’s a parse error in the info string.
                pstateLinePrefix = openingFence
              },
          stateParseErrors = []
        }
