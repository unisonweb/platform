module Unison.Codebase.Editor.HandleInput.FormatFile
  ( formatFile,
    applyTextReplacements,
    TextReplacement (..),
  )
where

import Control.Lens hiding (List)
import Control.Monad.State
import Data.IntervalMap.Interval qualified as Interval
import Data.List qualified as List
import Data.List.NonEmpty.Extra qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Builder qualified as TB
import U.Core.ABT qualified as ABT
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as Decl
import Unison.HashQualified qualified as HQ
import Unison.Lexer.Pos qualified as Pos
import Unison.Name qualified as Name
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Summary qualified as FileSummary
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Range (Range (..))
import Unison.Var qualified as Var

-- | Format a file, returning a list of Text replacements to apply to the file.
formatFile ::
  (Monad m) =>
  (Maybe (UnisonFile Symbol Ann.Ann) -> Maybe (TypecheckedUnisonFile Symbol Ann.Ann) -> m PPED.PrettyPrintEnvDecl) ->
  Int ->
  Path.Absolute ->
  Maybe (UnisonFile Symbol Ann.Ann) ->
  Maybe (TypecheckedUnisonFile Symbol Ann.Ann) ->
  Maybe (Set Range) ->
  m (Maybe [TextReplacement])
formatFile makePPEDForFile formattingWidth currentPath inputParsedFile inputTypecheckedFile mayRangesToFormat = runMaybeT $ do
  let (mayParsedFile, mayTypecheckedFile) = mkUnisonFilesDeterministic inputParsedFile inputTypecheckedFile
  fileSummary <- hoistMaybe $ FileSummary.mkFileSummary mayParsedFile mayTypecheckedFile
  filePPED <- lift $ makePPEDForFile mayParsedFile mayTypecheckedFile
  parsedFile <- hoistMaybe mayParsedFile
  -- Don't format anything unless the file typechecks.
  -- The formatter mostly works on a parsed file, but we currently fail to print
  -- '{{ .. }}'-style docs correctly if they don't typecheck.
  _typecheckedFile <- hoistMaybe mayTypecheckedFile
  formattedDecls <-
    (FileSummary.allTypeDecls fileSummary)
      & fmap
        ( \(ref, decl) ->
            let tldAnn = either (Decl.annotation . Decl.toDataDecl) (Decl.annotation) decl
             in (tldAnn, ref, decl)
        )
      & Map.filter (\(tldAnn, _, _) -> isInFormatRange tldAnn)
      & itraverse \sym (tldAnn, ref, decl) -> do
        symName <- hoistMaybe (Name.parseVar sym)
        let declNameSegments = NEL.appendr (Path.toList (Path.unabsolute currentPath)) (Name.segments symName)
        let declName = Name.fromSegments declNameSegments
        let hqName = HQ.fromName symName
        let biasedPPED = PPED.biasTo [declName] filePPED
        -- If it's a unique type the parser will re-order constructors arbitrarily because
        -- the random unique seed gets mixed in and then things are ordered by hash.
        --
        -- The constructor order will always be re-ordered on definition Add anyways, so we
        -- just force alphabetical order for unique types for sanity reasons.
        -- Doesn't work unless we alter it before building the pped
        -- let deterministicDecl = decl & Decl.declAsDataDecl_ . Decl.constructors_ %~ sortOn (view _1)
        pure $
          (tldAnn, DeclPrinter.prettyDecl biasedPPED DeclPrinter.RenderUniqueTypeGuids'No (Reference.DerivedId ref) hqName decl)
            & over _2 Pretty.syntaxToColor
  formattedTerms <-
    (FileSummary.termsBySymbol fileSummary)
      & Map.filter (\(tldAnn, _, trm, _) -> shouldFormatTerm tldAnn trm)
      & itraverse \sym (tldAnn, mayRefId, trm, _typ) -> do
        symName <- hoistMaybe (Name.parseVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute currentPath)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let hqName = HQ.NameOnly symName
        let biasedPPED = PPED.biasTo [defName] filePPED
        let definitionPPE = case mayRefId of
              Just refId -> PPE.declarationPPE biasedPPED (Reference.DerivedId refId)
              Nothing -> PPED.suffixifiedPPE biasedPPED
        let formattedTerm = Pretty.syntaxToColor $ TermPrinter.prettyBinding definitionPPE hqName (removeGeneratedTypeAnnotations parsedFile sym trm)
        -- TODO: format watch expressions and test watches
        -- let formattedWatches =
        --       allWatches fileSummary & map \(_tldAnn, maySym, _mayRef, trm, _mayType, mayWatchKind) -> do
        --         case (mayWatchKind, maySym) of
        --           (Just wk, Just (Symbol.Symbol _ (Var.User {}))) ->
        --             -- Watch with binding
        --             Pretty.syntaxToColor $ Pretty.string wk <> "> " <> TermPrinter.prettyBindingWithoutTypeSignature definitionPPE hqName (stripTypeAnnotation trm)
        --           (Just wk, _) -> Pretty.string wk <> "> " <> TermPrinter.prettyBlock False definitionPPE (stripTypeAnnotation trm)
        --           (Nothing, _) -> "> " <> TermPrinter.prettyBlock False definitionPPE (stripTypeAnnotation trm)
        pure (tldAnn, formattedTerm)

  -- Only keep definitions which are _actually_ in the file, skipping generated accessors
  -- and such.
  let nonGeneratedDefs =
        (formattedTerms <> formattedDecls)
          & mapMaybe
            ( \case
                (Ann.Ann {start, end}, txt) -> Just ((start, end), txt)
                _ -> Nothing
            )
  -- when (null filteredDefs) empty {- Don't format if we have no definitions or it wipes out the fold! -}
  let textEdits =
        nonGeneratedDefs & foldMap \((start, end), txt) -> do
          range <- maybeToList $ annToRange (Ann.Ann start end)
          pure $ (TextReplacement (Text.pack $ Pretty.toPlain (Pretty.Width formattingWidth) txt) range)
  pure textEdits
  where
    isInFormatRange :: Ann.Ann -> Bool
    isInFormatRange ann =
      case mayRangesToFormat of
        Nothing -> True
        Just rangesToFormat -> any (annRangeOverlap ann) rangesToFormat
    shouldFormatTerm :: Ann.Ann -> Term.Term Symbol Ann.Ann -> Bool
    shouldFormatTerm ann trm =
      isInFormatRange ann
        && not (isUntypecheckedDoc trm)

    -- The lexer converts '{{ .. }}' into 'syntax.docUntitledSection (..)', but the pretty
    -- printer doesn't print it back as '{{ .. }}' unless it typechecks, so
    -- we just don't format docs that have un-resolved 'docUntitledSection' symbols.
    isUntypecheckedDoc :: Term.Term Symbol Ann.Ann -> Bool
    isUntypecheckedDoc trm =
      ABT.freeVars trm
        & Set.map Var.nameStr
        & Set.member "syntax.docUntitledSection"
    -- Does the given range overlap with the given annotation?
    annRangeOverlap :: Ann.Ann -> Range -> Bool
    annRangeOverlap a r =
      annToInterval a & \case
        Nothing -> False
        Just annI -> rangeToInterval r `Interval.overlaps` annI

    -- Typechecking ALWAYS adds a type-signature, but we don't want to add ones that didn't
    -- already exist in the source file.
    removeGeneratedTypeAnnotations ::
      UnisonFile Symbol a -> Symbol -> (Term.Term Symbol a) -> (Term.Term Symbol a)
    removeGeneratedTypeAnnotations uf v = \case
      Term.Ann' tm _annotation | not (hasUserTypeSignature uf v) -> tm
      x -> x

    -- This is a bit of a hack.
    -- The file parser uses a different unique ID for unique types on every parse,
    -- that id changes hashes, and constructors are ordered by that hash.
    -- This means that pretty-printing isn't deterministic and constructors will re-order
    -- themselves on every save :|
    --
    -- It's difficult and a bad idea to change the parser to use a deterministic unique ID,
    -- so instead we just re-sort the constructors by their source-file annotation AFTER
    -- parsing. This is fine for pretty-printing, but don't use this for anything other than
    -- formatting since the Decls it produces aren't technically valid.
    mkUnisonFilesDeterministic :: Maybe (UnisonFile Symbol Ann.Ann) -> Maybe (TypecheckedUnisonFile Symbol Ann.Ann) -> (Maybe (UnisonFile Symbol Ann.Ann), Maybe (TypecheckedUnisonFile Symbol Ann.Ann))
    mkUnisonFilesDeterministic mayUnisonFile mayTypecheckedFile =
      let sortedUF =
            mayUnisonFile
              & _Just . #dataDeclarationsId . traversed . _2 %~ sortConstructors
              & _Just . #effectDeclarationsId . traversed . _2 . Decl.asDataDecl_ %~ sortConstructors
          sortedTF =
            mayTypecheckedFile
              & _Just . #dataDeclarationsId' . traversed . _2 %~ sortConstructors
              & _Just . #effectDeclarationsId' . traversed . _2 . Decl.asDataDecl_ %~ sortConstructors
       in (sortedUF, sortedTF)

    -- ppedForFileHelper
    sortConstructors :: Decl.DataDeclaration v Ann.Ann -> Decl.DataDeclaration v Ann.Ann
    sortConstructors dd =
      -- Sort by their Ann so we keep the order they were in the original file.
      dd & Decl.constructors_ %~ sortOn @Ann.Ann (view _1)

annToRange :: Ann.Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.GeneratedFrom a -> annToRange a
  Ann.Ann start end -> Just $ Range start end

rangeToInterval :: Range -> Interval.Interval Pos.Pos
rangeToInterval (Range start end) =
  Interval.ClosedInterval start end

annToInterval :: Ann.Ann -> Maybe (Interval.Interval Pos.Pos)
annToInterval ann = annToRange ann <&> rangeToInterval

-- | Returns 'True' if the given symbol is a term with a user provided type signature in the
-- parsed file, false otherwise.
hasUserTypeSignature :: (Eq v) => UnisonFile v a -> v -> Bool
hasUserTypeSignature parsedFile sym =
  Map.toList (UF.terms parsedFile)
    & any (\(v, (_, trm)) -> v == sym && isJust (Term.getTypeAnnotation trm))

-- | A text replacement to apply to a file.
data TextReplacement = TextReplacement
  { -- The new new text to replace the old text in the range with. w
    replacementText :: Text,
    -- The range to replace, [start, end)
    replacementRange :: Range
  }
  deriving (Eq, Show)

-- | Apply a list of range replacements to a text, returning the updated text.
--
-- >>> applyFormatUpdates [TextReplacement "cakes" (Range (Pos.Pos 1 21) (Pos.Pos 1 28))] "my favorite food is oranges because\nthey are delicious and nutritious"
-- "my favorite food is cakes because\nthey are delicious and nutritious"
--
-- Multiple replacements.
-- >>> let txt = "my favorite food is oranges because\nthey are delicious and nutritious"
-- >>> let replacements = [TextReplacement "cakes" (Range (Pos.Pos 1 21) (Pos.Pos 1 28)), TextReplacement "decadent" (Range (Pos.Pos 2 10) (Pos.Pos 2 19)), TextReplacement "tasty" (Range (Pos.Pos 2 24) (Pos.Pos 2 34))]
-- >>> applyFormatUpdates replacements txt
-- "my favorite food is cakes because\nthey are decadent and tasty"
--
-- Multi-line replacements.
-- >>> let txt = "mary had a little lamb\nwhose fleece was white as snow\nand everywhere that mary went\nthe lamb was sure to go"
-- >>> let replacements = [TextReplacement "lambo, which" (Range (Pos.Pos 1 19) (Pos.Pos 2 13)), TextReplacement " the people stared" (Range (Pos.Pos 3 99) (Pos.Pos 4 99))]
-- >>> applyFormatUpdates replacements txt
-- "mary had a little lambo, which was white as snow\nand everywhere that mary went the people stared"
applyTextReplacements :: [TextReplacement] -> Text -> Text
applyTextReplacements replacements inputText = applyTextReplacementsHelper relativeOffsets (Text.lines inputText) & TB.run
  where
    tupleReplacements :: [(Int, Int, Maybe Text)]
    tupleReplacements =
      replacements
        & foldMap
          ( \(TextReplacement txt (Range (Pos.Pos startLine startCol) (Pos.Pos endLine endCol))) ->
              -- Convert from 1-based indexing to 0-based indexing
              [(startLine - 1, startCol - 1, Nothing), (endLine - 1, endCol - 1, Just txt)]
          )
    relativeOffsets = relativizeOffsets (List.sortOn (\(line, col, _) -> (line, col)) tupleReplacements)

-- | Given a list of offsets, return a list of offsets where each offset is positioned relative to the previous offset.
-- I.e. if the first offset is at line 3 col 4, and the next is at line 5 col 6, we subtract 3
-- from the 5 but leave the column alone since it's on a different line, resulting in [(3,4), (2,6)]
--
-- If the first offset is at line 3 col 4, and the next is at line 3 col 6, we subtract 3 from
-- the line number AND subtract 4 from the column number since they're on the same line.
-- Resulting in [(3,4), (0,2)]
--
--
-- >>> relativizeOffsets [(0, 0, Nothing), (0, 4, Just "1"), (0, 10, Nothing), (1, 0, Just "2"), (1, 5, Nothing), (5, 10, Just "3")]
-- NOW [(0,0,Nothing),(0,4,Just "1"),(0,6,Nothing),(1,0,Just "2"),(0,5,Nothing),(4,10,Just "3")]
relativizeOffsets :: [(Int, Int, Maybe Text)] -> [(Int, Int, Maybe Text)]
relativizeOffsets xs =
  let grouped = List.groupBy (\(a, _, _) (b, _, _) -> a == b) xs
   in grouped
        & fmap (snd . List.mapAccumL (\acc (line, col, r) -> (col, (line, col - acc, r))) 0)
        & List.concat
        & snd . List.mapAccumL (\acc (line, col, r) -> (line, (line - acc, col, r))) 0

-- | Apply a list of range replacements to a list of lines, returning the result.
--
-- >>> applyTextReplacementsHelper [(0, 1, Nothing), (0, 4, Just "1"), (0, 2, Nothing), (1, 3, Just "2"), (0, 5, Nothing), (1, 10, Just "3")] ["abcdefghijk", "lmnopqrstuv", "wxyz", "1234567890"] & TB.run
-- "a1fg2opqrs3\n1234567890"
applyTextReplacementsHelper :: [(Int, Int, Maybe Text)] -> [Text] -> TB.Builder
applyTextReplacementsHelper [] ls = TB.intercalate "\n" (TB.text <$> ls)
applyTextReplacementsHelper _ [] = mempty
applyTextReplacementsHelper ((0, col, r) : rest) (l : ls) =
  let (prefix, suffix) = Text.splitAt col l
   in TB.text (fromMaybe prefix r) <> applyTextReplacementsHelper rest (suffix : ls)
applyTextReplacementsHelper ((line, col, r) : rest) ls =
  case List.splitAt line ls of
    (prefixLines, []) -> TB.intercalate "\n" (TB.text <$> prefixLines)
    (prefixLines, (lastLine : restLines)) ->
      let (prefixChars, suffixChars) = Text.splitAt col lastLine
          segment =
            (TB.intercalate "\n" $ fmap TB.text prefixLines)
              <> TB.char '\n'
              <> TB.text prefixChars
       in maybe segment TB.text r <> applyTextReplacementsHelper rest (suffixChars : restLines)
