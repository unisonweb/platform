module Unison.Syntax.Lexer.Unison
  ( Token (..),
    Line,
    Column,
    Err (..),
    Pos (..),
    Lexeme (..),
    lexer,
    lexer',
    preParse,
    escapeChars,
    debugFilePreParse,
    debugPreParse,
    debugPreParse',
    showEscapeChar,
    touches,

    -- * Lexers
    typeOrTerm,

    -- * Character classifiers
    wordyIdChar,
    wordyIdStartChar,
    symbolyIdChar,

    -- * Error formatting
    formatTrivialError,
    displayLexeme,
  )
where

import Control.Lens qualified as Lens
import Control.Monad.State qualified as S
import Data.Char (isAlphaNum, isDigit, isSpace, ord, toLower)
import Data.Foldable qualified as Foldable
import Data.Functor.Classes (Show1 (..), showsPrec1)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as Nel
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Exts (sortWith)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as CP
import Text.Megaparsec.Char.Lexer qualified as LP
import Text.Megaparsec.Error qualified as EP
import Text.Megaparsec.Internal qualified as PI
import U.Codebase.Reference (ReferenceType (..))
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment (docSegment)
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (toText)
import Unison.Syntax.Lexer
import Unison.Syntax.Lexer.Token (posP, tokenP)
import Unison.Syntax.Name qualified as Name (isSymboly, nameP, toText, unsafeParseText)
import Unison.Syntax.NameSegment qualified as NameSegment (ParseErr (..))
import Unison.Syntax.Parser.Doc qualified as Doc
import Unison.Syntax.Parser.Doc.Data qualified as Doc
import Unison.Syntax.ReservedWords (delimiters, typeModifiers, typeOrAbility)
import Unison.Syntax.ShortHash qualified as ShortHash (shortHashP)
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.Monoid (intercalateMap)

type BlockName = String

type Layout = [(BlockName, Column)]

data ParsingEnv = ParsingEnv
  { -- | layout stack
    layout :: !Layout,
    -- | `Just b` if a block of type `b` is being opened
    opening :: Maybe BlockName,
    -- | are we inside a construct that uses layout?
    inLayout :: Bool
  }
  deriving (Show)

initialEnv :: BlockName -> ParsingEnv
initialEnv scope = ParsingEnv [] (Just scope) True

type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

data Err
  = ReservedWordyId String
  | InvalidSymbolyId String
  | ReservedSymbolyId String
  | InvalidShortHash String
  | InvalidBytesLiteral String
  | InvalidHexLiteral
  | InvalidOctalLiteral
  | InvalidBinaryLiteral
  | Both Err Err
  | MissingFractional String -- ex `1.` rather than `1.04`
  | MissingExponent String -- ex `1e` rather than `1e3`
  | UnknownLexeme
  | TextLiteralMissingClosingQuote String
  | InvalidEscapeCharacter Char
  | LayoutError
  | CloseWithoutMatchingOpen String String -- open, close
  | UnexpectedDelimiter String
  | UnexpectedTokens String -- Catch-all for all other lexer errors, representing some unexpected tokens.
  deriving stock (Eq, Ord, Show) -- richer algebra

-- Design principle:
--   `[Lexeme]` should be sufficient information for parsing without
--   further knowledge of spacing or indentation levels
--   any knowledge of comments
data Lexeme
  = -- | start of a block
    Open String
  | -- | separator between elements of a block
    Semi IsVirtual
  | -- | end of a block
    Close
  | -- | reserved tokens such as `{`, `(`, `type`, `of`, etc
    Reserved String
  | -- | text literals, `"foo bar"`
    Textual String
  | -- | character literals, `?X`
    Character Char
  | -- | a (non-infix) identifier. invariant: last segment is wordy
    WordyId (HQ'.HashQualified Name)
  | -- | an infix identifier. invariant: last segment is symboly
    SymbolyId (HQ'.HashQualified Name)
  | -- | numeric literals, left unparsed
    Numeric String
  | -- | bytes literals
    Bytes Bytes.Bytes
  | -- | hash literals
    Hash ShortHash
  | Err Err
  | Doc (Doc.UntitledSection (Doc.Tree (Token (ReferenceType, HQ'.HashQualified Name)) [Token Lexeme]))
  deriving stock (Eq, Show, Ord)

type IsVirtual = Bool -- is it a virtual semi or an actual semi?

-- Committed failure
err :: (P.TraversableStream s, P.MonadParsec (Token Err) s m) => Pos -> Err -> m x
err start t = do
  stop <- posP
  -- This consumes a character and therefore produces committed failure,
  -- so `err s t <|> p2` won't try `p2`
  _ <- void P.anySingle <|> P.eof
  P.customFailure (Token t start stop)

token :: P Lexeme -> P [Token Lexeme]
token = token' (\a start end -> [Token a start end])

-- Token parser: strips trailing whitespace and comments after a
-- successful parse, and also takes care of emitting layout tokens
-- (such as virtual semicolons and closing tokens).
token' :: (a -> Pos -> Pos -> [Token Lexeme]) -> P a -> P [Token Lexeme]
token' tok p = LP.lexeme space (token'' tok p)

-- Token parser implementation which leaves trailing whitespace and comments
-- but does emit layout tokens such as virtual semicolons and closing tokens.
token'' :: (a -> Pos -> Pos -> [Token Lexeme]) -> P a -> P [Token Lexeme]
token'' tok p = do
  start <- posP
  -- We save the current state so we can backtrack the state if `p` fails.
  env <- S.get
  layoutToks <- case opening env of
    -- If we're opening a block named b, we push (b, currentColumn) onto
    -- the layout stack. Example:
    --
    --   blah = cases
    --       {- A comment -}
    --          -- A one-line comment
    --     0 -> "hi"
    --     1 -> "bye"
    --
    -- After the `cases` token, the state will be opening = Just "cases",
    -- meaning the parser is searching for the next non-whitespace/comment
    -- character to determine the leftmost column of the `cases` block.
    -- That will be the column of the `0`.
    Just blockname ->
      -- special case - handling of empty blocks, as in:
      --   foo =
      --   bar = 42
      if blockname == "=" && column start <= top l && not (null l)
        then do
          S.put (env {layout = (blockname, column start + 1) : l, opening = Nothing})
          pops start
        else [] <$ S.put (env {layout = layout', opening = Nothing})
      where
        layout' = (blockname, column start) : l
        l = layout env
    -- If we're not opening a block, we potentially pop from
    -- the layout stack and/or emit virtual semicolons.
    Nothing -> if inLayout env then pops start else pure []
  beforeTokenPos <- posP
  a <- p <|> (S.put env >> fail "resetting state")
  endPos <- posP
  pure $ layoutToks ++ tok a beforeTokenPos endPos
  where
    pops :: Pos -> P [Token Lexeme]
    pops p = do
      env <- S.get
      let l = layout env
      if column p == top l && topContainsVirtualSemis l
        then pure [Token (Semi True) p p]
        else
          if column p > top l || topHasClosePair l
            then pure []
            else
              if column p < top l
                then S.put (env {layout = pop l}) >> ((Token Close p p :) <$> pops p)
                else -- we hit this branch exactly when `token''` is given the state
                -- `{layout = [], opening = Nothing, inLayout = True}`
                  fail "internal error: token''"

    -- don't emit virtual semis in (, {, or [ blocks
    topContainsVirtualSemis :: Layout -> Bool
    topContainsVirtualSemis = \case
      [] -> False
      ((name, _) : _) -> name /= "(" && name /= "{" && name /= "["

    topHasClosePair :: Layout -> Bool
    topHasClosePair [] = False
    topHasClosePair ((name, _) : _) =
      name `elem` ["DUMMY", "{", "(", "[", "handle", "match", "if", "then"]

showErrorFancy :: (P.ShowErrorComponent e) => P.ErrorFancy e -> String
showErrorFancy = \case
  P.ErrorFail msg -> msg
  P.ErrorIndentation ord ref actual ->
    "incorrect indentation (got "
      <> show (P.unPos actual)
      <> ", should be "
      <> p
      <> show (P.unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  P.ErrorCustom a -> P.showErrorComponent a

lexer' :: String -> String -> Either (EP.ParseErrorBundle String (Token Err)) [Token Lexeme]
lexer' scope = flip S.evalState env0 . P.runParserT (lexemes eof) scope
  where
    eof :: P [Token Lexeme]
    eof = P.try do
      p <- P.eof >> posP
      n <- maybe 0 (const 1) <$> S.gets opening
      l <- S.gets layout
      pure $ replicate (length l + n) (Token Close p p)
    env0 = initialEnv scope

lexer :: String -> String -> [Token Lexeme]
lexer scope rem =
  case lexer' scope rem of
    Left e ->
      let errsWithSourcePos =
            fst $
              P.attachSourcePos
                P.errorOffset
                (toList (P.bundleErrors e))
                (P.bundlePosState e)
          errorToTokens :: (EP.ParseError String (Token Err), P.SourcePos) -> [Token Lexeme]
          errorToTokens (err, top) = case err of
            P.FancyError _ (customErrs -> es) | not (null es) -> es
            P.FancyError _errOffset es ->
              let msg = intercalateMap "\n" showErrorFancy es
               in [Token (Err (UnexpectedTokens msg)) (toPos top) (toPos top)]
            P.TrivialError _errOffset mayUnexpectedTokens expectedTokens ->
              let unexpectedStr :: Set String
                  unexpectedStr =
                    mayUnexpectedTokens
                      & fmap errorItemToString
                      & maybeToList
                      & Set.fromList
                  errorLength :: Int
                  errorLength = case Set.toList unexpectedStr of
                    [] -> 0
                    (x : _) -> length x
                  expectedStr :: Set String
                  expectedStr =
                    expectedTokens
                      & Set.map errorItemToString
                  err = UnexpectedTokens $ formatTrivialError unexpectedStr expectedStr
                  startPos = toPos top
                  -- This is just an attempt to highlight errors better in source excerpts.
                  -- It may not work in all cases, but should generally provide a better experience.
                  endPos = startPos & \(Pos l c) -> Pos l (c + errorLength)
               in [Token (Err err) startPos endPos]
       in errsWithSourcePos >>= errorToTokens
    Right ts -> postLex $ Token (Open scope) topLeftCorner topLeftCorner : ts
  where
    errorItemToString :: EP.ErrorItem Char -> String
    errorItemToString = \case
      (P.Tokens ts) -> Foldable.toList ts
      (P.Label ts) -> Foldable.toList ts
      (P.EndOfInput) -> "end of input"
    customErrs es = [Err <$> e | P.ErrorCustom e <- toList es]
    toPos (P.SourcePos _ line col) = Pos (P.unPos line) (P.unPos col)

-- | hacky postprocessing pass to do some cleanup of stuff that's annoying to
-- fix without adding more state to the lexer:
--   - 1+1 lexes as [1, +1], convert this to [1, +, 1]
--   - when a semi followed by a virtual semi, drop the virtual, lets you
--     write
--       foo x = action1;
--               2
--   - semi immediately after first Open is ignored
tweak :: (Token Lexeme) -> [Token Lexeme] -> [Token Lexeme]
tweak h@(Token (Semi False) _ _) (Token (Semi True) _ _ : t) = h : t
-- __NB__: This case only exists to guard against the following one
tweak h@(Token (Reserved _) _ _) t = h : t
tweak t1 (t2@(Token (Numeric num) _ _) : rem)
  | notLayout t1 && touches t1 t2 && isSigned num =
      t1
        : Token
          (SymbolyId (HQ'.fromName (Name.unsafeParseText (Text.pack (take 1 num)))))
          (start t2)
          (inc $ start t2)
        : Token (Numeric (drop 1 num)) (inc $ start t2) (end t2)
        : rem
  where
    isSigned num = all (\ch -> ch == '-' || ch == '+') $ take 1 num
tweak h t = h : t

formatTrivialError :: Set String -> Set String -> [Char]
formatTrivialError unexpectedTokens expectedTokens =
  let unexpectedMsg = case Set.toList unexpectedTokens of
        [] -> "I found something I didn't expect."
        [x] ->
          let article = case x of
                (c : _) | c `elem` ("aeiou" :: String) -> "an"
                _ -> "a"
           in "I was surprised to find " <> article <> " " <> x <> " here."
        xs -> "I was surprised to find these:\n\n* " <> List.intercalate "\n* " xs
      expectedMsg = case Set.toList expectedTokens of
        [] -> Nothing
        xs -> Just $ "\nI was expecting one of these instead:\n\n* " <> List.intercalate "\n* " xs
   in concat $ catMaybes [Just unexpectedMsg, expectedMsg]

displayLexeme :: Lexeme -> String
displayLexeme = \case
  Open o -> o
  Semi True -> "end of stanza"
  Semi False -> "semicolon"
  Close -> "end of section"
  Reserved r -> "'" <> r <> "'"
  Textual t -> "\"" <> t <> "\""
  Character c -> "?" <> [c]
  WordyId hq -> Text.unpack (HQ'.toTextWith Name.toText hq)
  SymbolyId hq -> Text.unpack (HQ'.toTextWith Name.toText hq)
  Numeric n -> n
  Bytes _b -> "bytes literal"
  Hash h -> Text.unpack (SH.toText h)
  Err e -> show e
  Doc _ -> "doc structure"

-- | The `Doc` lexer as documented on unison-lang.org
doc2 :: P [Token Lexeme]
doc2 = do
  -- Ensure we're at a doc before we start consuming tokens
  P.lookAhead (lit "{{")
  openStart <- posP
  -- Produce any layout tokens, such as closing the last open block or virtual semicolons
  -- We don't use 'token' on "{{" directly because we don't want to duplicate layout
  -- tokens if we do the rewrite hack for type-docs below.
  beforeStartToks <- token' ignore (pure ())
  void $ lit "{{"
  openEnd <- posP
  CP.space
  env0 <- S.get
  -- Disable layout while parsing the doc block and reset the section number
  (docTok, closeTok) <- local
    (\env -> env {inLayout = False})
    do
      body <- Doc.doc (tokenP typeOrTerm) lexemes' . P.lookAhead $ lit "}}"
      closeStart <- posP
      lit "}}"
      closeEnd <- posP
      pure (Token (Doc body) openStart closeEnd, Token Close closeStart closeEnd)
  -- Parse any layout tokens after the doc block, e.g. virtual semicolon
  endToks <- token' ignore (pure ())
  -- Hack to allow anonymous doc blocks before type decls
  --   {{ Some docs }}             Foo.doc = {{ Some docs }}
  --   ability Foo where      =>   ability Foo where
  --
  -- __FIXME__: This should be done _after_ parsing, not in lexing.
  tn <- subsequentTypeName
  pure $
    beforeStartToks <> case (tn) of
      -- If we're followed by a type, we rewrite the doc block to be a named doc block.
      Just (WordyId tname)
        | isTopLevel ->
            Token (WordyId (HQ'.fromName (Name.snoc (HQ'.toName tname) NameSegment.docSegment))) openStart openEnd
              : Token (Open "=") openStart openEnd
              : docTok
              -- We need an extra 'Close' here because we added an extra Open above.
              : closeTok
              : endToks
        where
          isTopLevel = length (layout env0) + maybe 0 (const 1) (opening env0) == 1
      _ -> docTok : endToks
  where
    subsequentTypeName = P.lookAhead . P.optional $ do
      let lit' s = lit s <* sp
      let modifier = typeModifiersAlt (lit' . Text.unpack)
      _ <- optional modifier *> typeOrAbility' *> sp
      Token name start stop <- tokenP identifierP
      if Name.isSymboly (HQ'.toName name)
        then P.customFailure (Token (InvalidSymbolyId (Text.unpack (HQ'.toTextWith Name.toText name))) start stop)
        else pure (WordyId name)
    ignore _ _ _ = []
    -- DUPLICATED
    sp = P.try $ do
      spaces <- P.takeWhile1P (Just "space") isSpace
      close <- P.optional (P.lookAhead (lit "}}"))
      case close of
        Nothing -> guard $ ok spaces
        Just _ -> pure ()
      pure spaces
      where
        ok s = length [() | '\n' <- s] < 2

typeOrTerm :: (Monad m) => P.ParsecT (Token Err) String m (ReferenceType, HQ'.HashQualified Name)
typeOrTerm = do
  mtype <- P.optional $ typeOrAbility' <* CP.space
  ident <- identifierP <* CP.space
  pure (maybe RtTerm (const RtType) mtype, ident)

typeOrAbility' :: (Monad m) => P.ParsecT (Token Err) String m String
typeOrAbility' = typeOrAbilityAlt (wordyKw . Text.unpack)
  where
    wordyKw kw = separated wordySep (lit kw)

lexemes' :: P () -> P [Token Lexeme]
lexemes' eof =
  -- NB: `postLex` requires the token stream to start with an `Open`, otherwise it can’t create a `BlockTree`, so this
  --     adds one, runs `postLex`, then removes it.
  fmap (tail . postLex . (Token (Open "fake") mempty mempty :)) $
    local (const $ initialEnv "DUMMY") do
      p <- lexemes $ [] <$ eof
      -- deals with a final "unclosed" block at the end of `p`)
      unclosed <- takeWhile (("DUMMY" /=) . fst) . layout <$> S.get
      finalPos <- posP
      pure $ p <> replicate (length unclosed) (Token Close finalPos finalPos)

-- | Consumes an entire Unison “module”.
lexemes :: P [Token Lexeme] -> P [Token Lexeme]
lexemes eof =
  P.optional space >> do
    hd <- join <$> P.manyTill toks (P.lookAhead eof)
    tl <- eof
    pure $ hd <> tl
  where
    toks :: P [Token Lexeme]
    toks =
      doc2
        <|> doc
        <|> token numeric
        <|> token character
        <|> reserved
        <|> token identifierLexemeP
        <|> (asum . map token) [semi, textual, hash]

    doc :: P [Token Lexeme]
    doc = open <+> (CP.space *> fmap fixup body) <+> (close <* space)
      where
        open = token'' (\t _ _ -> t) $ tok (Open <$> lit "[:")
        close = tok (Close <$ lit ":]")
        at = lit "@"
        -- this removes some trailing whitespace from final textual segment
        fixup [] = []
        fixup (Token (Textual (reverse -> txt)) start stop : []) =
          [Token (Textual txt') start stop]
          where
            txt' = reverse (dropWhile (\c -> isSpace c && not (c == '\n')) txt)
        fixup (h : t) = h : fixup t

        body :: P [Token Lexeme]
        body = txt <+> (atk <|> pure [])
          where
            ch = (":]" <$ lit "\\:]") <|> ("@" <$ lit "\\@") <|> (pure <$> P.anySingle)
            txt = tok (Textual . join <$> P.manyTill ch (P.lookAhead sep))
            sep = void at <|> void close
            ref = at *> (tok identifierLexemeP <|> docTyp)
            atk = (ref <|> docTyp) <+> body
            docTyp = do
              _ <- lit "["
              typ <- tok (P.manyTill P.anySingle (P.lookAhead (lit "]")))
              _ <- lit "]" *> CP.space
              t <- tok identifierLexemeP
              pure $ (fmap Reserved <$> typ) <> t

    semi = char ';' $> Semi False
    textual = Textual <$> quoted
    quoted = quotedRaw <|> quotedSingleLine
    quotedRaw = do
      _ <- lit "\"\"\""
      n <- many (char '"')
      _ <- optional (char '\n') -- initial newline is skipped
      s <- P.manyTill P.anySingle (lit (replicate (length n + 3) '"'))
      col0 <- column <$> posP
      let col = col0 - (length n) - 3 -- this gets us first col of closing quotes
      let leading = replicate (max 0 (col - 1)) ' '
      -- a last line that's equal to `leading` is ignored, since leading
      -- spaces up to `col` are not considered part of the string
      let tweak l = case reverse l of
            last : rest
              | col > 1 && last == leading -> reverse rest
              | otherwise -> l
            [] -> []
      pure $ case tweak (lines s) of
        [] -> s
        ls
          | all (\l -> List.isPrefixOf leading l || all isSpace l) ls -> List.intercalate "\n" (drop (length leading) <$> ls)
          | otherwise -> s
    quotedSingleLine = char '"' *> P.manyTill (LP.charLiteral <|> sp) (char '"')
      where
        sp = lit "\\s" $> ' '
    character = Character <$> (char '?' *> (spEsc <|> LP.charLiteral))
      where
        spEsc = P.try (char '\\' *> char 's' $> ' ')

    numeric = bytes <|> otherbase <|> float <|> intOrNat
      where
        intOrNat = P.try $ num <$> sign <*> LP.decimal
        float = do
          _ <- P.try (P.lookAhead (sign >> (LP.decimal :: P Int) >> (char '.' <|> char 'e' <|> char 'E'))) -- commit after this
          start <- posP
          sign <- fromMaybe "" <$> sign
          base <- P.takeWhile1P (Just "base") isDigit
          decimals <-
            P.optional $
              let missingFractional = err start (MissingFractional $ base <> ".")
               in liftA2 (<>) (lit ".") (P.takeWhile1P (Just "decimals") isDigit <|> missingFractional)
          exp <- P.optional $ do
            e <- map toLower <$> (lit "e" <|> lit "E")
            sign <- fromMaybe "" <$> optional (lit "+" <|> lit "-")
            let missingExp = err start (MissingExponent $ base <> fromMaybe "" decimals <> e <> sign)
            exp <- P.takeWhile1P (Just "exponent") isDigit <|> missingExp
            pure $ e <> sign <> exp
          pure $ Numeric (sign <> base <> fromMaybe "" decimals <> fromMaybe "" exp)

        bytes = do
          start <- posP
          _ <- lit "0xs"
          s <- map toLower <$> P.takeWhileP (Just "hexidecimal character") isAlphaNum
          case Bytes.fromBase16 $ Bytes.fromWord8s (fromIntegral . ord <$> s) of
            Left _ -> err start (InvalidBytesLiteral $ "0xs" <> s)
            Right bs -> pure (Bytes bs)
        otherbase = octal <|> hex <|> binary
        octal = do
          start <- posP
          commitAfter2 sign (lit "0o") $ \sign _ ->
            fmap (num sign) LP.octal <|> err start InvalidOctalLiteral
        hex = do
          start <- posP
          commitAfter2 sign (lit "0x") $ \sign _ ->
            fmap (num sign) LP.hexadecimal <|> err start InvalidHexLiteral
        binary = do
          start <- posP
          commitAfter2 sign (lit "0b") $ \sign _ ->
            fmap (num sign) LP.binary <|> err start InvalidBinaryLiteral

        num :: Maybe String -> Integer -> Lexeme
        num sign n = Numeric (fromMaybe "" sign <> show n)
        sign = P.optional (lit "+" <|> lit "-")

    hash = Hash <$> P.try shortHashP

    reserved :: P [Token Lexeme]
    reserved =
      token' (\ts _ _ -> ts) $
        braces
          <|> parens
          <|> brackets
          <|> commaSeparator
          <|> delim
          <|> delayOrForce
          <|> keywords
          <|> layoutKeywords
      where
        keywords =
          -- yes "wordy" - just like a wordy keyword like "true", the literal "." (as in the dot in
          -- "forall a. a -> a") is considered the keyword "." so long as it is either followed by EOF, a space, or some
          -- non-wordy character (because ".foo" is a single identifier lexeme)
          wordyKw "."
            <|> symbolyKw ":"
            <|> openKw "@rewrite"
            <|> symbolyKw "@"
            <|> symbolyKw "||"
            <|> symbolyKw "|"
            <|> symbolyKw "&&"
            <|> wordyKw "true"
            <|> wordyKw "false"
            <|> wordyKw "namespace"
            <|> wordyKw "use"
            <|> wordyKw "forall"
            <|> wordyKw "∀"
            <|> wordyKw "termLink"
            <|> wordyKw "typeLink"

        wordyKw s = separated wordySep (kw s)
        symbolyKw s = separated (not . symbolyIdChar) (kw s)

        kw :: String -> P [Token Lexeme]
        kw s = tokenP (lit s) <&> \token -> [Reserved <$> token]

        layoutKeywords :: P [Token Lexeme]
        layoutKeywords =
          ifElse
            <|> withKw
            <|> openKw "match"
            <|> openKw "handle"
            <|> typ
            <|> arr
            <|> rewriteArr
            <|> eq
            <|> openKw "cases"
            <|> openKw "where"
            <|> openKw "let"
            <|> openKw "do"
          where
            ifElse =
              openKw "if"
                <|> closeKw' (Just "then") ["if"] (lit "then")
                <|> closeKw' (Just "else") ["then"] (lit "else")
            modKw = typeModifiersAlt (openKw1 wordySep . Text.unpack)
            typeOrAbilityKw = typeOrAbilityAlt (openTypeKw1 . Text.unpack)
            typ = modKw <|> typeOrAbilityKw

            withKw = do
              [Token _ pos1 pos2] <- wordyKw "with"
              env <- S.get
              let l = layout env
              case findClose ["handle", "match"] l of
                Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen "'with'")
                  where
                    msgOpen = "'handle' or 'match'"
                Just (withBlock, n) -> do
                  let b = withBlock <> "-with"
                  S.put (env {layout = drop n l, opening = Just b})
                  let opens = [Token (Open "with") pos1 pos2]
                  pure $ replicate n (Token Close pos1 pos2) ++ opens

            -- In `structural/unique type` and `structural/unique ability`,
            -- only the `structural` or `unique` opens a layout block,
            -- and `ability` and `type` are just keywords.
            openTypeKw1 t = do
              b <- S.gets (topBlockName . layout)
              case b of
                Just mod | Set.member (Text.pack mod) typeModifiers -> wordyKw t
                _ -> openKw1 wordySep t

            -- layout keyword which bumps the layout column by 1, rather than looking ahead
            -- to the next token to determine the layout column
            openKw1 :: (Char -> Bool) -> String -> P [Token Lexeme]
            openKw1 sep kw = do
              Token kw pos0 pos1 <- tokenP $ separated sep (lit kw)
              S.modify (\env -> env {layout = (kw, column $ inc pos0) : layout env})
              pure [Token (Open kw) pos0 pos1]

            eq = do
              [Token _ start end] <- symbolyKw "="
              env <- S.get
              case topBlockName (layout env) of
                -- '=' does not open a layout block if within a type declaration
                Just t | t == "type" || Set.member (Text.pack t) typeModifiers -> pure [Token (Reserved "=") start end]
                Just _ -> S.put (env {opening = Just "="}) >> pure [Token (Open "=") start end]
                _ -> err start LayoutError

            rewriteArr = do
              [Token _ start end] <- symbolyKw "==>"
              env <- S.get
              S.put (env {opening = Just "==>"}) >> pure [Token (Open "==>") start end]

            arr = do
              [Token _ start end] <- symbolyKw "->"
              env <- S.get
              -- -> introduces a layout block if we're inside a `match with` or `cases`
              case topBlockName (layout env) of
                Just match | match `elem` matchWithBlocks -> do
                  S.put (env {opening = Just "->"})
                  pure [Token (Open "->") start end]
                _ -> pure [Token (Reserved "->") start end]

        -- a bit of lookahead here to reserve }} for closing a documentation block
        braces = open "{" <|> close ["{"] p
          where
            p = do
              l <- lit "}"
              -- if we're within an existing {{ }} block, inLayout will be false
              -- so we can actually allow }} to appear in normal code
              inLayout <- S.gets inLayout
              when (not inLayout) $ void $ P.lookAhead (P.satisfy (/= '}'))
              pure l
        matchWithBlocks = ["match-with", "cases"]
        parens = open "(" <|> close ["("] (lit ")")
        brackets = open "[" <|> close ["["] (lit "]")
        -- `allowCommaToClose` determines if a comma should close inner blocks.
        -- Currently there is a set of blocks where `,` is not treated specially
        -- and it just emits a Reserved ",". There are currently only three:
        -- `cases`, `match-with`, and `{`
        allowCommaToClose match = not $ match `elem` ("{" : matchWithBlocks)
        commaSeparator = do
          env <- S.get
          case topBlockName (layout env) of
            Just match
              | allowCommaToClose match ->
                  blockDelimiter ["[", "("] (lit ",")
            _ -> fail "this comma is a pattern separator"

        delim = P.try $ do
          ch <- P.satisfy (\ch -> ch /= ';' && Set.member ch delimiters)
          pos <- posP
          pure [Token (Reserved [ch]) pos (inc pos)]

        delayOrForce = separated ok $ do
          token <- tokenP $ P.satisfy isDelayOrForce
          pure [token <&> \op -> Reserved [op]]
          where
            ok c = isDelayOrForce c || isSpace c || isAlphaNum c || Set.member c delimiters || c == '\"'

open :: String -> P [Token Lexeme]
open b = do
  token <- tokenP $ lit b
  env <- S.get
  S.put (env {opening = Just b})
  pure [Open b <$ token]

openKw :: String -> P [Token Lexeme]
openKw s = separated wordySep $ do
  token <- tokenP $ lit s
  env <- S.get
  S.put (env {opening = Just s})
  pure [Open <$> token]

tok :: P a -> P [Token a]
tok p = do
  token <- tokenP p
  pure [token]

-- An identifier is a non-empty dot-delimited list of segments, with an optional leading dot, where each segment is
-- symboly (comprised of only symbols) or wordy (comprised of only alphanums).
--
-- Examples:
--
--   foo
--   .foo.++.doc
--   `.`.`..`     (This is a two-segment identifier without a leading dot: "." then "..")
identifierP :: (Monad m) => P.ParsecT (Token Err) String m (HQ'.HashQualified Name)
identifierP = do
  P.label "identifier (ex: abba1, snake_case, .foo.bar#xyz, .foo.++#xyz, or 🌻)" do
    name <- PI.withParsecT (fmap nameSegmentParseErrToErr) Name.nameP
    P.optional shortHashP <&> \case
      Nothing -> HQ'.fromName name
      Just shorthash -> HQ'.HashQualified name shorthash
  where
    nameSegmentParseErrToErr :: NameSegment.ParseErr -> Err
    nameSegmentParseErrToErr = \case
      NameSegment.ReservedOperator s -> ReservedSymbolyId (Text.unpack s)
      NameSegment.ReservedWord s -> ReservedWordyId (Text.unpack s)

-- An identifier is a non-empty dot-delimited list of segments, with an optional leading dot, where each segment is
-- symboly (comprised of only symbols) or wordy (comprised of only alphanums).
--
-- Examples:
--
--   foo
--   .foo.++.doc
--   `.`.`..`     (This is a two-segment identifier without a leading dot: "." then "..")
identifierLexemeP :: P Lexeme
identifierLexemeP = identifierLexeme <$> identifierP

identifierLexeme :: HQ'.HashQualified Name -> Lexeme
identifierLexeme name =
  if Name.isSymboly (HQ'.toName name)
    then SymbolyId name
    else WordyId name

shortHashP :: P.ParsecT (Token Err) String m ShortHash
shortHashP =
  PI.withParsecT (fmap (InvalidShortHash . Text.unpack)) ShortHash.shortHashP

blockDelimiter :: [String] -> P String -> P [Token Lexeme]
blockDelimiter open closeP = do
  Token close pos1 pos2 <- tokenP closeP
  env <- S.get
  case findClose open (layout env) of
    Nothing -> err pos1 (UnexpectedDelimiter (quote close))
      where
        quote s = "'" <> s <> "'"
    Just (_, n) -> do
      S.put (env {layout = drop (n - 1) (layout env)})
      let delims = [Token (Reserved close) pos1 pos2]
      pure $ replicate (n - 1) (Token Close pos1 pos2) ++ delims

close :: [String] -> P String -> P [Token Lexeme]
close = close' Nothing

closeKw' :: Maybe String -> [String] -> P String -> P [Token Lexeme]
closeKw' reopenBlockname open closeP = close' reopenBlockname open (separated wordySep closeP)

close' :: Maybe String -> [String] -> P String -> P [Token Lexeme]
close' reopenBlockname open closeP = do
  Token close pos1 pos2 <- tokenP closeP
  env <- S.get
  case findClose open (layout env) of
    Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen (quote close))
      where
        msgOpen = List.intercalate " or " (quote <$> open)
        quote s = "'" <> s <> "'"
    Just (_, n) -> do
      S.put (env {layout = drop n (layout env), opening = reopenBlockname})
      let opens = maybe [] (const $ [Token (Open close) pos1 pos2]) reopenBlockname
      pure $ replicate n (Token Close pos1 pos2) ++ opens

findClose :: [String] -> Layout -> Maybe (String, Int)
findClose _ [] = Nothing
findClose s ((h, _) : tl) = if h `elem` s then Just (h, 1) else fmap (1 +) <$> findClose s tl

notLayout :: Token Lexeme -> Bool
notLayout t = case payload t of
  Close -> False
  Semi _ -> False
  Open _ -> False
  _ -> True

top :: Layout -> Column
top [] = 1
top ((_, h) : _) = h

-- todo: make Layout a NonEmpty
topBlockName :: Layout -> Maybe BlockName
topBlockName [] = Nothing
topBlockName ((name, _) : _) = Just name

topLeftCorner :: Pos
topLeftCorner = Pos 1 1

data BlockTree a
  = Block
      -- | The token that opens the block
      a
      -- | “Stanzas” of nested tokens
      [[BlockTree a]]
      -- | The closing token, if any
      (Maybe a)
  | Leaf a
  deriving (Functor, Foldable, Traversable)

headToken :: BlockTree a -> a
headToken (Block a _ _) = a
headToken (Leaf a) = a

instance (Show a) => Show (BlockTree a) where
  showsPrec = showsPrec1

-- | This instance should be compatible with `Read`, but inserts newlines and indentation to make it more
--  /human/-readable.
instance Show1 BlockTree where
  liftShowsPrec spa sla = shows ""
    where
      shows by prec =
        showParen (prec > appPrec) . \case
          Leaf a -> showString "Leaf " . showsNext spa "" a
          Block open mid close ->
            showString "Block "
              . showsNext spa "" open
              . showString "\n"
              . showIndentedList (showIndentedList (\b -> showsIndented (shows b 0) b)) ("  " <> by) mid
              . showString "\n"
              . showsNext (liftShowsPrec spa sla) ("  " <> by) close
      appPrec = 10
      showsNext :: (Int -> x -> ShowS) -> String -> x -> ShowS
      showsNext fn = showsIndented (fn $ appPrec + 1)
      showsIndented :: (x -> ShowS) -> String -> x -> ShowS
      showsIndented fn by x = showString by . fn x
      showIndentedList :: (String -> x -> ShowS) -> String -> [x] -> ShowS
      showIndentedList fn by xs =
        showString by
          . showString "["
          . foldr (\x acc -> showString "\n" . fn ("  " <> by) x . showString "," . acc) id xs
          . showString "\n"
          . showString by
          . showString "]"

reorderTree :: ([[BlockTree a]] -> [[BlockTree a]]) -> BlockTree a -> BlockTree a
reorderTree f (Block open mid close) = Block open (f (fmap (reorderTree f) <$> mid)) close
reorderTree _ l = l

tree :: [Token Lexeme] -> BlockTree (Token Lexeme)
tree toks = one toks const
  where
    one (open@(payload -> Open _) : ts) k = many (Block open . stanzas) [] ts k
    one (t : ts) k = k (Leaf t) ts
    one [] k = k lastErr []
      where
        lastErr = Leaf case drop (length toks - 1) toks of
          [] -> Token (Err LayoutError) topLeftCorner topLeftCorner
          (t : _) -> t {payload = Err LayoutError}

    many open acc [] k = k (open (reverse acc) Nothing) []
    many open acc (t@(payload -> Close) : ts) k = k (open (reverse acc) $ pure t) ts
    many open acc ts k = one ts $ \t ts -> many open (t : acc) ts k

stanzas :: [BlockTree (Token Lexeme)] -> [[BlockTree (Token Lexeme)]]
stanzas =
  toList
    . foldr
      ( \tok (curr :| stanzas) -> case tok of
          Leaf (Token (Semi _) _ _) -> [tok] :| curr : stanzas
          _ -> (tok : curr) :| stanzas
      )
      ([] :| [])

-- Moves type and ability declarations to the front of the token stream (but not before the leading optional namespace
-- directive) and move `use` statements to the front of each block
reorder :: [[BlockTree (Token Lexeme)]] -> [[BlockTree (Token Lexeme)]]
reorder = foldr fixup [] . sortWith f
  where
    f [] = 4 :: Int
    f (t0 : _) = case payload $ headToken t0 of
      Open mod | Set.member (Text.pack mod) typeModifiers -> 3
      Open typOrA | Set.member (Text.pack typOrA) typeOrAbility -> 3
      -- put `namespace` before `use` because the file parser only accepts a namespace directive at the top of the file
      Reserved "namespace" -> 1
      Reserved "use" -> 2
      _ -> 4 :: Int
    -- after reordering can end up with trailing semicolon at the end of
    -- a block, which we remove with this pass
    fixup stanza [] = case Lens.unsnoc stanza of
      Nothing -> []
      -- remove any trailing `Semi` from the last non-empty stanza
      Just (init, Leaf (Token (Semi _) _ _)) -> [init]
      -- don’t touch other stanzas
      Just (_, _) -> [stanza]
    fixup stanza tail = stanza : tail

-- | This turns the lexeme stream into a tree, reordering some lexeme subsequences.
preParse :: [Token Lexeme] -> BlockTree (Token Lexeme)
preParse = reorderTree reorder . tree

-- | A few transformations that happen between lexing and parsing.
--
--   All of these things should move out of the lexer, and be applied in the parse.
postLex :: [Token Lexeme] -> [Token Lexeme]
postLex = toList . preParse . foldr tweak []

isDelayOrForce :: Char -> Bool
isDelayOrForce op = op == '\'' || op == '!'

-- Mapping between characters and their escape codes. Use parse/showEscapeChar to convert.
escapeChars :: [(Char, Char)]
escapeChars =
  [ ('0', '\0'),
    ('a', '\a'),
    ('b', '\b'),
    ('f', '\f'),
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('v', '\v'),
    ('s', ' '),
    ('\'', '\''),
    ('"', '"'),
    ('\\', '\\')
  ]

-- Inverse of parseEscapeChar; map a character to its escaped version:
showEscapeChar :: Char -> Maybe Char
showEscapeChar c =
  Map.lookup c (Map.fromList [(x, y) | (y, x) <- escapeChars])

typeModifiersAlt :: (Alternative f) => (Text -> f a) -> f a
typeModifiersAlt f =
  asum $ map f (toList typeModifiers)

debugFilePreParse :: FilePath -> IO ()
debugFilePreParse file = putStrLn . debugPreParse . preParse . lexer file . Text.unpack =<< readUtf8 file

debugPreParse :: BlockTree (Token Lexeme) -> String
debugPreParse (Leaf (Token (Err (UnexpectedTokens msg)) start end)) =
  (if start == end then msg1 else msg2) <> ":\n" <> msg
  where
    msg1 = "Error on line " <> show (line start) <> ", column " <> show (column start)
    msg2 =
      "Error on line "
        <> show (line start)
        <> ", column "
        <> show (column start)
        <> " - line "
        <> show (line end)
        <> ", column "
        <> show (column end)
debugPreParse ts = show $ payload <$> ts

debugPreParse' :: String -> String
debugPreParse' = debugPreParse . preParse . lexer "debugPreParse"

instance EP.ShowErrorComponent (Token Err) where
  showErrorComponent (Token err _ _) = go err
    where
      go = \case
        UnexpectedTokens msg -> msg
        CloseWithoutMatchingOpen open close -> "I found a closing " <> close <> " but no matching " <> open <> "."
        Both e1 e2 -> go e1 <> "\n" <> go e2
        LayoutError -> "Indentation error"
        TextLiteralMissingClosingQuote s -> "This text literal missing a closing quote: " <> excerpt s
        e -> show e
      excerpt s = if length s < 15 then s else take 15 s <> "..."

instance P.VisualStream [Token Lexeme] where
  showTokens _ xs =
    join . Nel.toList . S.evalState (traverse go xs) . end $ Nel.head xs
    where
      go :: Token Lexeme -> S.State Pos String
      go tok = do
        prev <- S.get
        S.put $ end tok
        pure $ pad prev (start tok) ++ pretty (payload tok)
      pretty (Open s) = s
      pretty (Reserved w) = w
      pretty (Textual t) = '"' : t ++ ['"']
      pretty (Character c) =
        case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c]
      pretty (WordyId n) = Text.unpack (HQ'.toText n)
      pretty (SymbolyId n) = Text.unpack (HQ'.toText n)
      pretty (Numeric n) = n
      pretty (Hash sh) = show sh
      pretty (Err e) = show e
      pretty (Bytes bs) = "0xs" <> show bs
      pretty Close = "<outdent>"
      pretty (Semi True) = "<virtual semicolon>"
      pretty (Semi False) = ";"
      pretty (Doc d) = show d
      pad (Pos line1 col1) (Pos line2 col2) =
        if line1 == line2
          then replicate (col2 - col1) ' '
          else replicate (line2 - line1) '\n' ++ replicate col2 ' '
