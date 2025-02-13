{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Syntax.TermParser
  ( binding,
    blockTerm,
    doc2Block,
    imports,
    lam,
    substImports,
    term,
    verifyRelativeVarName,
  )
where

import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Control.Monad.Reader (asks, local)
import Data.Bitraversable (bitraverse)
import Data.Char qualified as Char
import Data.Foldable (foldrM)
import Data.List qualified as List
import Data.List.Extra qualified as List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Sequence
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple.Extra qualified as TupleE
import Text.Megaparsec qualified as P
import U.Codebase.Reference (ReferenceType (..))
import U.Core.ABT qualified as ABT
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult (ResolutionError (..), ResolutionFailure (..))
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (Ann))
import Unison.Parser.Ann qualified as Ann
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Syntax.Lexer.Unison qualified as L
import Unison.Syntax.Name qualified as Name (toText, toVar, unsafeParseVar)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.Parser hiding (seq)
import Unison.Syntax.Parser qualified as Parser (seq, uniqueName)
import Unison.Syntax.Parser.Doc.Data qualified as Doc
import Unison.Syntax.Precedence (operatorPrecedence)
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Term (IsTop, Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.Components qualified as Components
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.List (intercalateMapWith, quenchRuns)
import Unison.Util.Recursion
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (and, or, seq)

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence (with a few exceptions), and operators
must have surrounding whitespace (a + b, not a+b) to distinguish from
identifiers that may contain operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type TermP v m = P v m (Term v Ann)

term :: (Monad m, Var v) => TermP v m
term = term2

term2 :: (Monad m, Var v) => TermP v m
term2 = lam term2 <|> term3

term3 :: (Monad m, Var v) => TermP v m
term3 = do
  t <- infixAppOrBooleanOp
  ot <- optional (reserved ":" *> TypeParser.computationType)
  pure case ot of
    Nothing -> t
    Just y -> Term.ann (mkAnn t y) t y

keywordBlock :: (Monad m, Var v) => TermP v m
keywordBlock = letBlock <|> handle <|> ifthen <|> match <|> lamCase <|> rewriteBlock

rewriteBlock :: (Monad m, Var v) => TermP v m
rewriteBlock = do
  t <- openBlockWith "@rewrite"
  elements <- sepBy semi (rewriteTerm <|> rewriteCase <|> rewriteType)
  b <- closeBlock
  pure (DD.rewrites (ann t <> ann b) elements)
  where
    rewriteTermlike kw mk = do
      kw <- quasikeyword kw
      lhs <- term
      (_spanAnn, rhs) <- layoutBlock "==>"
      pure (mk (ann kw <> ann rhs) lhs rhs)
    rewriteTerm = rewriteTermlike "term" DD.rewriteTerm
    rewriteCase = rewriteTermlike "case" DD.rewriteCase
    rewriteType = do
      kw <- quasikeyword "signature"
      vs <- P.try (some prefixDefinitionName <* reserved ".") <|> pure []
      lhs <- TypeParser.computationType
      rhs <- openBlockWith "==>" *> TypeParser.computationType <* closeBlock
      pure (DD.rewriteType (ann kw <> ann rhs) (L.payload <$> vs) lhs rhs)

typeLink' :: (Monad m, Var v) => P v m (L.Token TypeReference)
typeLink' = findUniqueType =<< hqPrefixId

findUniqueType :: (Monad m, Var v) => L.Token (HQ.HashQualified Name) -> P v m (L.Token TypeReference)
findUniqueType id =
  resolveToLocalNamespacedType id >>= \case
    Nothing -> do
      ns <- asks names
      case Names.lookupHQType Names.IncludeSuffixes (L.payload id) ns of
        s
          | Set.size s == 1 -> pure (Set.findMin s <$ id)
          | otherwise -> customFailure $ UnknownType id s
    Just ref -> pure (ref <$ id)

termLink' :: (Monad m, Var v) => P v m (L.Token Referent)
termLink' = do
  id <- hqPrefixId
  ns <- asks names
  case Names.lookupHQTerm Names.IncludeSuffixes (L.payload id) ns of
    s
      | Set.size s == 1 -> pure $ const (Set.findMin s) <$> id
      | otherwise -> customFailure $ UnknownTerm id s

link' :: (Monad m, Var v) => P v m (Either (L.Token TypeReference) (L.Token Referent))
link' = do
  id <- hqPrefixId
  ns <- asks names
  let s = Names.lookupHQTerm Names.IncludeSuffixes (L.payload id) ns
  let s2 = Names.lookupHQType Names.IncludeSuffixes (L.payload id) ns
  if
    | Set.size s == 1 && Set.null s2 -> pure . Right $ Set.findMin s <$ id
    | Set.size s2 == 1 && Set.null s -> pure . Left $ Set.findMin s2 <$ id
    | True -> customFailure $ UnknownId id s s2

link :: (Monad m, Var v) => TermP v m
link = termLink <|> typeLink
  where
    typeLink = do
      _ <- reserved "typeLink" -- type opens a block, gotta use something else
      tok <- typeLink'
      pure $ Term.typeLink (ann tok) (L.payload tok)
    termLink = do
      _ <- reserved "termLink"
      tok <- termLink'
      pure $ Term.termLink (ann tok) (L.payload tok)

resolveToLocalNamespacedType :: (Monad m, Ord v) => L.Token (HQ.HashQualified Name) -> P v m (Maybe TypeReference)
resolveToLocalNamespacedType tok =
  case L.payload tok of
    HQ.NameOnly name ->
      asks maybeNamespace >>= \case
        Nothing -> pure Nothing
        Just namespace -> do
          localNames <- asks localNamespacePrefixedTypesAndConstructors
          pure case Names.lookupHQType Names.ExactName (HQ.NameOnly (Name.joinDot namespace name)) localNames of
            refs
              | Set.null refs -> Nothing
              -- 2+ name case is impossible: we looked up exact names in the locally-bound names. Two bindings
              -- with the same name would have been a parse error. So, just take the minimum element from the set,
              -- which we know is a singleton.
              | otherwise -> Just (Set.findMin refs)
    _ -> pure Nothing

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: (Monad m, Var v) => TermP v m
blockTerm = lam term <|> infixAppOrBooleanOp

match :: (Monad m, Var v) => TermP v m
match = do
  start <- openBlockWith "match"
  scrutinee <- term
  _ <- optionalCloseBlock
  _ <-
    openBlockWith "with" <|> do
      t <- anyToken
      P.customFailure (ExpectedBlockOpen "with" t)
  (_arities, cases) <- unzip <$> matchCases
  _ <- optionalCloseBlock
  let anns = foldr ((<>) . ann) (ann start) $ lastMay cases
  pure $ Term.match anns scrutinee cases

matchCases :: (Monad m, Var v) => P v m [(Int, Term.MatchCase Ann (Term v Ann))]
matchCases = sepBy semi matchCase <&> \cases_ -> [(n, c) | (n, cs) <- cases_, c <- cs]

-- Returns the arity of the pattern and the `MatchCase`. Examples:
--
--   (a, b) -> a - b -- arity 1
--   foo, hd +: tl -> foo tl -- arity 2
--
-- Cases with arity greater than 1 are desugared to matching on tuples,
-- so the following are parsed the same:
--
--   42, x -> ...
--   (42, x) -> ...
matchCase :: (Monad m, Var v) => P v m (Int, [Term.MatchCase Ann (Term v Ann)])
matchCase = do
  pats <- sepBy1 (label "\",\"" $ reserved ",") parsePattern
  let boundVars' = [v | (_, vs) <- pats, (_ann, v) <- vs]
      pat = case fst <$> pats of
        [p] -> p
        pats -> foldr pair (unit (ann . last $ pats)) pats
      unit ann = Pattern.Constructor ann (ConstructorReference DD.unitRef 0) []
      pair p1 p2 = Pattern.Constructor (ann p1 <> ann p2) (ConstructorReference DD.pairRef 0) [p1, p2]
  let guardedBlocks = label "pattern guard" . some $ do
        _ <- reserved "|"
        guard <-
          asum
            [ Nothing <$ quasikeyword "otherwise",
              Just <$> infixAppOrBooleanOp
            ]
        (_spanAnn, t) <- layoutBlock "->"
        pure (guard, t)
  let unguardedBlock = label "case match" do
        (_spanAnn, t) <- layoutBlock "->"
        pure (Nothing, t)
  -- a pattern's RHS is either one or more guards, or a single unguarded block.
  guardsAndBlocks <- guardedBlocks <|> (pure @[] <$> unguardedBlock)
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
  let mk (guard, t) = Term.MatchCase pat (fmap (absChain boundVars') guard) (absChain boundVars' t)
  pure $ (length pats, mk <$> guardsAndBlocks)

parsePattern :: forall m v. (Monad m, Var v) => P v m (Pattern Ann, [(Ann, v)])
parsePattern = label "pattern" root
  where
    root = chainl1 patternCandidates patternInfixApp
    patternCandidates = constructor <|> leaf
    patternInfixApp ::
      P
        v
        m
        ( (Pattern Ann, [(Ann, v)]) ->
          (Pattern Ann, [(Ann, v)]) ->
          (Pattern Ann, [(Ann, v)])
        )
    patternInfixApp = f <$> seqOp
      where
        f op (l, lvs) (r, rvs) =
          (Pattern.SequenceOp (ann l <> ann r) l op r, lvs ++ rvs)

    -- note: nullaryCtor comes before var patterns, since (for better or worse)
    -- they can overlap (a variable could be called 'Foo' in the current grammar).
    -- This order treats ambiguous patterns as nullary constructors if there's
    -- a constructor with a matching name.
    leaf =
      literal
        <|> nullaryCtor
        <|> varOrAs
        <|> unbound
        <|> seqLiteral
        <|> parenthesizedOrTuplePattern
        <|> effect
    literal = (,[]) <$> asum [true, false, number, text, char]
    true = (\t -> Pattern.Boolean (ann t) True) <$> reserved "true"
    false = (\t -> Pattern.Boolean (ann t) False) <$> reserved "false"
    number =
      join $
        number'
          (pure . tok Pattern.Int)
          (pure . tok Pattern.Nat)
          (tok (const . failCommitted . FloatPattern))
    text = (\t -> Pattern.Text (ann t) (L.payload t)) <$> string
    char = (\c -> Pattern.Char (ann c) (L.payload c)) <$> character
    parenthesizedOrTuplePattern :: P v m (Pattern Ann, [(Ann, v)])
    parenthesizedOrTuplePattern = do
      (_spanAnn, (pat, pats)) <- tupleOrParenthesized parsePattern unit pair
      pure (pat, pats)
    unit ann = (Pattern.Constructor ann (ConstructorReference DD.unitRef 0) [], [])
    pair (p1, v1) (p2, v2) =
      ( Pattern.Constructor (ann p1 <> ann p2) (ConstructorReference DD.pairRef 0) [p1, p2],
        v1 ++ v2
      )
    -- Foo x@(Blah 10)
    varOrAs :: P v m (Pattern Ann, [(Ann, v)])
    varOrAs = do
      v <- wordyPatternName
      o <- optional (reserved "@")
      if isJust o
        then (\(p, vs) -> (Pattern.As (ann v) p, tokenToPair v : vs)) <$> leaf
        else pure (Pattern.Var (ann v), [tokenToPair v])
    unbound :: P v m (Pattern Ann, [(Ann, v)])
    unbound = (\tok -> (Pattern.Unbound (ann tok), [])) <$> blank
    ctor :: CT.ConstructorType -> P v m (L.Token ConstructorReference)
    ctor ct = do
      -- this might be a var, so we avoid consuming it at first
      tok <- P.lookAhead hqPrefixId

      -- First, if:
      --
      --   * The token isn't hash-qualified (e.g. "Foo.Bar")
      --   * We're under a namespace directive (e.g. "baz")
      --   * There's an exact match for a locally-bound constructor (e.g. "baz.Foo.Bar")
      --
      -- Then:
      --
      --   * Use that constructor reference (duh)
      --
      -- Else:
      --
      --   * Fall through to the normal logic of looking the constructor name up in all of the names (which includes
      --     the locally-bound constructors).

      maybeLocalCtor <-
        case L.payload tok of
          HQ.NameOnly name ->
            asks maybeNamespace >>= \case
              Nothing -> pure Nothing
              Just namespace -> do
                localNames <- asks localNamespacePrefixedTypesAndConstructors
                case Names.lookupHQPattern Names.ExactName (HQ.NameOnly (Name.joinDot namespace name)) ct localNames of
                  refs
                    | Set.null refs -> pure Nothing
                    -- 2+ name case is impossible: we looked up exact names in the locally-bound names. Two bindings
                    -- with the same name would have been a parse error. So, just take the minimum element from the set,
                    -- which we know is a singleton.
                    | otherwise -> do
                        -- matched ctor name, consume the token
                        _ <- anyToken
                        pure (Just (Set.findMin refs))
          _ -> pure Nothing

      case maybeLocalCtor of
        Just localCtor -> pure (localCtor <$ tok)
        Nothing -> do
          names <- asks names
          case Names.lookupHQPattern Names.IncludeSuffixes (L.payload tok) ct names of
            s
              | Set.size s == 1 -> do
                  -- matched ctor name, consume the token
                  _ <- anyToken
                  pure (Set.findMin s <$ tok)
              | otherwise -> die names tok s
      where
        isLower = Text.all Char.isLower . Text.take 1 . NameSegment.toUnescapedText . Name.lastSegment
        isIgnored n = Text.take 1 (Name.toText n) == "_"
        die :: Names -> L.Token (HQ.HashQualified Name) -> Set ConstructorReference -> P v m a
        die names hq s = case L.payload hq of
          -- if token not hash qualified and not uppercase,
          -- fail w/out consuming it to allow backtracking
          HQ.NameOnly n
            | Set.null s
                && (isLower n || isIgnored n) ->
                fail $ "not a constructor name: " <> show n
          -- it was hash qualified and/or uppercase, and was either not found or ambiguous, that's a failure!
          _ ->
            failCommitted $
              ResolutionFailures
                [ TermResolutionFailure
                    (L.payload hq)
                    (ann hq)
                    if Set.null s
                      then NotFound
                      else
                        Ambiguous
                          names
                          (Set.map (\ref -> Referent.Con ref ct) s)
                          -- Eh, here we're saying there are no "local" constructors – they're all from "the namespace".
                          -- That's not necessarily true, but it doesn't (currently) affect the error message any, and
                          -- we have already parsed and hashed local constructors (so they aren't really different from
                          -- namespace constructors).
                          Set.empty
                ]
    unzipPatterns f elems = case unzip elems of (patterns, vs) -> f patterns (join vs)

    effectBind = do
      tok <- ctor CT.Effect
      leaves <- many leaf
      _ <- reserved "->"
      (cont, vsp) <- parsePattern
      pure $
        let f patterns vs = (Pattern.EffectBind (ann tok <> ann cont) (L.payload tok) patterns cont, vs ++ vsp)
         in unzipPatterns f leaves

    effectPure = go <$> parsePattern
      where
        go (p, vs) = (Pattern.EffectPure (ann p) p, vs)

    effect = do
      start <- openBlockWith "{"

      -- After the opening curly brace, we are expecting either an EffectBind or an EffectPure:
      --
      --   EffectBind            EffectPure
      --
      --   { foo bar -> baz }    { qux }
      --     ^^^^^^^^^^^^^^        ^^^
      --
      -- We accomplish that as follows:
      --
      --   * First try EffectPure + "}"
      --     * If that fails, back the parser up and try EffectBind + "}" instaed
      --
      -- This won't always result in the best possible error messages, but it's not exactly trivial to do better,
      -- requiring more sophisticated look-ahead logic. So, this is how it works for now.
      (inner, vs, end) <-
        asum
          [ P.try do
              (inner, vs) <- effectPure
              end <- closeBlock
              pure (inner, vs, end),
            do
              (inner, vs) <- effectBind
              end <- closeBlock
              pure (inner, vs, end)
          ]

      pure (Pattern.setLoc inner (ann start <> ann end), vs)

    -- ex: unique type Day = Mon | Tue | ...
    nullaryCtor = do
      tok <- ctor CT.Data
      pure (Pattern.Constructor (ann tok) (L.payload tok) [], [])

    constructor = do
      tok <- ctor CT.Data
      let f patterns vs =
            let loc = foldl (<>) (ann tok) $ map ann patterns
             in (Pattern.Constructor loc (L.payload tok) patterns, vs)
      unzipPatterns f <$> many leaf

    seqLiteral = Parser.seq f root
      where
        f loc = unzipPatterns ((,) . Pattern.SequenceLiteral loc)

lam :: (Var v) => TermP v m -> TermP v m
lam p = label "lambda" $ mkLam <$> P.try (some prefixDefinitionName <* reserved "->") <*> p
  where
    mkLam vs b =
      let annotatedArgs = vs <&> \v -> (ann v, L.payload v)
       in Term.lam' (ann (head vs) <> ann b) annotatedArgs b

letBlock, handle, ifthen :: (Monad m, Var v) => TermP v m
letBlock = label "let" $ (snd <$> layoutBlock "let")
handle = label "handle" do
  (handleSpan, b) <- block "handle"
  (_withSpan, handler) <- layoutBlock "with"
  -- We don't use the annotation span from 'with' here because it will
  -- include a dedent if it's at the end of block.
  -- Meaning the newline gets overwritten when pretty-printing and it messes things up.
  pure $ Term.handle (handleSpan <> ann handler) handler b

checkCasesArities :: (Ord v, Annotated a) => [(Int, a)] -> P v m (Int, [a])
checkCasesArities = \case
  [] -> pure (1, [])
  cases@((i, _) : rest) -> case List.find (\(j, _) -> j /= i) rest of
    Nothing -> pure (i, snd <$> cases)
    Just (j, a) -> P.customFailure $ PatternArityMismatch i j (ann a)

lamCase :: (Monad m, Var v) => TermP v m
lamCase = do
  start <- openBlockWith "cases"
  cases <- matchCases
  (arity, cases) <- checkCasesArities cases
  _ <- optionalCloseBlock
  lamvars <- replicateM arity (Parser.uniqueName 10)
  let vars =
        Var.named <$> [tweak v i | (v, i) <- lamvars `zip` [(1 :: Int) ..]]
      tweak v 0 = v
      tweak v i = v <> Text.pack (show i)
      lamvarTerms = Term.var (ann start) <$> vars
      lamvarTerm = case lamvarTerms of
        [e] -> e
        es -> DD.tupleTerm es
      anns = foldr ((<>) . ann) (ann start) $ lastMay cases
      matchTerm = Term.match anns lamvarTerm cases
  let annotatedVars = (Ann.GeneratedFrom $ ann start,) <$> vars
  pure $ Term.lam' anns annotatedVars matchTerm

ifthen = label "if" do
  start <- peekAny
  (_spanAnn, c) <- block "if"
  (_spanAnn, t) <- block "then"
  (_spanAnn, f) <- layoutBlock "else"
  pure $ Term.iff (ann start <> ann f) c t f

text :: (Var v) => TermP v m
text = tok Term.text <$> string

char :: (Var v) => TermP v m
char = tok Term.char <$> character

boolean :: (Var v) => TermP v m
boolean =
  ((\t -> Term.boolean (ann t) True) <$> reserved "true")
    <|> ((\t -> Term.boolean (ann t) False) <$> reserved "false")

list :: (Var v) => TermP v m -> TermP v m
list = Parser.seq Term.list

hashQualifiedPrefixTerm :: (Monad m, Var v) => TermP v m
hashQualifiedPrefixTerm = resolveHashQualified =<< hqPrefixId

quasikeyword :: (Ord v) => Text -> P v m (L.Token ())
quasikeyword kw = queryToken \case
  L.WordyId (HQ'.NameOnly n) | nameIsKeyword n kw -> Just ()
  _ -> Nothing

nameIsKeyword :: Name -> Text -> Bool
nameIsKeyword name keyword =
  case (Name.isRelative name, Name.reverseSegments name) of
    (True, segment NonEmpty.:| []) -> NameSegment.toEscapedText segment == keyword
    _ -> False

-- If the hash qualified is name only, it is treated as a var, if it
-- has a short hash, we resolve that short hash immediately and fail
-- committed if that short hash can't be found in the current environment
resolveHashQualified :: (Monad m, Var v) => L.Token (HQ.HashQualified Name) -> TermP v m
resolveHashQualified tok = do
  case L.payload tok of
    HQ.NameOnly n -> pure $ Term.var (ann tok) (Name.toVar n)
    _ -> do
      names <- asks names
      case Names.lookupHQTerm Names.IncludeSuffixes (L.payload tok) names of
        s
          | Set.null s -> failCommitted $ UnknownTerm tok s
          | Set.size s > 1 -> failCommitted $ UnknownTerm tok s
          | otherwise -> pure $ Term.fromReferent (ann tok) (Set.findMin s)

termLeaf :: forall m v. (Monad m, Var v) => TermP v m
termLeaf =
  asum
    [ force,
      hashQualifiedPrefixTerm,
      text,
      char,
      number,
      bytes,
      boolean,
      link,
      tupleOrParenthesizedTerm,
      keywordBlock,
      list term,
      delayQuote,
      (snd <$> delayBlock),
      bang,
      docBlock,
      doc2Block <&> \(spanAnn, trm) -> trm {ABT.annotation = ABT.annotation trm <> spanAnn}
    ]

-- | Gives a parser an explicit stream to parse, so that it consumes nothing from the original stream when it runs.
--
--   This is used inside the `Doc` -> `Term` conversion, where we have chunks of Unison code embedded that need to be
--   parsed. It’s a consequence of parsing Doc in the midst of the Unison lexer.
subParse :: (Ord v, Monad m) => P v m a -> [L.Token L.Lexeme] -> P v m a
subParse p toks = do
  orig <- P.getInput
  P.setInput $ Input toks
  result <- p <* P.eof
  P.setInput orig
  pure result

-- | Syntax for documentation v2 blocks, which are surrounded by @{{@ @}}@.
-- The lexer does most of the heavy lifting so there's not a lot for
-- the parser to do. For instance, in
--
-- > {{
-- > Hi there!
-- >
-- > goodbye.
-- > }}
--
-- the lexer will produce:
--
-- > [ Doc
-- >   ( DocUntitledSection
-- >     (DocParagraph (DocWord "Hi" :| [DocWord "there!"]))
-- >     (DocParagraph (DocWord "goodbye" :| []))
-- >   )
-- > ]
--
-- The parser will parse this into the Unison expression:
--
-- > syntax.docUntitledSection [
-- >   syntax.docParagraph [syntax.docWord "Hi", syntax.docWord "there!"],
-- >   syntax.docParagraph [syntax.docWord "goodbye"]
-- > ]
--
-- Where @syntax.doc{Paragraph, UntitledSection,...}@ are all ordinary term
-- variables that will be looked up in the environment like anything else. This
-- means that the documentation syntax can have its meaning changed by
-- overriding what functions the names @syntax.doc*@ correspond to.
doc2Block :: forall m v. (Monad m, Var v) => P v m (Ann {- Annotation for the whole spanning block -}, Term v Ann)
doc2Block = do
  L.Token docContents startDoc endDoc <- doc
  let docAnn = Ann startDoc endDoc
  (docAnn,) . docUntitledSection (gann docAnn) <$> traverse foldTop docContents
  where
    foldTop = cataM \(a :< top) -> docTop a =<< bitraverse (cataM \(a :< leaf) -> docLeaf a leaf) pure top

    gann :: (Annotated a) => a -> Ann
    gann = Ann.GeneratedFrom . ann

    addDelay :: Term v Ann -> Term v Ann
    addDelay tm = Term.delay (ann tm) tm

    f :: (Annotated a) => a -> String -> Term v Ann
    f a = Term.var (gann a) . Var.nameds . ("syntax.doc" <>)

    docUntitledSection :: Ann -> Doc.UntitledSection (Term v Ann) -> Term v Ann
    docUntitledSection ann (Doc.UntitledSection tops) =
      Term.app ann (f ann "UntitledSection") $ Term.list (gann tops) tops

    docTop :: Ann -> Doc.Top [L.Token L.Lexeme] (Term v Ann) (Term v Ann) -> TermP v m
    docTop d = \case
      Doc.Section title body -> pure $ Term.apps' (f d "Section") [docParagraph d title, Term.list (gann body) body]
      Doc.Eval code ->
        Term.app (gann d) (f d "Eval") . addDelay . snd
          <$> subParse (block' False False "syntax.docEval" (pure $ pure ()) $ Ann.External <$ P.eof) code
      Doc.ExampleBlock code ->
        Term.apps' (f d "ExampleBlock") . (Term.nat (gann d) 0 :) . pure . addDelay . snd
          <$> subParse (block' False True "syntax.docExampleBlock" (pure $ pure ()) $ Ann.External <$ P.eof) code
      Doc.CodeBlock label body ->
        pure $
          Term.apps'
            (f d "CodeBlock")
            [Term.text d $ Text.pack label, Term.text d $ Text.pack body]
      Doc.List' list -> pure $ docList d list
      Doc.Paragraph' para -> pure $ docParagraph d para

    docParagraph d leaves = Term.app (gann d) (f d "Paragraph") . Term.list d $ toList leaves

    docList :: Ann -> Doc.List (Term v Ann) -> Term v Ann
    docList d = \case
      Doc.BulletedList items ->
        Term.app (gann d) (f d "BulletedList") . Term.list (gann d) . toList $ docColumn d <$> items
      Doc.NumberedList items@((n, _) :| _) ->
        Term.apps'
          (f d "NumberedList")
          [Term.nat (ann d) $ n, Term.list (gann d) . toList $ docColumn d . snd <$> items]

    docColumn :: Ann -> Doc.Column (Term v Ann) -> Term v Ann
    docColumn d (Doc.Column para sublist) =
      Term.app (gann d) (f d "Column") . Term.list (gann d) $ docParagraph d para : toList (docList d <$> sublist)

    docLeaf :: Ann -> Doc.Leaf (L.Token (ReferenceType, HQ'.HashQualified Name)) [L.Token L.Lexeme] (Term v Ann) -> TermP v m
    docLeaf d = \case
      Doc.Link link -> Term.app (gann d) (f d "Link") <$> docEmbedLink d link
      Doc.NamedLink para group -> pure $ Term.apps' (f d "NamedLink") [docParagraph d para, docGroup d group]
      Doc.Example code -> do
        trm <- subParse term code
        pure . Term.apps' (f d "Example") $ case trm of
          tm@(Term.Apps' _ xs) ->
            let fvs = List.Extra.nubOrd $ concatMap (toList . Term.freeVars) xs
                n = Term.nat (ann tm) (fromIntegral (length fvs))
                lam = addDelay $ Term.lam' (ann tm) ((mempty,) <$> fvs) tm
             in [n, lam]
          tm -> [Term.nat (ann tm) 0, addDelay tm]
      Doc.Transclude' trans -> docTransclude d trans
      Doc.Bold para -> pure . Term.app (gann d) (f d "Bold") $ docParagraph d para
      Doc.Italic para -> pure . Term.app (gann d) (f d "Italic") $ docParagraph d para
      Doc.Strikethrough para -> pure . Term.app (gann d) (f d "Strikethrough") $ docParagraph d para
      Doc.Verbatim leaf -> pure . Term.app (gann d) (f d "Verbatim") $ docWord d leaf
      Doc.Code leaf -> pure . Term.app (gann d) (f d "Code") $ docWord d leaf
      Doc.Source elems ->
        Term.app (gann d) (f d "Source") . Term.list d . toList <$> traverse (docSourceElement d) elems
      Doc.FoldedSource elems ->
        Term.app (gann d) (f d "FoldedSource") . Term.list d . toList <$> traverse (docSourceElement d) elems
      Doc.EvalInline code -> Term.app (gann d) (f d "EvalInline") . addDelay <$> subParse term code
      Doc.Signature links ->
        Term.app (gann d) (f d "Signature") . Term.list d . toList <$> traverse (docEmbedSignatureLink d) links
      Doc.SignatureInline link -> Term.app (gann d) (f d "SignatureInline") <$> docEmbedSignatureLink d link
      Doc.Word' word -> pure $ docWord d word
      Doc.Group' group -> pure $ docGroup d group

    docEmbedLink :: Ann -> Doc.EmbedLink (L.Token (ReferenceType, HQ'.HashQualified Name)) -> TermP v m
    docEmbedLink d (Doc.EmbedLink (L.Token (level, ident) start end)) = case level of
      RtType ->
        Term.app (gann d) (f d "EmbedTypeLink") . Term.typeLink (ann d) . L.payload
          <$> findUniqueType (L.Token (HQ'.toHQ ident) start end)
      RtTerm ->
        Term.app (gann d) (f d "EmbedTermLink") . addDelay <$> resolveHashQualified (L.Token (HQ'.toHQ ident) start end)

    docTransclude :: Ann -> Doc.Transclude [L.Token L.Lexeme] -> TermP v m
    docTransclude d (Doc.Transclude code) = Term.app (gann d) (f d "Transclude") <$> subParse term code

    docSourceElement ::
      Ann ->
      Doc.SourceElement (L.Token (ReferenceType, HQ'.HashQualified Name)) (Doc.Transclude [L.Token L.Lexeme]) ->
      TermP v m
    docSourceElement d (Doc.SourceElement link anns) = do
      link' <- docEmbedLink d link
      anns' <- traverse (docEmbedAnnotation d) anns
      pure $ Term.apps' (f d "SourceElement") [link', Term.list d anns']

    docEmbedSignatureLink ::
      Ann -> Doc.EmbedSignatureLink (L.Token (ReferenceType, HQ'.HashQualified Name)) -> TermP v m
    docEmbedSignatureLink d (Doc.EmbedSignatureLink (L.Token (level, ident) start end)) = case level of
      RtType -> P.customFailure . TypeNotAllowed $ L.Token (HQ'.toHQ ident) start end
      RtTerm ->
        Term.app (gann d) (f d "EmbedSignatureLink") . addDelay
          <$> resolveHashQualified (L.Token (HQ'.toHQ ident) start end)

    docEmbedAnnotation ::
      Ann ->
      Doc.EmbedAnnotation (L.Token (ReferenceType, HQ'.HashQualified Name)) (Doc.Transclude [L.Token L.Lexeme]) ->
      TermP v m
    docEmbedAnnotation d (Doc.EmbedAnnotation a) =
      -- This is the only place I’m not sure we’re doing the right thing. In the lexer, this can be an identifier or a
      -- DocLeaf, but here it could be either /text/ or a Doc element. And I don’t think there’s any way the lexemes
      -- produced for an identifier and the lexemes consumed for text line up. So, I think this is a bugfix I can’t
      -- avoid.
      Term.app (gann d) (f d "EmbedAnnotation")
        <$> either
          ( \(L.Token (level, ident) start end) -> case level of
              RtType -> P.customFailure . TypeNotAllowed $ L.Token (HQ'.toHQ ident) start end
              RtTerm -> resolveHashQualified $ L.Token (HQ'.toHQ ident) start end
          )
          (docTransclude d)
          a

    docWord :: Ann -> Doc.Word -> Term v Ann
    docWord d (Doc.Word txt) = Term.app (gann d) (f d "Word") . Term.text d $ Text.pack txt

    docGroup :: Ann -> Doc.Group (Term v Ann) -> Term v Ann
    docGroup d (Doc.Group (Doc.Join leaves)) =
      Term.app d (f d "Group") . Term.app d (f d "Join") . Term.list (ann leaves) $ toList leaves

docBlock :: (Monad m, Var v) => TermP v m
docBlock = do
  openTok <- openBlockWith "[:"
  segs <- many segment
  closeTok <- closeBlock
  let a = ann openTok <> ann closeTok
  pure . docNormalize $ Term.app a (Term.constructor a (ConstructorReference DD.docRef DD.docJoinId)) (Term.list a segs)
  where
    segment = blob <|> linky
    blob = do
      s <- string
      pure $
        Term.app
          (ann s)
          (Term.constructor (ann s) (ConstructorReference DD.docRef DD.docBlobId))
          (Term.text (ann s) (L.payload s))
    linky = asum [include, signature, evaluate, source, link]
    include = do
      _ <- P.try (reserved "include")
      hashQualifiedPrefixTerm
    signature = do
      _ <- P.try (reserved "signature")
      tok <- termLink'
      pure $
        Term.app
          (ann tok)
          (Term.constructor (ann tok) (ConstructorReference DD.docRef DD.docSignatureId))
          (Term.termLink (ann tok) (L.payload tok))
    evaluate = do
      _ <- P.try (reserved "evaluate")
      tok <- termLink'
      pure $
        Term.app
          (ann tok)
          (Term.constructor (ann tok) (ConstructorReference DD.docRef DD.docEvaluateId))
          (Term.termLink (ann tok) (L.payload tok))
    source = do
      _ <- P.try (reserved "source")
      l <- link''
      pure $
        Term.app
          (ann l)
          (Term.constructor (ann l) (ConstructorReference DD.docRef DD.docSourceId))
          l
    link'' = either ty t <$> link'
      where
        t tok =
          Term.app
            (ann tok)
            (Term.constructor (ann tok) (ConstructorReference DD.linkRef DD.linkTermId))
            (Term.termLink (ann tok) (L.payload tok))
        ty tok =
          Term.app
            (ann tok)
            (Term.constructor (ann tok) (ConstructorReference DD.linkRef DD.linkTypeId))
            (Term.typeLink (ann tok) (L.payload tok))
    link = d <$> link''
      where
        d tm = Term.app (ann tm) (Term.constructor (ann tm) (ConstructorReference DD.docRef DD.docLinkId)) tm

-- Used by unbreakParas within docNormalize.  Doc literals are a joined sequence
-- segments.  This type describes a property of a segment.
data UnbreakCase
  = -- Finishes with a newline and hence does not determine whether the next
    -- line starts with whitespace.
    LineEnds
  | -- Ends with "\n something", i.e. introduces an indented line.
    StartsIndented
  | -- Ends with "\nsomething", i.e. introduces an unindented line.
    StartsUnindented
  deriving (Eq, Show)

-- Doc literal normalization
--
-- This normalization allows the pretty-printer and doc display code to do
-- indenting, and to do line-wrap of paragraphs, but without the inserted
-- newlines being then frozen into the text for ever more over subsequent
-- edit/update cycles.
--
-- The alternative would be to stop line-wrapping docs on view/display by adding
-- newlines in the pretty-printer, and instead leave wrapping to the
-- terminal/editor.  Might be worth considering if this code ends up being
-- too buggy and fragile to maintain.  Maybe display could add newlines,
-- and view could refrain from doing so.
--
-- Operates on the text of the Blobs within a doc (as parsed by docBlock):
-- - reduces the whitespace after all newlines so that at least one of the
--   non-initial lines has zero indent (important because the pretty-printer adds
--   indenting when displaying doc literals)
-- - removes trailing whitespace from each line
-- - removes newlines between any sequence of non-empty zero-indent lines
--   (i.e. undo line-breaking within paragraphs).
--
-- Should be understood in tandem with Util.Pretty.paragraphyText, which
-- outputs doc text for display/edit/view.
-- See also unison-src/transcripts/doc-formatting.md.
--
-- There is some heuristic/approximate logic in here - see the comment flagged
-- with ** below.
--
-- This function is a bit painful - it's trying to act on a sequence of lines,
-- but that sequence is split up between the various blobs in the doc, which
-- are separated by the elements tracking things like @[source] etc.  It
-- would be simplified if the doc representation was something like
-- [Either Char EnrichedElement].
--
-- This function has some tracing which you can enable by deleting some calls to
-- 'const id' below.
docNormalize :: (Ord v, Show v) => Term v a -> Term v a
docNormalize tm = case tm of
  -- This pattern is just `DD.DocJoin seqs`, but exploded in order to grab
  -- the annotations.  The aim is just to map `normalize` over it.
  a@(Term.App' c@(Term.Constructor' (ConstructorReference DD.DocRef DD.DocJoinId)) s@(Term.List' seqs)) ->
    join
      (ABT.annotation a)
      (ABT.annotation c)
      (ABT.annotation s)
      (normalize seqs)
    where

  _ -> error $ "unexpected doc structure: " ++ show tm
  where
    normalize =
      Sequence.fromList
        . (map TupleE.fst3)
        . (tracing "after unbreakParas")
        . unbreakParas
        . (tracing "after full preprocess")
        . preProcess
        . (tracing "after unindent")
        . unIndent
        . (tracing "initial parse")
        . miniPreProcess
    preProcess xs =
      zip3
        seqs
        (lineStarteds $ Sequence.fromList seqs)
        (followingLines $ Sequence.fromList seqs)
      where
        seqs = map fst xs
    miniPreProcess seqs = zip (toList seqs) (lineStarteds seqs)
    unIndent ::
      (Ord v) =>
      [(Term v a, UnbreakCase)] ->
      [(Term v a, UnbreakCase)]
    unIndent tms = map go tms
      where
        go (b, previous) =
          ((mapBlob $ (reduceIndent includeFirst minIndent)) b, previous)
          where
            -- Since previous was calculated before unindenting, it will often be wrongly
            -- StartsIndented instead of StartsUnindented - but that's OK just for the test
            -- below.  And we'll recalculate it later in preProcess.
            includeFirst = previous == LineEnds
        concatenatedBlobs :: Text
        concatenatedBlobs = mconcat (toList (fmap (getBlob . fst) tms))
        getBlob (DD.DocBlob txt) = txt
        getBlob _ = "."
        -- Note we exclude the first line when calculating the minimum indent - the lexer
        -- already stripped leading spaces from it, and anyway it would have been sharing
        -- its line with the [: and maybe other stuff.
        nonInitialNonEmptyLines =
          filter (not . Text.null) $
            map Text.stripEnd $
              drop 1 $
                Text.lines
                  concatenatedBlobs
        minIndent =
          minimumOrZero $
            map
              (Text.length . (Text.takeWhile Char.isSpace))
              nonInitialNonEmptyLines
        minimumOrZero xs = if length xs == 0 then 0 else minimum xs
        reduceIndent :: Bool -> Int -> Text -> Text
        reduceIndent includeFirst n t =
          fixup $
            Text.unlines $
              mapExceptFirst reduceLineIndent onFirst $
                Text.lines t
          where
            onFirst = if includeFirst then reduceLineIndent else id
            reduceLineIndent l = result
              where
                currentIndent = Text.length $ (Text.takeWhile Char.isSpace) l
                remainder = (Text.dropWhile Char.isSpace) l
                newIndent = maximum [0, currentIndent - n]
                result = Text.replicate newIndent " " `mappend` remainder
            -- unlines . lines adds a trailing newline if one was not present: undo that.
            fixup = if Text.takeEnd 1 t == "\n" then id else Text.dropEnd 1
    -- Remove newlines between any sequence of non-empty zero-indent lines.
    -- This is made more complicated by Doc elements (e.g. links) which break up a
    -- blob but don't break a line of output text**.  We sometimes need to refer back to the
    -- previous blob to see whether a newline is between two zero-indented lines.
    -- For example...
    -- "This link to @foo makes it harder to see\n
    --  that the newline should be removed."
    -- Whether an element does this (breaks a blob but not a line of output text) really
    -- depends on some things we don't know here: does an @[include] target doc occupy
    -- just one line or several; whether this doc is going to be viewed or displayed.
    -- So we'll get it wrong sometimes.  The impact of this is that we may sometimes
    -- misjudge whether a newline is separating two non-indented lines, and should therefore
    -- be removed.
    unbreakParas ::
      (Show v, Ord v) =>
      [(Term v a, UnbreakCase, Bool)] ->
      [(Term v a, UnbreakCase, Bool)]
    unbreakParas = map go
      where
        -- 'candidate' means 'candidate to be joined with an adjacent line as part of a
        -- paragraph'.
        go (b, previous, nextIsCandidate) =
          (mapBlob go b, previous, nextIsCandidate)
          where
            go txt = if Text.null txt then txt else tr result'
              where
                tr =
                  const id $
                    trace $
                      "\nprocessElement on blob "
                        ++ (show txt)
                        ++ ", result' = "
                        ++ (show result')
                        ++ ", lines: "
                        ++ (show ls)
                        ++ ", candidates = "
                        ++ (show candidates)
                        ++ ", previous = "
                        ++ (show previous)
                        ++ ", firstIsCandidate = "
                        ++ (show firstIsCandidate)
                        ++ "\n\n"
                -- remove trailing whitespace
                -- ls is non-empty thanks to the Text.null check above
                -- Don't cut the last line's trailing whitespace - there's an assumption here
                -- that it's followed by something which will put more text on the same line.
                ls = mapExceptLast Text.stripEnd id $ Text.lines txt
                -- Work out which lines are candidates to be joined as part of a paragraph, i.e.
                -- are not indented.
                candidate l = case Text.uncons l of
                  Just (initial, _) -> not . Char.isSpace $ initial
                  Nothing -> False -- empty line
                  -- The segment of this blob that runs up to the first newline may not itself
                  -- be the start of a line of the doc - for example if it's preceded by a link.
                  -- So work out whether the line of which it is a part is a candidate.
                firstIsCandidate = case previous of
                  LineEnds -> candidate (head ls)
                  StartsIndented -> False
                  StartsUnindented -> True
                candidates = firstIsCandidate : (tail (map candidate ls))
                result = mconcat $ intercalateMapWith sep fst (zip ls candidates)
                sep (_, candidate1) (_, candidate2) =
                  if candidate1 && candidate2 then " " else "\n"
                -- Text.lines forgets whether there was a trailing newline.
                -- If there was one, then either add it back or convert it to a space.
                result' =
                  if (Text.takeEnd 1 txt) == "\n"
                    then
                      if (last candidates) && nextIsCandidate
                        then result `Text.append` " "
                        else result `Text.append` "\n"
                    else result
    -- A list whose entries match those of tms.  `Nothing` is used for elements
    -- which just continue a line, and so need to be ignored when looking back
    -- for how the last line started.  Otherwise describes whether the last
    -- line of this entry is indented (or maybe terminated by a newline.)
    -- A value of `Nothing` protects ensuing text from having its leading
    -- whitespace removed by `unindent`.
    -- Note that some elements render over multiple lines when displayed.
    -- See test2 in transcript doc-formatting.md for an example of how
    -- this looks when there is whitespace immediately following @[source]
    -- or @[evaluate].
    lastLines :: (Show v) => Sequence.Seq (Term v a) -> [Maybe UnbreakCase]
    lastLines tms = (flip fmap) (toList tms) $ \case
      DD.DocBlob txt -> unbreakCase txt
      DD.DocLink _ -> Nothing
      DD.DocSource _ -> Nothing
      DD.DocSignature _ -> Nothing
      DD.DocEvaluate _ -> Nothing
      Term.Var' _ -> Nothing -- @[include]
      e@_ -> error ("unexpected doc element: " ++ show e)
    -- Work out whether the last line of this blob is indented (or maybe
    -- terminated by a newline.)
    unbreakCase :: Text -> Maybe UnbreakCase
    unbreakCase txt =
      let (startAndNewline, afterNewline) = Text.breakOnEnd "\n" txt
       in if Text.null startAndNewline
            then Nothing
            else
              if Text.null afterNewline
                then Just LineEnds
                else
                  if Char.isSpace (Text.head afterNewline)
                    then Just StartsIndented
                    else Just StartsUnindented
    -- A list whose entries match those of tms.  Describes how the current
    -- line started (the line including the start of this entry) - or LineEnds
    -- if this entry is starting a line itself.
    -- Calculated as the UnbreakCase of the previous entry that included a newline.
    -- Really there's a function of type (a -> Bool) -> a -> [a] -> [a] in here
    -- fighting to break free - overwriting elements that are 'shadowed' by
    -- a preceding element for which the predicate is true, with a copy of
    -- that element.
    lineStarteds :: (Show v) => Sequence.Seq (Term v a) -> [UnbreakCase]
    lineStarteds tms = tr $ quenchRuns LineEnds StartsUnindented $ xs''
      where
        tr =
          const id $
            trace $
              "lineStarteds: xs = "
                ++ (show xs)
                ++ ", xss = "
                ++ (show xss)
                ++ ", xs' = "
                ++ (show xs')
                ++ ", xs'' = "
                ++ (show xs'')
                ++ "\n\n"
        -- Make sure there's a Just at the start of the list so we always find
        -- one when searching back.
        -- Example: xs = [J1,N2,J3]
        xs :: [Maybe UnbreakCase]
        xs = Just LineEnds : (lastLines tms)
        -- Example: xss = [[J1],[J1,N2],[J1,N2,J3]]
        xss :: [[Maybe UnbreakCase]]
        xss = drop 1 $ List.inits xs
        -- Example: after each step of the map...
        --            [[J1],[N2,J1],[J3,N2,J1]]   -- after reverse
        --            [Just J1, Just J1, Just J3] -- after find
        --            ...
        --   result = [1,1,3]
        xs' =
          map (Maybe.fromJust . Maybe.fromJust . (List.find isJust) . reverse) xss
        xs'' = List.Extra.dropEnd 1 xs'
    -- For each element, can it be a line-continuation of a preceding blob?
    continuesLine :: Sequence.Seq (Term v a) -> [Bool]
    continuesLine tms = (flip fmap) (toList tms) \case
      DD.DocBlob _ -> False -- value doesn't matter - you don't get adjacent blobs
      DD.DocLink _ -> True
      DD.DocSource _ -> False
      DD.DocSignature _ -> False
      DD.DocEvaluate _ -> False
      Term.Var' _ -> False -- @[include]
      _ -> error ("unexpected doc element" ++ show tm)
    -- A list whose entries match those of tms.  Can the subsequent entry by a
    -- line continuation of this one?
    followingLines tms = drop 1 ((continuesLine tms) ++ [False])
    mapExceptFirst :: (a -> b) -> (a -> b) -> [a] -> [b]
    mapExceptFirst fRest fFirst = \case
      [] -> []
      x : rest -> (fFirst x) : (map fRest rest)
    mapExceptLast fRest fLast = reverse . (mapExceptFirst fRest fLast) . reverse
    tracing :: (Show a) => [Char] -> a -> a
    tracing when x =
      (const id $ trace ("at " ++ when ++ ": " ++ (show x) ++ "\n")) x
    blob aa ac at txt =
      Term.app aa (Term.constructor ac (ConstructorReference DD.docRef DD.docBlobId)) (Term.text at txt)
    join aa ac as segs =
      Term.app aa (Term.constructor ac (ConstructorReference DD.docRef DD.docJoinId)) (Term.list' as segs)
    mapBlob :: (Ord v) => (Text -> Text) -> Term v a -> Term v a
    -- this pattern is just `DD.DocBlob txt` but exploded to capture the annotations as well
    mapBlob f (aa@(Term.App' ac@(Term.Constructor' (ConstructorReference DD.DocRef DD.DocBlobId)) at@(Term.Text' txt))) =
      blob (ABT.annotation aa) (ABT.annotation ac) (ABT.annotation at) (f txt)
    mapBlob _ t = t

delayQuote :: (Monad m, Var v) => TermP v m
delayQuote = P.label "quote" do
  start <- reserved "'"
  e <- termLeaf
  pure $ DD.delayTerm (ann start <> ann e) (ann start) e

delayBlock :: (Monad m, Var v) => P v m (Ann {- Ann spanning the whole block -}, Term v Ann)
delayBlock = P.label "do" do
  (spanAnn, b) <- layoutBlock "do"
  let argSpan = (ann b {- would be nice to use the annotation for 'do' here, but it's not terribly important -})
  pure $ (spanAnn, DD.delayTerm (ann b) argSpan b)

bang :: (Monad m, Var v) => TermP v m
bang = P.label "bang" do
  start <- reserved "!"
  e <- termLeaf
  pure $ DD.forceTerm (ann start <> ann e) (ann start) e

force :: forall m v. (Monad m, Var v) => TermP v m
force = P.label "force" $ P.try do
  -- `forkAt pool() blah` parses as `forkAt (pool ()) blah`
  -- That is, empty parens immediately (no space) following a symbol
  -- is treated as high precedence function application of `Unit`
  fn <- hashQualifiedPrefixTerm
  tok <- ann <$> openBlockWith "("
  guard (L.column (Ann.start tok) == L.column (Ann.end (ann fn)))
  close <- closeBlock
  pure $ DD.forceTerm (ann fn <> ann close) (tok <> ann close) fn

seqOp :: (Ord v) => P v m Pattern.SeqOp
seqOp =
  Pattern.Snoc <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.snocSegment)))
    <|> Pattern.Cons <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.consSegment)))
    <|> Pattern.Concat <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.concatSegment)))

term4 :: (Monad m, Var v) => TermP v m
term4 = f <$> some termLeaf
  where
    f (func : args) = Term.apps func ((\a -> (ann func <> ann a, a)) <$> args)
    f [] = error "'some' shouldn't produce an empty list"

data InfixParse v
  = InfixOp (L.Token (HQ.HashQualified Name)) (Term v Ann) (InfixParse v) (InfixParse v)
  | InfixAnd (L.Token String) (InfixParse v) (InfixParse v)
  | InfixOr (L.Token String) (InfixParse v) (InfixParse v)
  | InfixOperand (Term v Ann)
  deriving (Show, Eq, Ord)

-- e.g. term4 + term4 - term4
-- or term4 || term4 && term4
-- The algorithm works as follows:
-- 1. Parse the expression left-associated
-- 2. Starting at the leftmost operator subexpression, see if the next operator
--   has higher precedence. If so, rotate the expression to the right.
--   e.g. in `a + b * c`, we first parse `(a + b) * c` then rotate to `a + (b * c)`.
-- 3. Perform the algorithm on the right-hand side if necessary, as `b` might be
--   an infix expression with lower precedence than `*`.
-- 4. Proceed to the next operator to the right in the original expression and
--    repeat steps 2-3 until we reach the end.
infixAppOrBooleanOp :: forall m v. (Monad m, Var v) => TermP v m
infixAppOrBooleanOp = do
  (p, ps) <- prelimParse
  -- traceShowM ("orig" :: String, foldl' (flip ($)) p ps)
  let p' = reassociate (p, ps)
  -- traceShowM ("reassoc" :: String, p')
  return (applyInfixOps p')
  where
    -- To handle a mix of infix operators with and without precedence rules,
    -- we first parse the expression left-associated, then reassociate it
    -- according to the precedence rules.
    prelimParse =
      chainl1Accum (InfixOperand <$> term4) genericInfixApp
    genericInfixApp =
      (InfixAnd <$> (label "and" (reserved "&&")))
        <|> (InfixOr <$> (label "or" (reserved "||")))
        <|> (uncurry InfixOp <$> parseInfix)
    shouldRotate child parent = case (child, parent) of
      (Just p1, Just p2) -> p1 < p2
      _ -> False
    parseInfix = label "infixApp" do
      op <- hqInfixId <* optional semi
      resolved <- resolveHashQualified op
      pure (op, resolved)
    reassociate (exp, ops) =
      foldl' checkOp exp ops
    checkOp exp op = fixUp (op exp)
    fixUp = \case
      InfixOp op tm lhs rhs ->
        rotate (unqualified op) (InfixOp op tm) lhs rhs
      InfixAnd op lhs rhs ->
        rotate "&&" (InfixAnd op) lhs rhs
      InfixOr op lhs rhs ->
        rotate "||" (InfixOr op) lhs rhs
      x -> x
    rotate op ctor lhs rhs =
      case lhs of
        InfixOp lop ltm ll lr
          | shouldRotate (operatorPrecedence (unqualified lop)) (operatorPrecedence op) ->
              InfixOp lop ltm ll (fixUp (ctor lr rhs))
        InfixAnd lop ll lr
          | shouldRotate (operatorPrecedence "&&") (operatorPrecedence op) ->
              InfixAnd lop ll (fixUp (ctor lr rhs))
        InfixOr lop ll lr
          | shouldRotate (operatorPrecedence "||") (operatorPrecedence op) ->
              InfixOr lop ll (fixUp (ctor lr rhs))
        _ -> ctor lhs rhs
    unqualified t = Maybe.fromJust $ NameSegment.toEscapedText . Name.lastSegment <$> (HQ.toName $ L.payload t)
    applyInfixOps :: InfixParse v -> Term v Ann
    applyInfixOps t = case t of
      InfixOp _ tm lhs rhs ->
        Term.apps' tm [applyInfixOps lhs, applyInfixOps rhs]
      InfixOperand tm -> tm
      InfixAnd op lhs rhs ->
        let lhs' = applyInfixOps lhs
            rhs' = applyInfixOps rhs
         in Term.and (ann lhs' <> ann op <> ann rhs') lhs' rhs'
      InfixOr op lhs rhs ->
        let lhs' = applyInfixOps lhs
            rhs' = applyInfixOps rhs
         in Term.or (ann lhs' <> ann op <> ann rhs') lhs' rhs'

typedecl :: (Monad m, Var v) => P v m (L.Token v, Type v Ann)
typedecl =
  (,)
    <$> P.try (prefixTermName <* reserved ":")
    <*> TypeParser.valueType
    <* semi

verifyRelativeVarName :: (Var v) => P v m (L.Token v) -> P v m (L.Token v)
verifyRelativeVarName p = do
  v <- p
  verifyRelativeName' (Name.unsafeParseVar <$> v)
  pure v

verifyRelativeName' :: (Ord v) => L.Token Name -> P v m ()
verifyRelativeName' name = do
  let txt = Name.toText . L.payload $ name
  when (Text.isPrefixOf "." txt && txt /= ".") $
    failCommitted (DisallowedAbsoluteName name)

-- example:
--   (x, y)   = foo
--   stuff
--
-- desugars to:
--
--   match foo with
--     (x,y) -> stuff
--
destructuringBind :: forall m v. (Monad m, Var v) => P v m (Ann, Term v Ann -> Term v Ann)
destructuringBind = do
  -- We have to look ahead as far as the `=` to know if this is a bind or
  -- just an action, for instance:
  --   (Some 42)
  --   vs
  --   (Some 42) = List.head elems
  (p, boundVars) <- P.try do
    (p, boundVars) <- parsePattern
    let boundVars' = snd <$> boundVars
    _ <- P.lookAhead (openBlockWith "=")
    pure (p, boundVars')
  (_spanAnn, scrute) <- layoutBlock "=" -- Dwight K. Scrute ("The People's Scrutinee")
  let guard = Nothing
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
      thecase t = Term.MatchCase p (fmap (absChain boundVars) guard) $ absChain boundVars t
  pure
    ( ann p,
      \t ->
        let a = ann p <> ann t
         in Term.match a scrute [thecase t]
    )

-- | Rules for the annotation of the resulting binding is as follows:
-- * If the binding has a type signature, the top level scope of the annotation for the type
-- Ann node will contain the _entire_ binding, including the type signature.
-- * The body expression of the binding contains the entire lhs (including the name of the
-- binding) and the entire body.
-- * If the binding is a lambda, the  lambda node includes the entire LHS of the binding,
-- including the name as well.
binding :: forall m v. (Monad m, Var v) => P v m ((Ann, v), Term v Ann)
binding = label "binding" do
  typ <- optional typedecl
  -- a ++ b = ...
  let infixLhs = do
        (arg1, op) <-
          P.try $
            (,) <$> prefixDefinitionName <*> symbolyDefinitionName
        arg2 <- prefixDefinitionName
        pure (ann arg1, op, [arg1, arg2])
  let prefixLhs = do
        v <- prefixTermName
        vs <- many prefixTermName
        pure (ann v, v, vs)
  let lhs :: P v m (Ann, L.Token v, [L.Token v])
      lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (lhsLoc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      (_bodySpanAnn, body) <- block "="
      verifyRelativeName' (fmap Name.unsafeParseVar name)
      let binding = mkBinding lhsLoc args body
      -- We don't actually use the span annotation from the block (yet) because it
      -- may contain a bunch of white-space and comments following a top-level-definition.
      let spanAnn = ann lhsLoc <> ann binding
      pure $ ((spanAnn, (L.payload name)), binding)
    Just (nameT, typ) -> do
      (lhsLoc, name, args) <- lhs
      verifyRelativeName' (fmap Name.unsafeParseVar name)
      when (L.payload name /= L.payload nameT) $
        customFailure $
          SignatureNeedsAccompanyingBody nameT
      (_bodySpanAnn, body) <- block "="
      let binding = mkBinding lhsLoc args body
      -- We don't actually use the span annotation from the block (yet) because it
      -- may contain a bunch of white-space and comments following a top-level-definition.
      let spanAnn = ann nameT <> ann binding
      pure $ ((spanAnn, L.payload name), Term.ann (ann nameT <> ann binding) binding typ)
  where
    mkBinding :: Ann -> [L.Token v] -> Term.Term v Ann -> Term.Term v Ann
    mkBinding _lhsLoc [] body = body
    mkBinding lhsLoc args body =
      let annotatedArgs = args <&> \arg -> (ann arg, L.payload arg)
       in Term.lam' (lhsLoc <> ann body) annotatedArgs body

customFailure :: (P.MonadParsec e s m) => e -> m a
customFailure = P.customFailure

block :: forall m v. (Monad m, Var v) => String -> P v m (Ann, Term v Ann)
block s = block' False False s (openBlockWith s) closeBlock

layoutBlock :: forall m v. (Monad m, Var v) => String -> P v m (Ann, Term v Ann)
layoutBlock s = block' False False s (openBlockWith s) optionalCloseBlock

-- example: use Foo.bar.Baz + ++ x
-- + ++ and x are called the "suffixes" of the `use` statement, and
-- `Foo.bar.Baz` is called the prefix. A `use` statement has the effect
-- of allowing you to reference identifiers of the form <prefix>.<suffix>
-- using just <suffix>.
--
-- `use foo` by itself is equivalent to `use foo bar baz ...` for all
-- names in the environment prefixed by `foo`
--
-- todo: doesn't support use Foo.bar ++#abc, which lets you use `++` unqualified to refer to `Foo.bar.++#abc`
importp :: (Monad m, Ord v) => P v m [(Name, Name)]
importp = do
  kw <- reserved "use"
  -- we allow symbolyId here and parse the suffix optionaly, so we can generate
  -- a nicer error message if the suffixes are empty
  prefix <-
    optional $
      fmap Right importWordyId
        <|> fmap Left importSymbolyId
  suffixes <- optional (some (importRelativeWordyId <|> importRelativeSymbolyId))
  case (prefix, suffixes) of
    (Nothing, _) -> P.customFailure $ UseEmpty kw
    (Just prefix@(Left _), _) -> P.customFailure $ UseInvalidPrefixSuffix prefix suffixes
    (Just (Right prefix), Nothing) -> do
      -- `wildcard import`
      names <- asks names
      pure $ Names.expandWildcardImport (L.payload prefix) names
    (Just (Right prefix), Just suffixes) -> pure do
      suffix <- L.payload <$> suffixes
      pure (suffix, Name.joinDot (L.payload prefix) suffix)

data BlockElement v
  = Binding ((Ann, v), Term v Ann)
  | DestructuringBind (Ann, Term v Ann -> Term v Ann)
  | Action (Term v Ann)

instance (Show v) => Show (BlockElement v) where
  show (Binding ((pos, name), _)) = show ("binding: " :: Text, pos, name)
  show (DestructuringBind (pos, _)) = show ("destructuring bind: " :: Text, pos)
  show (Action tm) = show ("action: " :: Text, ann tm)

-- subst
-- use Foo.Bar + blah
-- use Bar.Baz zonk zazzle
imports :: (Monad m, Var v) => P v m (Names, [(v, v)])
imports = do
  let sem = P.try (semi <* P.lookAhead (reserved "use"))
  imported <- mconcat . reverse <$> sepBy sem importp
  ns' <- Names.importing imported <$> asks names
  pure (ns', [(Name.toVar suffix, Name.toVar full) | (suffix, full) <- imported])

-- A key feature of imports is we want to be able to say:
-- `use foo.bar Baz qux` without having to specify whether `Baz` or `qux` are
-- terms or types.
substImports :: (Var v) => Names -> [(v, v)] -> Term v Ann -> Term v Ann
substImports ns imports =
  ABT.substsInheritAnnotation
    [ (suffix, Term.var () full)
      | (suffix, full) <- imports
    ]
    . Term.substTypeVars -- no guard here, as `full` could be bound
    -- not in Names, but in a later term binding
      [ (suffix, Type.var () full)
        | (suffix, full) <- imports,
          Names.hasTypeNamed Names.IncludeSuffixes (Name.unsafeParseVar full) ns
      ]

block' ::
  forall m v end.
  (Monad m, Var v, Annotated end) =>
  IsTop ->
  -- | `True` means insert `()` at end of block if it ends with a statement
  Bool ->
  String ->
  P v m (L.Token ()) ->
  P v m end ->
  P v m (Ann {- ann which spans the whole block -}, Term v Ann)
block' isTop implicitUnitAtEnd s openBlock closeBlock = do
  open <- openBlock
  (names, imports) <- imports
  _ <- optional semi
  statements <- local (\e -> e {names}) $ sepBy semi statement
  end <- closeBlock
  body <- substImports names imports <$> go open statements
  pure (ann open <> ann end, body)
  where
    statement = asum [Binding <$> binding, DestructuringBind <$> destructuringBind, Action <$> blockTerm]
    go :: L.Token () -> [BlockElement v] -> P v m (Term v Ann)
    go open =
      let finish :: Term.Term v Ann -> TermP v m
          finish tm = case Components.minimize' tm of
            Left dups -> customFailure $ DuplicateTermNames (toList dups)
            Right tm -> pure tm
          toTm :: [BlockElement v] -> TermP v m
          toTm [] = customFailure $ EmptyBlock (const s <$> open)
          toTm (be : bes) = do
            let (bs, blockResult) = determineBlockResult (be :| bes)
            finish =<< foldrM step blockResult bs
            where
              step :: BlockElement v -> Term v Ann -> TermP v m
              step elem result = case elem of
                Binding ((a, v), tm) ->
                  pure $
                    Term.consLetRec
                      isTop
                      (ann a <> ann result)
                      (a, v, tm)
                      result
                Action tm ->
                  pure $
                    Term.consLetRec
                      isTop
                      (ann tm <> ann result)
                      (ann tm, positionalVar (ann tm) (Var.named "_"), tm)
                      result
                DestructuringBind (_, f) ->
                  f <$> finish result
          determineBlockResult :: NonEmpty (BlockElement v) -> ([BlockElement v], Term v Ann)
          determineBlockResult bs = case NonEmpty.reverse bs of
            Binding ((a, _v), _) :| _ ->
              if implicitUnitAtEnd
                then (toList bs, DD.unitTerm a)
                else (toList bs, Term.var a (positionalVar a Var.missingResult))
            Action e :| bs -> (reverse (toList bs), e)
            DestructuringBind (a, _) :| _ ->
              if implicitUnitAtEnd
                then (toList bs, DD.unitTerm a)
                else (toList bs, Term.var a (positionalVar a Var.missingResult))
       in toTm

number :: (Var v) => TermP v m
number = number' (tok Term.int) (tok Term.nat) (tok Term.float)

bytes :: (Var v) => TermP v m
bytes = do
  b <- bytesToken
  let a = ann b
  pure $
    Term.app
      a
      (Term.builtin a "Bytes.fromList")
      (Term.list a $ Term.nat a . fromIntegral <$> Bytes.toWord8s (L.payload b))

number' ::
  (Ord v) =>
  (L.Token Int64 -> a) ->
  (L.Token Word64 -> a) ->
  (L.Token Double -> a) ->
  P v m a
number' i u f = fmap go numeric
  where
    go num@(L.payload -> p)
      | any (\c -> c == '.' || c == 'e') p && take 1 p == "+" = f (read . drop 1 <$> num)
      | any (\c -> c == '.' || c == 'e') p = f (read <$> num)
      | take 1 p == "+" = i (read . drop 1 <$> num)
      | take 1 p == "-" = i (read <$> num)
      | otherwise = u (read <$> num)

tupleOrParenthesizedTerm :: (Monad m, Var v) => TermP v m
tupleOrParenthesizedTerm = label "tuple" $ do
  (spanAnn, tm) <- tupleOrParenthesized term DD.unitTerm pair
  pure $ tm {ABT.annotation = spanAnn}
  where
    pair t1 t2 =
      Term.app
        (ann t1 <> ann t2)
        ( Term.app
            (ann t1)
            (Term.constructor (ann t1 <> ann t2) (ConstructorReference DD.pairRef 0))
            t1
        )
        t2
