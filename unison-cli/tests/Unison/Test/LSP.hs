{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.LSP (test) where

import Crypto.Random qualified as Random
import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as Map
import Data.String.Here.Uninterpolated (here)
import Data.Text
import Data.Text qualified as Text
import EasyTest
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.IO.Temp qualified as Temp
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls (unitRef)
import Unison.Cli.TypeCheck qualified as Typecheck
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Init qualified as Codebase.Init
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.FileParsers qualified as FileParsers
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.FileAnalysis.UnusedBindings qualified as UnusedBindings
import Unison.LSP.Queries qualified as LSPQ
import Unison.Lexer.Pos qualified as Lexer
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Recursion

test :: Test ()
test = do
  scope "annotations" $
    tests
      [ refFinding,
        annotationNesting
      ]
  scope "diagnostics" $
    tests
      [ unusedBindingLocations,
        typeMismatchLocations
      ]

trm :: Term.F Symbol () () (ABT.Term (Term.F Symbol () ()) Symbol ()) -> LSPQ.SourceNode ()
trm = LSPQ.TermNode . ABT.tm

typ :: Type.F (ABT.Term Type.F Symbol ()) -> LSPQ.SourceNode ()
typ = LSPQ.TypeNode . ABT.tm

pat :: Pattern.Pattern () -> LSPQ.SourceNode ()
pat = LSPQ.PatternNode

-- | Test that we can find the correct reference for a given cursor position.
refFinding :: Test ()
refFinding =
  scope "refs" . tests . fmap makeNodeSelectionTest $
    [ ( "Binary Op lhs",
        [here|term = tr^ue && false|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Binary Op rhs",
        [here|term = true && fa^lse|],
        True,
        trm (Term.Boolean False)
      ),
      ( "Custom Op lhs",
        [here|
a &&& b = a && b
term = tr^ue &&& false
|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Simple type annotation on non-typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = "this won't typecheck"
|],
        False,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Simple type annotation on typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = This
|],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations within bindings for do-block elements",
        [here|
term = do
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within bindings for let-block elements",
        [here|
term = let
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within actions for let-block elements",
        [here|
term = let
  first = false
  first && tr^ue
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for blocks with destructuring binds",
        [here|
structural type Identity a = Identity a
term = let
  (Identity a) = Identity tr^ue
  a
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for destructuring tuples (they have a special parser)",
        [here|
term = let
  (true, fal^se)
        |],
        True,
        trm (Term.Boolean False)
      ),
      ( "Test annotations within pattern binds",
        [here|
term = let
  (third, (^)) = (false, ())
  true
  |],
        True,
        pat (Pattern.Constructor () (ConstructorReference unitRef 0) [])
      ),
      ( "Test annotations for types with arrows",
        [here|
structural type Thing = This | That

term : Thing -> Thing -> Thi^ng
term a b = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for types with effects",
        [here|
unique ability Foo a where
    foo : a

unique ability Bar b where
    bar : b

structural type Thing = This | That

term : (Thing -> {Foo a, Bar b} Th^ing) -> {Foo a, Bar b} Thing
term f = f This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for effects themselves",
        [here|
structural ability Foo a where
    foo : a

structural type Thing = This | That

term : () -> {F^oo a} Thing
term _ = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#h4uhcub76va4tckj1iccnsb07rh0fhgpigqapb4jh5n07s0tugec4nm2vikuv973mab7oh4ne07o6armcnnl7mbfjtb4imphgrjgimg"))
      ),
      ( "Test annotations for types with arrows",
        [here|
structural type Thing = This | That

term : Thing -> Thing -> Thi^ng
term a b = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for types with effects",
        [here|
unique ability Foo a where
    foo : a

unique ability Bar b where
    bar : b

structural type Thing = This | That

term : (Thing -> {Foo a, Bar b} Th^ing) -> {Foo a, Bar b} Thing
term f = f This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for effects themselves",
        [here|
structural ability Foo a where
    foo : a

structural type Thing = This | That

term : () -> {F^oo a} Thing
term _ = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#h4uhcub76va4tckj1iccnsb07rh0fhgpigqapb4jh5n07s0tugec4nm2vikuv973mab7oh4ne07o6armcnnl7mbfjtb4imphgrjgimg"))
      ),
      ( "Test annotations for blocks recursive binds",
        [here|
term = let
  f x = g true && x
  g y = f fal^se && y
  f true
        |],
        True,
        trm (Term.Boolean False)
      )
    ]

-- | Test helper which lets you specify a cursor position inline with source text as a '^'.
extractCursor :: Text -> Test (Lexer.Pos, Text)
extractCursor txt =
  case splitOnDelimiter '^' txt of
    Just (before, pos, after) -> pure (pos, before <> after)
    _ -> crash "expected exactly one cursor"

-- | Splits a text on a delimiter, returning the text before and after the delimiter, along with the position of the delimiter.
--
-- >>> splitOnDelimiter '^' "foo b^ar baz"
-- Just ("foo b",Pos {line = 1, column = 5},"ar baz")
splitOnDelimiter :: Char -> Text -> Maybe (Text, Lexer.Pos, Text)
splitOnDelimiter sym txt =
  case second Text.uncons $ Text.breakOn (Text.singleton sym) txt of
    (_before, Nothing) -> Nothing
    (before, Just (_delim, after)) ->
      let col = (Text.length $ Text.takeWhileEnd (/= '\n') before)
          line = Text.count "\n" before + 1
       in Just (before, Lexer.Pos line col, after)

-- | Test helper which lets you specify a relevant block of source inline using specified delimiters
--
-- >>> extractDelimitedBlocks ('{', '}') "foo {bar} baz"
-- Just ("foo bar baz",[(Ann {start = Pos {line = 1, column = 5}, end = Pos {line = 1, column = 8}},"bar")])
--
-- >>> extractDelimitedBlocks ('{', '}') "term =\n  {foo} = 12345"
-- Just ("term =\n  foo = 12345",[(Ann {start = Pos {line = 2, column = 3}, end = Pos {line = 2, column = 6}},"foo")])
--
-- >>> extractDelimitedBlocks ('{', '}') "term =\n  {foo} = {12345} + 10"
-- Just ("term =\n  foo = 12345 + 10",[(Ann {start = Pos {line = 2, column = 3}, end = Pos {line = 2, column = 6}},"foo"),(Ann {start = Pos {line = 3, column = 4}, end = Pos {line = 3, column = 9}},"12345")])
extractDelimitedBlocks :: (Char, Char) -> Text -> Maybe (Text {- entire source text with the delimiters stripped -}, [(Ann {- ann spanning the inside of the delimiters -}, Text {- Text within the delimiters -})])
extractDelimitedBlocks (startDelim, endDelim) txt =
  extractDelimitedBlocksHelper mempty txt
  where
    extractDelimitedBlocksHelper :: Lexer.Pos -> Text -> Maybe (Text, [(Ann, Text)])
    extractDelimitedBlocksHelper offset txt = do
      (beforeStart, startPos, afterStart) <- splitOnDelimiter startDelim txt
      (beforeEnd, endPos, afterEnd) <- splitOnDelimiter endDelim (beforeStart <> afterStart)
      let ann = Ann (offset <> startPos) (offset <> endPos)
      case extractDelimitedBlocksHelper endPos afterEnd of
        Nothing -> pure (beforeEnd <> afterEnd, [(ann, Text.takeWhile (/= endDelim) afterStart)])
        Just (cleanSrc, splits) -> pure $ (beforeEnd <> cleanSrc, (ann, Text.takeWhile (/= endDelim) afterStart) : splits)

makeNodeSelectionTest :: (String, Text, Bool, LSPQ.SourceNode ()) -> Test ()
makeNodeSelectionTest (name, testSrc, testTypechecked, expected) = scope name $ do
  (pos, src) <- extractCursor testSrc
  (pf, mayTypecheckedFile) <- typecheckSrc name src
  scope "parsed file" $ do
    let pfResult =
          UF.terms pf
            & Map.toList
            & firstJust \(_v, (_fileAnn, trm)) ->
              LSPQ.findSmallestEnclosingNode pos trm
    expectEqual (Just expected) (void <$> pfResult)

  when testTypechecked $
    scope "typechecked file" $ do
      tf <- either (\notes -> crash ("Failed to typecheck: " ++ show notes)) pure mayTypecheckedFile
      let tfResult =
            UF.hashTermsId tf
              & toList
              & firstJust \(_fileAnn, _refId, _wk, trm, _typ) ->
                LSPQ.findSmallestEnclosingNode pos trm
      expectEqual (Just expected) (void <$> tfResult)

-- | Tests which assert that the annotation for each ABT node spans at least the span of
-- its children, i.e. all child annotations are contained within the annotation of their parent.
annotationNesting :: Test ()
annotationNesting =
  scope "nesting" . tests . fmap annotationNestingTest $
    [ ( "let blocks",
        [here|
term = let
  x = true
  y = false
  true && false
|]
      ),
      ( "let-rec blocks",
        [here|
term = let
  x a = a && y true
  y b = b && x true
  x true && y true
|]
      ),
      ( "function bindings",
        [here|
term x y = x && y
|]
      )
    ]

annotationNestingTest :: (String, Text) -> Test ()
annotationNestingTest (name, src) = scope name do
  (_, maytf) <- typecheckSrc name src
  tf <- either (\notes -> crash ("Failed to typecheck: " ++ show notes)) pure maytf
  UF.hashTermsId tf
    & toList
    & traverse_ \(_fileAnn, _refId, _wk, trm, _typ) ->
      assertAnnotationsAreNested trm

-- | Asserts that for all nodes in the provided ABT EXCEPT Abs nodes, the annotations of all child nodes are
-- within the span of the parent node.
assertAnnotationsAreNested :: forall f. (Foldable f, Functor f, Show (f (Either String Ann))) => ABT.Term f Symbol Ann -> Test ()
assertAnnotationsAreNested term = do
  case cata alg term of
    Right _ -> pure ()
    Left err -> crash err
  where
    alg :: Algebra (ABT.Term' f Symbol Ann) (Either String Ann)
    alg (ABT.Term' _ ann abt) = do
      childSpan <- abt & foldMapM id
      case abt of
        -- Abs nodes are the only nodes whose annotations are allowed to not contain their children,
        -- they represet the location of the variable being bound instead. Ideally we'd have a separate child
        -- node for that, but we can't add it without editing the ABT or Term types.
        ABT.Abs _ _ ->
          pure (ann <> childSpan)
        _ -> do
          case ann `Ann.encompasses` childSpan of
            -- one of the annotations isn't in the file, don't bother checking.
            Nothing -> pure (ann <> childSpan)
            Just isInFile
              | isInFile -> pure ann
              | otherwise -> Left $ "Containment breach: children aren't contained with the parent:" <> show (ann, abt)

typecheckSrc ::
  String ->
  Text ->
  Test
    ( UF.UnisonFile Symbol Ann,
      Either
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckSrc name src = do
  result <-
    withTestCodebase \codebase -> do
      uniqueName <- Parser.uniqueBase32Namegen <$> Random.getSystemDRG
      let ambientAbilities = []
      let parseNames = mempty
      let parsingEnv =
            Parser.ParsingEnv
              { uniqueNames = uniqueName,
                uniqueTypeGuid = \_ -> pure Nothing,
                names = parseNames,
                maybeNamespace = Nothing,
                localNamespacePrefixedTypesAndConstructors = mempty
              }
      Codebase.runTransaction codebase do
        Parsers.parseFile name (Text.unpack src) parsingEnv >>= \case
          Left err -> pure (Left ("Failed to parse: " ++ show err))
          Right unisonFile -> do
            typecheckingEnv <-
              Typecheck.computeTypecheckingEnvironment
                (FileParsers.ShouldUseTndr'Yes parsingEnv)
                codebase
                ambientAbilities
                unisonFile
            typecheckingResult <-
              Result.runResultT (FileParsers.synthesizeFile typecheckingEnv unisonFile) <&> \case
                (Nothing, notes) -> Left notes
                (Just typecheckedUnisonFile, _) -> Right typecheckedUnisonFile
            pure (Right (unisonFile, typecheckingResult))
  case result of
    Left err -> crash err
    Right val -> pure val

withTestCodebase ::
  (Codebase IO Symbol Ann -> IO r) -> Test r
withTestCodebase action = do
  r <- io do
    tmp <- Temp.getCanonicalTemporaryDirectory
    tmpDir <- Temp.createTempDirectory tmp "lsp-test"
    Codebase.Init.withCreatedCodebase SC.init "lsp-test" tmpDir SC.DontLock action
  either (crash . show) pure r

makeUnusedBindingRangeTest :: (String, Text) -> Test ()
makeUnusedBindingRangeTest (testName, testSrc) = scope testName $ do
  (cleanSrc, ranges) <- case extractDelimitedBlocks ('«', '»') testSrc of
    Nothing -> pure (testSrc, [])
    Just (cleanSrc, ranges) -> pure (cleanSrc, ranges)
  (pf, _mayTypecheckedFile) <- typecheckSrc testName cleanSrc
  UF.terms pf
    & Map.elems
    & \case
      [(_a, trm)] -> do
        let diags = UnusedBindings.analyseTerm (LSP.Uri "test") trm
        matchDiagnostics ranges diags
      _ -> crash "Expected exactly one term"

makeTypecheckerDiagnosticRangeTest :: (String, Text) -> Test ()
makeTypecheckerDiagnosticRangeTest (testName, testSrc) = scope testName $ do
  (cleanSrc, ranges) <- case extractDelimitedBlocks ('«', '»') testSrc of
    Nothing -> pure (testSrc, [])
    Just (cleanSrc, ranges) -> pure (cleanSrc, ranges)
  (_pf, tf) <- typecheckSrc testName cleanSrc
  case tf of
    Left notes -> do
      let codebase = error "unexpected use of codebase"
      let ppe = PPE.empty
      (diags, _codeActions) <- FileAnalysis.analyseNotes codebase (LSP.Uri "test") ppe "test" notes
      matchDiagnostics ranges diags
    Right _ -> crash "Expected typechecking to fail"

matchDiagnostics :: [(Ann, Text)] -> [LSP.Diagnostic] -> Test ()
matchDiagnostics ranges diags = case (ranges, diags) of
  ([], []) -> pure ()
  ([], _ : _) -> crash $ "Got diagnostics that weren't matched: " <> show diags
  (_ : _, []) -> crash $ "Expected diagnostics that weren't provided" <> show ranges
  (range@(ann, _src) : rest, diags) ->
    diags
      & popFind
        ( \diag ->
            let expectedRange = Cv.annToRange ann
                actualRange = Just (diag ^. LSP.range)
             in (expectedRange /= actualRange)
        )
      & \case
        Nothing -> crash $ "Expected diagnostic not found" <> show range <> ", remaining diagnostics: " <> show diags
        Just (_, diags) -> matchDiagnostics rest diags
  where
    popFind :: (a -> Bool) -> [a] -> Maybe (a, [a])
    popFind p = \case
      [] -> Nothing
      x : xs -> if p x then Just (x, xs) else second (x :) <$> popFind p xs

unusedBindingLocations :: Test ()
unusedBindingLocations =
  scope "unused bindings" . tests . fmap makeUnusedBindingRangeTest $
    [ ( "Unused binding in let block",
        [here|term =
  usedOne = true
  «unused = "unused"»
  usedTwo = false
  usedOne && usedTwo
        |]
      ),
      ( "Unused argument",
        [here|term «unused» = 1|]
      ),
      ( "Unused binding in cases block",
        [here|term = cases
  -- Note: the diagnostic _should_ only wrap the unused bindings, but right now it just wraps the whole pattern.
  («unused, used»)
    | used > 0 -> true
    | otherwise -> false
    |]
      ),
      ( "Ignored unused binding in cases block shouldn't error",
        [here|term = cases
  (used, _ignored) -> used
    |]
      )
    ]

typeMismatchLocations :: Test ()
typeMismatchLocations =
  scope "type mismatch locations" . tests . fmap makeTypecheckerDiagnosticRangeTest $
    [ ( "Should highlight the actual incorrect terminal expression in a let block",
        [here|
type Foo = Foo
term : Foo
term =
  _blah = true
  _foo = true
  _baz = true
  «"incorrect"»
        |]
      ),
      ( "Should highlight the actual incorrect terminal expression in an if-block",
        [here|
type Foo = Foo
term : Foo
term = if true
  then «"wrong"»
  else «"also wrong"»
|]
      ),
      ( "Should highlight the handler of handle expressions",
        [here|
type Foo = Foo
term : Foo
term =
  const a b = a
  handle "" with const «"wrong"»
|]
      )
    ]
