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
import Unison.LSP.FileAnalysis.UnusedBindings qualified as UnusedBindings
import Unison.LSP.Queries qualified as LSPQ
import Unison.Lexer.Pos qualified as Lexer
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Pattern qualified as Pattern
import Unison.Prelude
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
      [ unusedBindingLocations
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
    Nothing -> crash "expected exactly one cursor"
    Just (before, pos, after) -> pure (pos, before <> after)

-- | Splits a text on a delimiter, returning the text before and after the delimiter, along with the position of the delimiter.
--
-- >>> splitOnDelimiter '^' "foo b^ar baz"
-- Just ("foo b",Pos {line = 0, column = 5},"ar baz")
splitOnDelimiter :: Char -> Text -> Maybe (Text, Lexer.Pos, Text)
splitOnDelimiter sym txt =
  case Text.splitOn (Text.singleton sym) txt of
    [before, after] ->
      let col = (Text.length $ Text.takeWhileEnd (/= '\n') before) + 1
          line = Text.count "\n" before + 1
       in Just $ (before, Lexer.Pos line col, after)
    _ -> Nothing

-- | Test helper which lets you specify a cursor position inline with source text as a '^'.
--
-- >>> extractDelimitedBlock ('{', '}') "foo {bar} baz"
-- Just (Ann {start = Pos {line = 1, column = 4}, end = Pos {line = 1, column = 7}},"bar","foo bar baz")
--
-- >>> extractDelimitedBlock ('{', '}') "term =\n  {foo} = 12345"
-- Just (Ann {start = Pos {line = 2, column = 2}, end = Pos {line = 2, column = 5}},"foo","term =\n  foo = 12345")
extractDelimitedBlock :: (Char, Char) -> Text -> Maybe (Ann {- ann spanning the inside of the delimiters -}, Text {- Text within the delimiters -}, Text {- entire source text with the delimiters stripped -})
extractDelimitedBlock (startDelim, endDelim) txt = do
  (beforeStart, startPos, afterStart) <- splitOnDelimiter startDelim txt
  (beforeEnd, endPos, afterEnd) <- splitOnDelimiter endDelim (beforeStart <> afterStart)
  let ann = Ann startPos endPos
  pure (ann, Text.takeWhile (/= endDelim) afterStart, beforeEnd <> afterEnd)

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

makeDiagnosticRangeTest :: (String, Text) -> Test ()
makeDiagnosticRangeTest (testName, testSrc) = scope testName $ do
  let (cleanSrc, mayExpectedDiagnostic) = case extractDelimitedBlock ('«', '»') testSrc of
        Nothing -> (testSrc, Nothing)
        Just (ann, block, clean) -> (clean, Just (ann, block))
  (pf, _mayTypecheckedFile) <- typecheckSrc testName cleanSrc
  UF.terms pf
    & Map.elems
    & \case
      [(_a, trm)] -> do
        case (mayExpectedDiagnostic, UnusedBindings.analyseTerm (LSP.Uri "test") trm) of
          (Just (ann, _block), [diag]) -> do
            let expectedRange = Cv.annToRange ann
            let actualRange = Just (diag ^. LSP.range)
            when (expectedRange /= actualRange) do
              crash $ "Expected diagnostic at range: " <> show expectedRange <> ", got: " <> show actualRange
          (Nothing, []) -> pure ()
          (expected, actual) -> case expected of
            Nothing -> crash $ "Expected no diagnostics, got: " <> show actual
            Just _ -> crash $ "Expected exactly one diagnostic, but got " <> show actual
      _ -> crash "Expected exactly one term"

unusedBindingLocations :: Test ()
unusedBindingLocations =
  scope "unused bindings" . tests . fmap makeDiagnosticRangeTest $
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
