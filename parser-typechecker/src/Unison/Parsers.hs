module Unison.Parsers where

import Data.Text qualified as Text
import Unison.Builtin qualified as Builtin
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrintError (defaultWidth, prettyParseError)
import Unison.Symbol (Symbol)
import Unison.Syntax.FileParser qualified as FileParser
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import Unison.Util.Pretty qualified as Pr
import Unison.Var (Var)

unsafeGetRightFrom :: (Var v, Show v) => String -> Either (Parser.Err v) a -> a
unsafeGetRightFrom src =
  either (error . Pr.toANSI defaultWidth . prettyParseError src) id

parse ::
  (Monad m, Var v) =>
  Parser.P v m a ->
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) a)
parse p = Parser.run (Parser.root p)

parseTerm ::
  (Monad m, Var v) =>
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (Term v Ann))
parseTerm = parse TermParser.term

parseType ::
  (Monad m, Var v) =>
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (Type v Ann))
parseType = Parser.run (Parser.root TypeParser.valueType)

parseFile ::
  (Monad m, Var v) =>
  FilePath ->
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (UnisonFile v Ann))
parseFile filename s = Parser.run' (Parser.rootFile FileParser.file) s filename

readAndParseFile ::
  (MonadIO m, Var v) =>
  Parser.ParsingEnv m ->
  FilePath ->
  m (Either (Parser.Err v) (UnisonFile v Ann))
readAndParseFile penv fileName = do
  txt <- liftIO (readUtf8 fileName)
  let src = Text.unpack txt
  parseFile fileName src penv

unsafeParseTerm :: (Monad m, Var v) => String -> Parser.ParsingEnv m -> m (Term v Ann)
unsafeParseTerm s env =
  unsafeGetRightFrom s <$> parseTerm s env

unsafeReadAndParseFile ::
  Parser.ParsingEnv IO -> FilePath -> IO (UnisonFile Symbol Ann)
unsafeReadAndParseFile penv fileName = do
  txt <- readUtf8 fileName
  let str = Text.unpack txt
  unsafeGetRightFrom str <$> parseFile fileName str penv

unsafeParseFileBuiltinsOnly ::
  FilePath -> IO (UnisonFile Symbol Ann)
unsafeParseFileBuiltinsOnly =
  unsafeReadAndParseFile $
    Parser.ParsingEnv
      { uniqueNames = mempty,
        uniqueTypeGuid = \_ -> pure Nothing,
        names = Builtin.names,
        maybeNamespace = Nothing,
        localNamespacePrefixedTypesAndConstructors = mempty
      }

unsafeParseFile :: (Monad m) => String -> Parser.ParsingEnv m -> m (UnisonFile Symbol Ann)
unsafeParseFile s pEnv = unsafeGetRightFrom s <$> parseFile "" s pEnv
