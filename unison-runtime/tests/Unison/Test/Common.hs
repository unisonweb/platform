module Unison.Test.Common
  ( hqLength,
    t,
    tm,
    parseAndSynthesizeAsFile,
    parsingEnv,
  )
where

import Control.Monad.Writer (tell)
import Data.Functor.Identity (Identity (..))
import Data.Sequence (Seq)
import Text.Megaparsec.Error qualified as MPE
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as B
import Unison.FileParsers qualified as FP
import Unison.Parser.Ann (Ann (..))
import Unison.Parsers qualified as Parsers
import Unison.PrintError (prettyParseError)
import Unison.Result (Note, Result)
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.Pretty qualified as Pr
import Unison.Var (Var)

type Term v = Term.Term v Ann

type Type v = Type.Type v Ann

hqLength :: Int
hqLength = 10

t :: String -> Type Symbol
t s =
  ABT.amap (const Intrinsic)
    -- . either (error . show ) id
    -- . Type.bindSomeNames B.names0
    . either (error . showParseError s) tweak
    . runIdentity
    $ Parser.run (Parser.root TypeParser.valueType) s parsingEnv
  where
    tweak = Type.generalizeLowercase mempty

tm :: String -> Term Symbol
tm s =
  either (error . showParseError s) id
    -- . Term.bindSomeNames mempty B.names0
    -- . either (error . showParseError s) id
    . runIdentity
    $ Parser.run (Parser.root TermParser.term) s parsingEnv

showParseError ::
  (Var v) =>
  String ->
  MPE.ParseError Parser.Input (Parser.Error v) ->
  String
showParseError s = Pr.toANSI 60 . prettyParseError s

parseAndSynthesizeAsFile ::
  [Type Symbol] ->
  FilePath ->
  String ->
  Result
    (Seq (Note Symbol Ann))
    (Either (UnisonFile Symbol Ann) (TypecheckedUnisonFile Symbol Ann))
parseAndSynthesizeAsFile ambient filename s = do
  file <- Result.fromParsing (runIdentity (Parsers.parseFile filename s parsingEnv))
  let typecheckingEnv =
        runIdentity $
          FP.computeTypecheckingEnvironment
            (FP.ShouldUseTndr'Yes parsingEnv)
            ambient
            (\_deps -> pure B.typeLookup)
            file
  case FP.synthesizeFile typecheckingEnv file of
    Result.Result notes Nothing -> tell notes >> pure (Left file)
    Result.Result _ (Just typecheckedFile) -> pure (Right typecheckedFile)

parsingEnv :: Parser.ParsingEnv Identity
parsingEnv =
  Parser.ParsingEnv
    { uniqueNames = mempty,
      uniqueTypeGuid = \_ -> pure Nothing,
      names = B.names,
      maybeNamespace = Nothing,
      localNamespacePrefixedTypesAndConstructors = mempty
    }
