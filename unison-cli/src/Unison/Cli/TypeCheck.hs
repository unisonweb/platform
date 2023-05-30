module Unison.Cli.TypeCheck
  ( typecheck,
    typecheckHelper,
    typecheckFile,
    typecheckTerm,
  )
where

import Control.Monad.Reader (ask)
import Data.Text qualified as Text
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.FileParsers (parseAndSynthesizeFile, synthesizeFile')
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol (Symbol))
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

typecheck ::
  [Type Symbol Ann] ->
  NamesWithHistory ->
  Text ->
  (Text, [L.Token L.Lexeme]) ->
  Cli
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheck ambient names sourceName source =
  Cli.time "typecheck" do
    Cli.Env {codebase, generateUniqueName} <- ask
    uniqueName <- liftIO generateUniqueName
    (Cli.runTransaction . Result.getResult) $
      parseAndSynthesizeFile
        ambient
        (((<> Builtin.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
        (Parser.ParsingEnv uniqueName names)
        (Text.unpack sourceName)
        (fst source)

typecheckHelper ::
  (MonadIO m) =>
  Codebase IO Symbol Ann ->
  IO Parser.UniqueName ->
  [Type Symbol Ann] ->
  NamesWithHistory ->
  Text ->
  (Text, [L.Token L.Lexeme]) ->
  m
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheckHelper codebase generateUniqueName ambient names sourceName source = do
  uniqueName <- liftIO generateUniqueName
  (liftIO . Codebase.runTransaction codebase . Result.getResult) $
    parseAndSynthesizeFile
      ambient
      (((<> Builtin.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
      (Parser.ParsingEnv uniqueName names)
      (Text.unpack sourceName)
      (fst source)

typecheckTerm ::
  Term Symbol Ann ->
  Cli
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Type Symbol Ann)
    )
typecheckTerm tm = do
  Cli.Env {codebase} <- ask
  let v = Symbol 0 (Var.Inference Var.Other)
  liftIO $
    fmap extract
      <$> Codebase.runTransaction codebase (typecheckFile' codebase [] (UF.UnisonFileId mempty mempty [(v, tm)] mempty))
  where
    extract tuf
      | [[(_, _, ty)]] <- UF.topLevelComponents' tuf = ty
      | otherwise = error "internal error: typecheckTerm"

typecheckFile' ::
  Codebase m Symbol Ann ->
  [Type Symbol Ann] ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckFile' codebase ambient file = do
  typeLookup <-
    (<> Builtin.typeLookup)
      <$> Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  pure $ synthesizeFile' ambient typeLookup file

typecheckFile ::
  Codebase m Symbol Ann ->
  [Type Symbol Ann] ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either Names (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheckFile codebase ambient file =
  fmap Right <$> typecheckFile' codebase ambient file
