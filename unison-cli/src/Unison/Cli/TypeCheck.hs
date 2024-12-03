module Unison.Cli.TypeCheck
  ( computeTypecheckingEnvironment,
    typecheckTerm,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.FileParsers qualified as FileParsers
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol (Symbol))
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

computeTypecheckingEnvironment ::
  FileParsers.ShouldUseTndr Sqlite.Transaction ->
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  UnisonFile Symbol Ann ->
  Sqlite.Transaction (Typechecker.Env Symbol Ann)
computeTypecheckingEnvironment shouldUseTndr codebase ambientAbilities unisonFile =
  FileParsers.computeTypecheckingEnvironment
    shouldUseTndr
    ambientAbilities
    (Codebase.typeLookupForDependencies codebase)
    unisonFile

typecheckTerm ::
  Codebase IO Symbol Ann ->
  Term Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Type Symbol Ann)
    )
typecheckTerm codebase tm = do
  let v = Symbol 0 (Var.Inference Var.Other)
  let file = UF.UnisonFileId mempty mempty (Map.singleton v (External, tm)) mempty
  typeLookup <- Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  let typecheckingEnv =
        Typechecker.Env
          { ambientAbilities = [],
            typeLookup,
            termsByShortname = Map.empty,
            topLevelComponents = Map.empty
          }
  pure $ fmap extract $ FileParsers.synthesizeFile typecheckingEnv file
  where
    extract tuf
      | [[(_, _, _, ty)]] <- UF.topLevelComponents' tuf = ty
      | otherwise = error "internal error: typecheckTerm"
