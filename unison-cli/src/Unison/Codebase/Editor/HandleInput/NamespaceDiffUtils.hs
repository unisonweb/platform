-- | Helpers/utils that have to do with namespace diffs.
module Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils
  ( diffHelper,
  )
where

import Control.Monad.Reader (ask)
import Data.Map qualified as Map
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils (prettyPrintEnvDecl)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchDiff qualified as BranchDiff
import Unison.Codebase.Editor.Output.BranchDiff qualified as OBranchDiff
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as DD
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo)
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    rootBranch <- Cli.getRootBranch
    currentPath <- Cli.getCurrentPath
    hqLength <- Cli.runTransaction Codebase.hashLength
    diff <- liftIO (BranchDiff.diff0 before after)
    let (_parseNames, prettyNames0, _local) = Backend.namesForBranch rootBranch (Backend.AllNames $ Path.unabsolute currentPath)
    ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (NamesWithHistory prettyNames0 mempty)
    fmap (ppe,) do
      OBranchDiff.toOutput
        (Cli.runTransaction . Codebase.getTypeOfReferent codebase)
        (Cli.runTransaction . declOrBuiltin codebase)
        hqLength
        (Branch.toNames before)
        (Branch.toNames after)
        diff

declOrBuiltin :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (Maybe (DD.DeclOrBuiltin Symbol Ann))
declOrBuiltin codebase r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> Codebase.getTypeDeclaration codebase id
