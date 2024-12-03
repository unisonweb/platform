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
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchDiff qualified as BranchDiff
import Unison.Codebase.Editor.Output.BranchDiff qualified as OBranchDiff
import Unison.DataDeclaration qualified as DD
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    hqLength <- Cli.runTransaction Codebase.hashLength
    diff <- liftIO (BranchDiff.diff0 before after)
    names <- Cli.currentNames <&> \currentNames -> currentNames <> Branch.toNames before <> Branch.toNames after
    let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

    let suffixifiedPPE = PPED.suffixifiedPPE pped
    fmap (suffixifiedPPE,) do
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
