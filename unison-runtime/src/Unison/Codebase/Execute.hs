-- | Execute a computation of type '{IO} () that has been previously added to
-- the codebase, without setting up an interactive environment.
--
-- This allows one to run standalone applications implemented in the Unison
-- language.
module Unison.Codebase.Execute
  ( execute,
    codebaseToCodeLookup,
  )
where

import Control.Exception (finally)
import Control.Monad.Except
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Builtin qualified as Builtin
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.MainTerm (getMainTerm)
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Type (Codebase (..))
import Unison.HashQualified qualified as HQ
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Parser
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Pretty qualified as P

execute ::
  Codebase.Codebase IO Symbol Ann ->
  Runtime Symbol ->
  PP.ProjectPathNames ->
  IO (Either Runtime.Error ())
execute codebase runtime mainPath =
  (`finally` Runtime.terminate runtime) . runExceptT $ do
    (project, branch) <- ExceptT $ (Codebase.runTransactionWithRollback codebase) \rollback -> do
      project <- Q.loadProjectByName mainPath.project `whenNothingM` rollback (Left . P.text $ ("Project not found: " <> into @Text mainPath.project))
      branch <- Q.loadProjectBranchByName project.projectId mainPath.branch `whenNothingM` rollback (Left . P.text $ ("Branch not found: " <> into @Text mainPath.branch))
      pure . Right $ (project, branch)
    projectRootNames <- fmap (Branch.toNames . Branch.head) . liftIO $ Codebase.expectProjectBranchRoot codebase project.projectId branch.branchId
    let loadTypeOfTerm = Codebase.getTypeOfTerm codebase
    let mainType = Runtime.mainType runtime
    mainName <- case Path.toName (mainPath ^. PP.path_) of
      Just n -> pure (HQ.NameOnly n)
      Nothing -> throwError ("Path must lead to an executable term: " <> P.text (Path.toText (PP.path mainPath)))

    mt <- liftIO $ Codebase.runTransaction codebase $ getMainTerm loadTypeOfTerm projectRootNames mainName mainType
    case mt of
      MainTerm.NotFound s -> throwError ("Not found: " <> P.text (HQ.toText s))
      MainTerm.BadType s _ -> throwError (P.text (HQ.toText s) <> " is not of type '{IO} ()")
      MainTerm.Success _ tm _ -> do
        let codeLookup = codebaseToCodeLookup codebase
            ppe = PPE.empty
        (liftIO $ Runtime.evaluateTerm codeLookup ppe runtime tm) >>= \case
          Left err -> throwError err
          Right _ -> pure ()

codebaseToCodeLookup :: (MonadIO m) => Codebase m Symbol Parser.Ann -> CL.CodeLookup Symbol m Parser.Ann
codebaseToCodeLookup c =
  CL.CodeLookup goGetTerm goGetTypeOfTerm goGetTypeDecl
    <> Builtin.codeLookup
    <> IOSource.codeLookupM
  where
    goGetTerm = (Codebase.runTransaction c . getTerm c)
    goGetTypeOfTerm = (Codebase.runTransaction c . getTypeOfTermImpl c)
    goGetTypeDecl = (Codebase.runTransaction c . getTypeDeclaration c)
