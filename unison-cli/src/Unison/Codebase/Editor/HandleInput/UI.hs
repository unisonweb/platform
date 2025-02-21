module Unison.Codebase.Editor.HandleInput.UI (openUI) where

import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Branch.Type qualified as V2Branch
import U.Codebase.Reference qualified as V2 (Reference)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Referent qualified as V2.Referent
import U.Codebase.Sqlite.Project qualified as Project
import U.Codebase.Sqlite.ProjectBranch qualified as ProjectBranch
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorType qualified as ConstructorType
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (ProjectAndBranch))
import Unison.Referent qualified as Referent
import Unison.Server.CodebaseServer qualified as Server
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Web.Browser (openBrowser)

openUI :: Path.Path' -> Cli ()
openUI path' = do
  Cli.Env {serverBaseUrl} <- ask
  defnPath <- Cli.resolvePath' path'
  pp <- Cli.getCurrentProjectPath
  whenJust serverBaseUrl \url -> do
    openUIForProject url pp (defnPath ^. PP.absPath_)

openUIForProject :: Server.BaseUrl -> PP.ProjectPath -> Path.Absolute -> Cli ()
openUIForProject url pp@(PP.ProjectPath project projectBranch perspective) defnPath = do
  mayDefinitionRef <- getDefinitionRef perspective
  let projectBranchNames = bimap Project.name ProjectBranch.name (ProjectAndBranch project projectBranch)
  _success <- liftIO . openBrowser . Text.unpack $ Server.urlFor (Server.ProjectBranchUI projectBranchNames perspective mayDefinitionRef) url
  pure ()
  where
    -- If the provided ui path matches a definition, find it.
    getDefinitionRef :: Path.Absolute -> Cli (Maybe (Server.DefinitionReference))
    getDefinitionRef perspective = runMaybeT $ do
      Cli.Env {codebase} <- lift ask
      pathToDefinitionNamespace <- hoistMaybe $ Path.ascend defnPath
      let defnNamespaceProjectPath = pp & PP.absPath_ .~ pathToDefinitionNamespace
      namespaceBranch <- lift . Cli.runTransaction $ Codebase.getShallowBranchAtProjectPath defnNamespaceProjectPath
      fqn <- hoistMaybe $ do
        pathFromPerspective <- List.stripPrefix (Path.toList (Path.unabsolute perspective)) (Path.toList $ Path.unabsolute defnPath)
        Path.toName . Path.fromList $ pathFromPerspective
      def <- MaybeT $ getTermOrTypeRef codebase namespaceBranch fqn
      pure def

getTermOrTypeRef :: Codebase m Symbol Ann -> V2Branch.Branch n -> Name -> Cli (Maybe Server.DefinitionReference)
getTermOrTypeRef codebase namespaceBranch fqn = runMaybeT $ do
  let nameSeg = Name.lastSegment fqn
  let terms = do
        matchingTerms <- hoistMaybe $ Map.lookup nameSeg (V2Branch.terms namespaceBranch)
        oneTerm <- hoistMaybe $ Set.lookupMin $ Map.keysSet matchingTerms
        lift $ Cli.runTransaction (toTermReference codebase fqn oneTerm)
  let types = do
        matchingTypes <- hoistMaybe $ Map.lookup nameSeg (V2Branch.types namespaceBranch)
        oneType <- hoistMaybe $ Set.lookupMin $ Map.keysSet matchingTypes
        pure (toTypeReference fqn oneType)
  terms <|> types

toTypeReference :: Name -> V2.Reference -> Server.DefinitionReference
toTypeReference name reference =
  Server.TypeReference $
    HQ.fromNamedReference name (Conversions.reference2to1 reference)

toTermReference :: Codebase m Symbol Ann -> Name -> V2.Referent -> Sqlite.Transaction Server.DefinitionReference
toTermReference codebase name referent = do
  case referent of
    V2.Referent.Ref reference ->
      pure $
        Server.TermReference $
          HQ.fromNamedReference name (Conversions.reference2to1 reference)
    V2.Referent.Con _ _ -> do
      v1Referent <- Conversions.referent2to1 (Codebase.getDeclType codebase) referent
      let hq = HQ.fromNamedReferent name v1Referent

      pure case v1Referent of
        Referent.Con _ ConstructorType.Data ->
          Server.DataConstructorReference hq
        Referent.Con _ ConstructorType.Effect ->
          Server.AbilityConstructorReference hq
        Referent.Ref _ -> error "Impossible! *twirls mustache*"
