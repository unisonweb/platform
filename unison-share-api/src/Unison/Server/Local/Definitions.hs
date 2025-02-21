module Unison.Server.Local.Definitions
  ( prettyDefinitionsForHQName,
    termDefinitionByName,
    typeDefinitionByName,
  )
where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Map qualified as Map
import Data.Set.NonEmpty qualified as NESet
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Reference (TermReference, TypeReference)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as NS
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Server.Backend
import Unison.Server.Backend qualified as Backend
import Unison.Server.Doc qualified as Doc
import Unison.Server.Local qualified as Local
import Unison.Server.NameSearch (NameSearch)
import Unison.Server.NameSearch qualified as NS
import Unison.Server.NameSearch qualified as NameSearch
import Unison.Server.NameSearch.FromNames (makeNameSearch)
import Unison.Server.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (Width)

-- | Renders a definition for the given name or hash alongside its documentation.
prettyDefinitionsForHQName ::
  -- | The path representing the user's current perspective.
  -- Searches will be limited to definitions within this path, and names will be relative to
  -- this path.
  Path.Absolute ->
  -- | The root branch to use
  V2Branch.CausalBranch Sqlite.Transaction ->
  Maybe Width ->
  -- | Whether to suffixify bindings in the rendered syntax
  Suffixify ->
  -- | Runtime used to evaluate docs. This should be sandboxed if run on the server.
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  Backend IO DefinitionDisplayResults
prettyDefinitionsForHQName perspective shallowRoot renderWidth suffixifyBindings rt codebase perspectiveQuery = do
  result <- liftIO . Codebase.runTransaction codebase $ do
    shallowBranch <- V2Causal.value shallowRoot
    Local.relocateToNameRoot perspective perspectiveQuery shallowBranch
  (namesRoot, query) <- either throwError pure result
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our perspective but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,o
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current perspective,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query
  hqLength <- liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
  (localNamesOnly, unbiasedPPED) <- namesAtPathFromRootBranchHash codebase shallowRoot $ Path.unabsolute namesRoot
  let pped = PPED.biasTo biases unbiasedPPED
  let nameSearch = makeNameSearch hqLength localNamesOnly
  (DefinitionResults terms types misses) <- liftIO $ Codebase.runTransaction codebase do
    definitionsByName codebase nameSearch DontIncludeCycles Names.ExactName [query]
  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        docRefs <- Codebase.runTransaction codebase $ docsForDefinitionName codebase nameSearch Names.ExactName name
        renderDocRefs pped width codebase rt docRefs
          -- local server currently ignores doc eval errors
          <&> fmap \(hqn, h, doc, _errs) -> (hqn, h, doc)

  let fqnPPE = PPED.unsuffixifiedPPE pped
  typeDefinitions <-
    ifor (typesToSyntaxOf suffixifyBindings width pped (Map.asList_ . traversed) types) \ref tp -> do
      let hqTypeName = PPE.typeNameOrHashOnly fqnPPE ref
      docs <- liftIO $ (maybe (pure []) docResults (HQ.toName hqTypeName))
      mkTypeDefinition codebase pped width ref docs tp
  termDefinitions <-
    ifor (termsToSyntaxOf suffixifyBindings width pped (Map.asList_ . traversed) terms) \reference trm -> do
      let referent = Referent.Ref reference
      let hqTermName = PPE.termNameOrHashOnly fqnPPE referent
      docs <- liftIO $ (maybe (pure []) docResults (HQ.toName hqTermName))
      mkTermDefinition codebase pped width reference docs trm
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

-- | Find the term referenced by the given name and return a display object for it.
termDisplayObjectByName :: Codebase m Symbol Ann -> NameSearch Sqlite.Transaction -> Name -> Sqlite.Transaction (Maybe (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)))
termDisplayObjectByName codebase nameSearch name = runMaybeT do
  refs <- lift $ NameSearch.lookupRelativeHQRefs' (NS.termSearch nameSearch) NS.ExactName (HQ'.NameOnly name)
  ref <- fmap NESet.findMin . hoistMaybe $ NESet.nonEmptySet refs
  case ref of
    Referent.Ref r -> (r,) <$> lift (Backend.displayTerm codebase r)
    Referent.Con _ _ ->
      -- TODO: Should we error here or some other sensible thing rather than returning no
      -- result?
      empty

termDefinitionByName ::
  Codebase IO Symbol Ann ->
  PPED.PrettyPrintEnvDecl ->
  NameSearch Sqlite.Transaction ->
  Width ->
  Rt.Runtime Symbol ->
  Name ->
  Backend IO (Maybe TermDefinition)
termDefinitionByName codebase pped nameSearch width rt name = runMaybeT $ do
  let biasedPPED = PPED.biasTo [name] pped
  (ref, displayObject, docRefs) <- mapMaybeT (liftIO . Codebase.runTransaction codebase) $ do
    (ref, displayObject) <- MaybeT $ termDisplayObjectByName codebase nameSearch name
    docRefs <- lift $ Backend.docsForDefinitionName codebase nameSearch NS.ExactName name
    pure (ref, displayObject, docRefs)
  renderedDocs <-
    liftIO $
      renderDocRefs pped width codebase rt docRefs
        -- local server currently ignores doc eval errors
        <&> fmap \(hqn, h, doc, _errs) -> (hqn, h, doc)
  let (_ref, syntaxDO) = Backend.termsToSyntaxOf (Suffixify False) width pped id (ref, displayObject)
  lift $ Backend.mkTermDefinition codebase biasedPPED width ref renderedDocs syntaxDO

-- | Find the type referenced by the given name and return a display object for it.
typeDisplayObjectByName :: Codebase m Symbol Ann -> NameSearch Sqlite.Transaction -> Name -> Sqlite.Transaction (Maybe (TypeReference, DisplayObject () (DD.Decl Symbol Ann)))
typeDisplayObjectByName codebase nameSearch name = runMaybeT do
  refs <- lift $ NameSearch.lookupRelativeHQRefs' (NS.typeSearch nameSearch) NS.ExactName (HQ'.NameOnly name)
  ref <- fmap NESet.findMin . hoistMaybe $ NESet.nonEmptySet refs
  fmap (ref,) . lift $ Backend.displayType codebase ref

typeDefinitionByName ::
  Codebase IO Symbol Ann ->
  PPED.PrettyPrintEnvDecl ->
  NameSearch Sqlite.Transaction ->
  Width ->
  Rt.Runtime Symbol ->
  Name ->
  Backend IO (Maybe TypeDefinition)
typeDefinitionByName codebase pped nameSearch width rt name = runMaybeT $ do
  let biasedPPED = PPED.biasTo [name] pped
  (ref, displayObject, docRefs) <- mapMaybeT (liftIO . Codebase.runTransaction codebase) $ do
    (ref, displayObject) <- MaybeT $ typeDisplayObjectByName codebase nameSearch name
    docRefs <- lift $ Backend.docsForDefinitionName codebase nameSearch NS.ExactName name
    pure (ref, displayObject, docRefs)
  renderedDocs <-
    liftIO $
      renderDocRefs pped width codebase rt docRefs
        -- local server currently ignores doc eval errors
        <&> fmap \(hqn, h, doc, _errs) -> (hqn, h, doc)
  let (_ref, syntaxDO) = Backend.typesToSyntaxOf (Suffixify False) width pped id (ref, displayObject)
  lift $ Backend.mkTypeDefinition codebase biasedPPED width ref renderedDocs syntaxDO
