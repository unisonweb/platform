module Unison.Codebase.Editor.HandleInput.ShowDefinition
  ( handleShowDefinition,
    showDefinitions,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.Input (OutputLocation (..), RelativeToFold (..), ShowDefinitionScope (..))
import Unison.Codebase.Editor.Output
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference, TermReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toVar)
import Unison.Syntax.NamePrinter (SyntaxText)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile (..), UnisonFile (..))
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Defns (Defns (..))
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as WatchKind

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition :: OutputLocation -> ShowDefinitionScope -> List.NonEmpty (HQ.HashQualified Name) -> Cli ()
handleShowDefinition outputLoc showDefinitionScope query = do
  env <- ask

  let hasAbsoluteQuery = any (any Name.isAbsolute) query
  (names, unbiasedPPED) <- case (hasAbsoluteQuery, showDefinitionScope) of
    -- TODO: We should instead print each definition using the names from its project-branch root.
    (True, _) -> do
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute (Branch.toNames root0)
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionGlobal) -> do
      -- TODO: Maybe rewrite to be properly global
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute $ Branch.toNames root0
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionLocal) -> do
      currentNames <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 currentNames) (suffixify currentNames)
      pure (currentNames, pped)
  let pped = PPED.biasTo (mapMaybe HQ.toName (List.NonEmpty.toList query)) unbiasedPPED
  Backend.DefinitionResults terms types misses <- do
    let nameSearch = NameSearch.makeNameSearch 10 names
    Cli.runTransaction (Backend.definitionsByName env.codebase nameSearch includeCycles Names.IncludeSuffixes (toList query))
  showDefinitions outputLoc pped terms types misses
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ _ -> Backend.IncludeCycles
        LatestFileLocation _ -> Backend.IncludeCycles

-- | Show the provided definitions to console or scratch file.
-- The caller is responsible for ensuring that the definitions include cycles if that's
-- the desired behavior.
showDefinitions ::
  OutputLocation ->
  PPED.PrettyPrintEnvDecl ->
  (Map Reference.Reference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))) ->
  ( Map
      Reference.Reference
      (DisplayObject () (Decl Symbol Ann))
  ) ->
  [HQ.HashQualified Name] ->
  Cli ()
showDefinitions outputLoc pped terms types misses = do
  env <- ask
  outputPath <- getOutputPath
  case outputPath of
    _ | null terms && null types -> pure ()
    Nothing -> do
      -- If we're writing to console we don't add test-watch syntax
      let isTest _ = False
      let isSourceFile = False
      -- No filepath, render code to console.
      let (renderedCodePretty, _numRendered) =
            renderCodePretty
              pped
              isSourceFile
              isTest
              terms
              types
              (Defns Set.empty Set.empty)
      Cli.respond $ DisplayDefinitions renderedCodePretty
    Just (fp, relToFold) -> do
      -- Of all the names we were asked to show, if this is a `WithinFold` showing, then exclude the ones that are
      -- already bound in the file
      excludeNames <-
        case relToFold of
          AboveFold -> pure (Defns Set.empty Set.empty)
          WithinFold ->
            use #latestTypecheckedFile <&> \case
              Nothing -> Defns Set.empty Set.empty
              Just (Left unisonFile) ->
                let boundTermNames = Map.keysSet unisonFile.terms
                    boundTestWatchNames =
                      Map.toList unisonFile.watches
                        & foldMap \case
                          (WatchKind.TestWatch, watches) -> Set.fromList (map (view _1) watches)
                          _ -> Set.empty
                    boundDataDeclNames = Map.keysSet unisonFile.dataDeclarationsId
                    boundEffectDeclNames = Map.keysSet unisonFile.effectDeclarationsId
                 in Defns
                      { terms = boundTermNames <> boundTestWatchNames,
                        types = boundDataDeclNames <> boundEffectDeclNames
                      }
              Just (Right typecheckedUnisonFile) ->
                let boundTermNames = foldMap (Set.fromList . map (view _1)) typecheckedUnisonFile.topLevelComponents'
                    boundTestWatchNames =
                      typecheckedUnisonFile.watchComponents & foldMap \case
                        (WatchKind.TestWatch, watches) -> Set.fromList (map (view _1) watches)
                        _ -> Set.empty
                 in Defns
                      { terms = boundTermNames <> boundTestWatchNames,
                        types = UnisonFile.typeNamespaceBindings typecheckedUnisonFile
                      }

      -- We build an 'isTest' check to prepend "test>" to tests in a scratch file.
      testRefs <-
        Cli.runTransaction do
          Codebase.filterTermsByReferenceIdHavingType
            env.codebase
            (DD.testResultListType mempty)
            (Map.keysSet terms & Set.mapMaybe Reference.toId)
      let isTest r = Set.member r testRefs
      let isSourceFile = True
      let (renderedCodePretty, numRendered) = renderCodePretty pped isSourceFile isTest terms types excludeNames
      when (numRendered > 0) do
        let renderedCodeText = Text.pack $ Pretty.toPlain 80 renderedCodePretty

        -- We set latestFile to be programmatically generated, if we
        -- are viewing these definitions to a file - this will skip the
        -- next update for that file (which will happen immediately)
        #latestFile ?= (fp, True)
        liftIO $
          env.writeSource (Text.pack fp) renderedCodeText case relToFold of
            AboveFold -> True
            WithinFold -> False
      Cli.respond $ LoadedDefinitionsToSourceFile fp numRendered

  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  where
    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Cli (Maybe (FilePath, RelativeToFold))
    getOutputPath =
      case outputLoc of
        ConsoleLocation -> pure Nothing
        FileLocation path relToFold -> pure (Just (path, relToFold))
        LatestFileLocation relToFold -> do
          loopState <- State.get
          pure case loopState ^. #latestFile of
            Nothing -> Just ("scratch.u", relToFold)
            Just (path, _) -> Just (path, relToFold)

    renderCodePretty pped isSourceFile isTest terms types excludeNames =
      let prettyTypes = prettyTypeDisplayObjects pped types excludeNames.types
          prettyTerms = prettyTermDisplayObjects pped isSourceFile isTest terms excludeNames.terms
       in ( Pretty.syntaxToColor (Pretty.sep "\n\n" (prettyTypes ++ prettyTerms)),
            length prettyTerms + length prettyTypes
          )

prettyTypeDisplayObjects ::
  PPED.PrettyPrintEnvDecl ->
  (Map Reference (DisplayObject () (DD.Decl Symbol Ann))) ->
  Set Symbol ->
  [Pretty SyntaxText]
prettyTypeDisplayObjects pped types excludeNames =
  types
    & Map.toList
    & mapMaybe
      ( \(ref, dt) -> do
          let hqName = PPE.typeName unsuffixifiedPPE ref
          whenJust (HQ.toName hqName) \name ->
            guard (Set.notMember (Name.toVar name) excludeNames)
          Just (hqName, ref, dt)
      )
    & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
    & map (Pretty.prettyType pped)
  where
    unsuffixifiedPPE = PPED.unsuffixifiedPPE pped

prettyTermDisplayObjects ::
  PPED.PrettyPrintEnvDecl ->
  Bool ->
  (TermReferenceId -> Bool) ->
  (Map Reference.TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))) ->
  Set Symbol ->
  [Pretty SyntaxText]
prettyTermDisplayObjects pped isSourceFile isTest terms excludeNames =
  terms
    & Map.toList
    & mapMaybe
      ( \(ref, dt) -> do
          let hqName = PPE.termName unsuffixifiedPPE (Referent.Ref ref)
          whenJust (HQ.toName hqName) \name ->
            guard (Set.notMember (Name.toVar name) excludeNames)
          Just (hqName, ref, dt)
      )
    & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
    & map (\t -> Pretty.prettyTerm pped isSourceFile (fromMaybe False . fmap isTest . Reference.toId $ (t ^. _2)) t)
  where
    unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
