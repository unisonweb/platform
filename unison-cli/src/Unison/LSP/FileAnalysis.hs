{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.FileAnalysis where

import Control.Lens
import Control.Monad.Reader
import Crypto.Random qualified as Random
import Data.Align (alignWith)
import Data.Foldable
import Data.IntervalMap.Lazy (IntervalMap)
import Data.IntervalMap.Lazy qualified as IM
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These
import Data.Zip qualified as Zip
import Language.LSP.Protocol.Lens (HasCodeAction (codeAction), HasIsPreferred (isPreferred), HasRange (range), HasUri (uri))
import Language.LSP.Protocol.Lens qualified as LSPTypes
import Language.LSP.Protocol.Types
  ( Diagnostic,
    Position,
    Range,
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri (getUri),
  )
import Unison.ABT qualified as ABT
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.FileParsers (ShouldUseTndr (..))
import Unison.FileParsers qualified as FileParsers
import Unison.KindInference.Error qualified as KindInference
import Unison.LSP.Conversions
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.Diagnostics (DiagnosticSeverity (..), mkDiagnostic, reportDiagnostics)
import Unison.LSP.FileAnalysis.UnusedBindings qualified as UnusedBindings
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.PrintError qualified as PrintError
import Unison.Referent qualified as Referent
import Unison.Result (Note)
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (toText)
import Unison.Syntax.Lexer.Unison qualified as L
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term qualified as Term
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeError qualified as TypeError
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Summary (FileSummary (..), fileDefLocations)
import Unison.UnisonFile.Summary qualified as FileSummary
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as R1
import Unison.Var qualified as Var
import UnliftIO.STM
import Witherable

-- | Lex, parse, and typecheck a file.
checkFile :: (HasUri d Uri) => d -> Lsp (Maybe FileAnalysis)
checkFile doc = runMaybeT do
  pp <- lift getCurrentProjectPath
  let fileUri = doc ^. uri
  (fileVersion, contents) <- VFS.getFileContents fileUri
  parseNames <- lift getCurrentNames
  let sourceName = getUri $ doc ^. uri
  let lexedSource@(srcText, tokens) = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  cb <- asks codebase
  let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
  uniqueName <- liftIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid pp,
            names = parseNames,
            maybeNamespace = Nothing,
            localNamespacePrefixedTypesAndConstructors = mempty
          }
  (notes, parsedFile, typecheckedFile) <- do
    liftIO do
      Codebase.runTransaction cb do
        parseResult <- Parsers.parseFile (Text.unpack sourceName) (Text.unpack srcText) parsingEnv
        case Result.fromParsing parseResult of
          Result.Result parsingNotes Nothing -> pure (parsingNotes, Nothing, Nothing)
          Result.Result _ (Just parsedFile) -> do
            typecheckingEnv <- computeTypecheckingEnvironment (ShouldUseTndr'Yes parsingEnv) cb ambientAbilities parsedFile
            let Result.Result typecheckingNotes maybeTypecheckedFile = FileParsers.synthesizeFile typecheckingEnv parsedFile
            pure (typecheckingNotes, Just parsedFile, maybeTypecheckedFile)
  filePPED <- lift $ ppedForFileHelper parsedFile typecheckedFile
  (errDiagnostics, codeActions) <- lift $ analyseFile fileUri srcText filePPED notes
  let codeActionRanges =
        codeActions
          & foldMap (\(RangedCodeAction {_codeActionRanges, _codeAction}) -> (,_codeAction) <$> _codeActionRanges)
          & toRangeMap
  let typeSignatureHints = fromMaybe mempty (mkTypeSignatureHints <$> parsedFile <*> typecheckedFile)
  let fileSummary = FileSummary.mkFileSummary parsedFile typecheckedFile
  let unusedBindingDiagnostics = fileSummary ^.. _Just . to termsBySymbol . folded . folding (\(_topLevelAnn, _refId, trm, _type) -> UnusedBindings.analyseTerm fileUri trm)
  let tokenMap = getTokenMap tokens
  conflictWarningDiagnostics <-
    fold <$> for fileSummary \fs ->
      lift $ computeConflictWarningDiagnostics fileUri fs
  let diagnosticRanges =
        (errDiagnostics <> conflictWarningDiagnostics <> unusedBindingDiagnostics)
          & fmap (\d -> (d ^. range, d))
          & toRangeMap
  let fileAnalysis = FileAnalysis {diagnostics = diagnosticRanges, codeActions = codeActionRanges, fileSummary, typeSignatureHints, ..}
  pure fileAnalysis

-- | Get the location of user defined definitions within the file
getFileDefLocations :: Uri -> MaybeT Lsp (Map Symbol (Set Ann))
getFileDefLocations uri = do
  fileDefLocations <$> getFileSummary uri

fileAnalysisWorker :: Lsp ()
fileAnalysisWorker = forever do
  dirtyFilesV <- asks dirtyFilesVar
  checkedFilesV <- asks checkedFilesVar
  -- We may want to debounce this if it typechecks too eagerly,
  -- but typechecking is pretty fast right now when scratch files are small
  dirtyFileIDs <- atomically $ do
    dirty <- readTVar dirtyFilesV
    writeTVar dirtyFilesV mempty
    guard $ not $ null dirty
    pure dirty
  freshlyCheckedFiles <-
    Map.fromList <$> forMaybe (toList dirtyFileIDs) \docUri -> runMaybeT do
      fileInfo <- MaybeT (checkFile $ TextDocumentIdentifier docUri)
      pure (docUri, fileInfo)
  Debug.debugM Debug.LSP "Freshly Typechecked " (Map.toList freshlyCheckedFiles)
  -- Overwrite any files we successfully checked
  atomically $ do
    checkedFiles <- readTVar checkedFilesV
    let zipper = \case
          These mvar new -> tryTakeTMVar mvar *> putTMVar mvar new *> pure mvar
          This mvar -> pure mvar
          That new -> newTMVar new
    newCheckedFiles <- sequenceA $ alignWith zipper checkedFiles freshlyCheckedFiles
    writeTVar checkedFilesV newCheckedFiles
  for freshlyCheckedFiles \(FileAnalysis {fileUri, fileVersion, diagnostics}) -> do
    reportDiagnostics fileUri (Just fileVersion) $ fold diagnostics

analyseFile :: (Foldable f) => Uri -> Text -> PPED.PrettyPrintEnvDecl -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
analyseFile fileUri srcText pped notes = do
  let ppe = PPED.suffixifiedPPE pped
  Env {codebase} <- ask
  (noteDiags, noteActions) <- analyseNotes codebase fileUri ppe (Text.unpack srcText) notes
  pure (noteDiags, noteActions)

-- | Returns diagnostics which show a warning diagnostic when editing a term that's conflicted in the
-- codebase.
computeConflictWarningDiagnostics :: Uri -> FileSummary -> Lsp [Diagnostic]
computeConflictWarningDiagnostics fileUri fileSummary@FileSummary {fileNames} = do
  let defLocations = fileDefLocations fileSummary
  conflictedNames <- Names.conflicts <$> getCurrentNames
  let locationForName :: Name -> Set Ann
      locationForName name = fold $ Map.lookup (Name.toVar name) defLocations
  let conflictedTermLocations =
        let fileTerms = R1.toMultimap (Names.terms fileNames)
            conflictedTerms = R1.toMultimap (Names.terms conflictedNames)
         in Map.intersectionWithKey (\name _ _ -> locationForName name) fileTerms conflictedTerms
  let conflictedTypeLocations =
        let fileTypes = R1.toMultimap (Names.types fileNames)
            conflictedTypes = R1.toMultimap (Names.types conflictedNames)
         in Map.intersectionWithKey (\name _ _ -> locationForName name) fileTypes conflictedTypes
  let toDiagnostics annMap =
        annMap
          & Map.toList
          & foldMap \(name, locs) ->
            (mapMaybe Cv.annToRange . Set.toList $ locs)
              <&> \range ->
                let msg = "There are multiple definitions of `" <> Name.toText name <> "` in your namespace; updating this definition will replace them."
                    newRangeEnd =
                      range ^. LSPTypes.start
                        & LSPTypes.character +~ fromIntegral (Text.length (Name.toText name))
                    newRange = range & LSPTypes.end .~ newRangeEnd
                 in mkDiagnostic
                      fileUri
                      newRange
                      DiagnosticSeverity_Information
                      []
                      msg
                      mempty
  pure $ toDiagnostics conflictedTermLocations <> toDiagnostics conflictedTypeLocations

getTokenMap :: [L.Token L.Lexeme] -> IM.IntervalMap Position L.Lexeme
getTokenMap tokens =
  tokens
    & mapMaybe
      ( \token ->
          IM.singleton <$> (annToInterval $ Parser.ann token) <*> pure (L.payload token)
      )
    & fold

analyseNotes ::
  (Foldable f, MonadIO m) =>
  (Codebase.Codebase IO Symbol Ann) ->
  Uri ->
  PrettyPrintEnv ->
  String ->
  f (Note Symbol Ann) ->
  m ([Diagnostic], [RangedCodeAction])
analyseNotes codebase fileUri ppe src notes = do
  flip foldMapM notes \note -> case note of
    Result.TypeError errNote@(Context.ErrorNote {cause}) -> do
      let typeErr = TypeError.typeErrorFromNote errNote
          ranges = case typeErr of
            TypeError.Mismatch {mismatchSite, foundType, expectedType}
              | -- If it's a delay mismatch, the error is likely with the block definition (e.g. missing 'do') so we highlight the whole block.
                Just _ <- Typechecker.isMismatchMissingDelay foundType expectedType ->
                  singleRange $ ABT.annotation mismatchSite
              -- Otherwise we highlight the leafe nodes of the block
              | otherwise -> leafNodeRanges "mismatch" mismatchSite
            TypeError.BooleanMismatch {mismatchSite} -> leafNodeRanges "mismatch" mismatchSite
            TypeError.ExistentialMismatch {mismatchSite} -> leafNodeRanges "mismatch" mismatchSite
            TypeError.FunctionApplication {f} -> singleRange $ ABT.annotation f
            TypeError.NotFunctionApplication {f} -> singleRange $ ABT.annotation f
            TypeError.AbilityCheckFailure {abilityCheckFailureSite} -> singleRange abilityCheckFailureSite
            TypeError.AbilityEqFailure {abilityCheckFailureSite} -> singleRange abilityCheckFailureSite
            TypeError.AbilityEqFailureFromAp {expectedSite, mismatchSite} -> do
              let locs = [ABT.annotation expectedSite, ABT.annotation mismatchSite]
              (r, rs) <- withNeighbours (locs >>= aToR)
              pure (r, ("mismatch",) <$> rs)
            TypeError.UnguardedLetRecCycle {cycleLocs} -> do
              let ranges :: [Range]
                  ranges = cycleLocs >>= aToR
              (range, cycleRanges) <- withNeighbours ranges
              pure (range, ("cycle",) <$> cycleRanges)
            TypeError.UnknownType {typeSite} -> singleRange typeSite
            TypeError.UnknownTerm {termSite} -> singleRange termSite
            TypeError.DuplicateDefinitions {defns} -> do
              (_v, locs) <- toList defns
              (r, rs) <- withNeighbours (locs >>= aToR)
              pure (r, ("duplicate definition",) <$> rs)
            TypeError.RedundantPattern loc -> singleRange loc
            TypeError.UncoveredPatterns loc _pats -> singleRange loc
            TypeError.KindInferenceFailure ke -> singleRange (KindInference.lspLoc ke)
            -- These type errors don't have custom type error conversions, but some
            -- still have valid diagnostics.
            TypeError.Other e@(Context.ErrorNote {cause}) -> case cause of
              Context.PatternArityMismatch loc _typ _numArgs -> singleRange loc
              Context.HandlerOfUnexpectedType loc _typ -> singleRange loc
              Context.TypeMismatch {} -> shouldHaveBeenHandled e
              Context.IllFormedType {} -> shouldHaveBeenHandled e
              Context.UnknownSymbol loc _ -> singleRange loc
              Context.UnknownTerm loc _ _ _ -> singleRange loc
              Context.AbilityCheckFailure {} -> shouldHaveBeenHandled e
              Context.AbilityEqFailure {} -> shouldHaveBeenHandled e
              Context.EffectConstructorWrongArgCount {} -> shouldHaveBeenHandled e
              Context.MalformedEffectBind {} -> shouldHaveBeenHandled e
              Context.DuplicateDefinitions {} -> shouldHaveBeenHandled e
              Context.UnguardedLetRecCycle {} -> shouldHaveBeenHandled e
              Context.ConcatPatternWithoutConstantLength loc _ -> singleRange loc
              Context.DataEffectMismatch _ _ decl -> singleRange $ DD.annotation decl
              Context.UncoveredPatterns loc _ -> singleRange loc
              Context.RedundantPattern loc -> singleRange loc
              Context.InaccessiblePattern loc -> singleRange loc
              Context.KindInferenceFailure {} -> shouldHaveBeenHandled e
          shouldHaveBeenHandled e = do
            Debug.debugM Debug.LSP "This diagnostic should have been handled by a previous case but was not" e
            empty
          diags = noteDiagnostic note ranges
      -- Sort on match accuracy first, then name.
      codeActions <- case cause of
        Context.UnknownTerm _ v suggestions typ -> do
          typeHoleActions <- typeHoleReplacementCodeActions diags v typ
          pure $
            nameResolutionCodeActions diags suggestions
              <> typeHoleActions
        _ -> pure []
      pure (diags, codeActions)
    Result.NameResolutionFailures {} -> do
      -- TODO: diagnostics/code actions for resolution failures
      pure (noteDiagnostic note todoAnnotation, [])
    Result.Parsing err -> do
      let diags = do
            (errMsg, ranges) <- PrintError.renderParseErrors src err
            let txtMsg = Text.pack $ Pretty.toPlain 80 errMsg
            range <- ranges
            pure $ mkDiagnostic fileUri (uToLspRange range) DiagnosticSeverity_Error [] txtMsg []
      -- TODO: Some parsing errors likely have reasonable code actions
      pure (diags, [])
    Result.UnknownSymbol _ loc ->
      pure (noteDiagnostic note (singleRange loc), [])
    Result.TypeInfo {} ->
      -- No relevant diagnostics from type info.
      pure ([], [])
    Result.CompilerBug cbug -> do
      let ranges = case cbug of
            Result.TopLevelComponentNotFound _ trm -> singleRange $ ABT.annotation trm
            Result.ResolvedNameNotFound _ loc _ -> singleRange loc
            Result.TypecheckerBug tcbug -> case tcbug of
              Context.UnknownDecl _un _ref decls -> decls & foldMap \decl -> singleRange $ DD.annotation decl
              Context.UnknownConstructor _un _gcr decl -> singleRange $ DD.annotation decl
              Context.UndeclaredTermVariable _sym _con -> todoAnnotation
              Context.RetractFailure _el _con -> todoAnnotation
              Context.EmptyLetRec trm -> singleRange $ ABT.annotation trm
              Context.PatternMatchFailure -> todoAnnotation
              Context.EffectConstructorHadMultipleEffects typ -> singleRange $ ABT.annotation typ
              Context.FreeVarsInTypeAnnotation _set -> todoAnnotation
              Context.UnannotatedReference _ref -> todoAnnotation
              Context.MalformedPattern pat -> singleRange $ Pattern.loc pat
              Context.UnknownTermReference _ref -> todoAnnotation
              Context.UnknownExistentialVariable _sym _con -> todoAnnotation
              Context.IllegalContextExtension _con _el _s -> todoAnnotation
              Context.OtherBug _s -> todoAnnotation
      pure (noteDiagnostic note ranges, [])
  where
    leafNodeRanges label mismatchSite = do
      let locs = ABT.annotation <$> expressionLeafNodes mismatchSite
      (r, rs) <- withNeighbours (locs >>= aToR)
      pure (r, (label,) <$> rs)
    -- Diagnostics with this return value haven't been properly configured yet.
    todoAnnotation = []
    singleRange :: Ann -> [(Range, [a])]
    singleRange ann = do
      r <- aToR ann
      pure (r, [])

    aToR :: Ann -> [Range]
    aToR = maybeToList . annToRange
    -- >>> withNeighbours [1, 2, 3, 4]
    -- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
    withNeighbours :: [a] -> [(a, [a])]
    withNeighbours [] = []
    withNeighbours (a : as) = (a, as) : (second (a :) <$> withNeighbours as)
    -- Builds diagnostics for a note, one diagnostic per range.
    noteDiagnostic ::
      Note Symbol Ann ->
      -- All ranges affected by this note, each range may have references to 'related'
      -- ranges.
      -- E.g. a name conflict note might mark each conflicted name, and contain references to the
      -- other conflicted name locations.
      [(Range, [(Text, Range)])] ->
      [Diagnostic]
    noteDiagnostic note ranges =
      let msg = Text.pack $ Pretty.toPlain 80 $ PrintError.printNoteWithSource ppe src note
       in do
            (range, references) <- ranges
            pure $ mkDiagnostic fileUri range DiagnosticSeverity_Error [] msg references
    -- Suggest name replacements or qualifications when there's ambiguity
    nameResolutionCodeActions :: [Diagnostic] -> [Context.Suggestion Symbol Ann] -> [RangedCodeAction]
    nameResolutionCodeActions diags suggestions = do
      Context.Suggestion {suggestionName, suggestionType, suggestionMatch} <- sortOn nameResolutionSuggestionPriority suggestions
      let prettyType = TypePrinter.prettyStr Nothing ppe suggestionType
      let ranges = (diags ^.. folded . range)
      let rca = rangedCodeAction ("Use " <> Name.toText suggestionName <> " : " <> Text.pack prettyType) diags ranges
      pure $
        rca
          & includeEdits fileUri (Name.toText suggestionName) ranges
          & codeAction . isPreferred ?~ (suggestionMatch == Context.Exact)

    nameResolutionSuggestionPriority (Context.Suggestion {suggestionMatch, suggestionName}) = case suggestionMatch of
      Context.Exact -> (0 :: Int, suggestionName)
      Context.WrongType -> (1, suggestionName)
      Context.WrongName -> (2, suggestionName)

    -- typeHoleReplacementCodeActions :: Symbol -> _ -> Lsp [a]
    typeHoleReplacementCodeActions diags v typ
      | not (isUserBlank v) = pure []
      | otherwise = do
          let cleanedTyp = Context.generalizeAndUnTypeVar typ -- TODO: is this right?
          refs <- liftIO . Codebase.runTransaction codebase $ Codebase.termsOfType codebase cleanedTyp
          forMaybe (toList refs) $ \ref -> runMaybeT $ do
            hqNameSuggestion <- MaybeT . pure $ PPE.terms ppe ref
            typ <- MaybeT . liftIO . Codebase.runTransaction codebase $ Codebase.getTypeOfReferent codebase ref
            let prettyType = TypePrinter.prettyStr Nothing ppe typ
            let txtName = HQ'.toText hqNameSuggestion
            let ranges = (diags ^.. folded . range)
            let rca = rangedCodeAction ("Use " <> txtName <> " : " <> Text.pack prettyType) diags ranges
            pure $ includeEdits fileUri txtName ranges rca
    isUserBlank :: Symbol -> Bool
    isUserBlank v = case Var.typeOf v of
      Var.User name -> Text.isPrefixOf "_" name
      _ -> False

toRangeMap :: (Foldable f) => f (Range, a) -> IntervalMap Position [a]
toRangeMap vs =
  IM.fromListWith (<>) (toList vs <&> \(r, a) -> (rangeToInterval r, [a]))

getFileAnalysis :: Uri -> MaybeT Lsp FileAnalysis
getFileAnalysis uri = do
  checkedFilesV <- asks checkedFilesVar
  -- Try to get the file analysis, if there's a var, then read it, waiting if necessary
  -- If there's no var, add one and wait for it to be filled (all Uris should be analyzed
  -- eventually unless theres some bug in the VFS).
  tmvar <- atomically do
    checkedFiles <- readTVar checkedFilesV
    case Map.lookup uri checkedFiles of
      Nothing -> do
        mvar <- newEmptyTMVar
        Debug.debugM Debug.LSP "File analysis requested but none available, waiting for analysis for" uri
        writeTVar checkedFilesV $ Map.insert uri mvar checkedFiles
        pure mvar
      Just mvar -> pure mvar
  Debug.debugM Debug.LSP "Waiting on file analysis" uri
  r <- atomically (readTMVar tmvar)
  Debug.debugM Debug.LSP "Got file analysis" uri
  pure r

-- | Build a Names from a file if it's parseable.
--
-- If the file typechecks, generate names from that,
-- otherwise, generate names from the 'parsed' file. Note that the
-- names for a parsed file contains only names for parts of decls, since
-- we don't know references within terms before typechecking due to TDNR.
-- This should be fine though, since those references will all be kept in the
-- ABT as symbols anyways.
--
-- See UF.toNames and UF.typecheckedToNames for more info.
getFileNames :: Uri -> MaybeT Lsp Names
getFileNames fileUri = do
  FileAnalysis {typecheckedFile = tf, parsedFile = pf} <- getFileAnalysis fileUri
  hoistMaybe (fmap UF.typecheckedToNames tf <|> fmap UF.toNames pf)

getFileSummary :: Uri -> MaybeT Lsp FileSummary
getFileSummary uri = do
  FileAnalysis {fileSummary} <- getFileAnalysis uri
  MaybeT . pure $ fileSummary

-- TODO memoize per file
ppedForFile :: Uri -> Lsp PPED.PrettyPrintEnvDecl
ppedForFile fileUri = do
  runMaybeT (getFileAnalysis fileUri) >>= \case
    Just (FileAnalysis {typecheckedFile = tf, parsedFile = uf}) ->
      ppedForFileHelper uf tf
    _ -> ppedForFileHelper Nothing Nothing

ppedForFileHelper :: Maybe (UF.UnisonFile Symbol a) -> Maybe (UF.TypecheckedUnisonFile Symbol a) -> Lsp PPED.PrettyPrintEnvDecl
ppedForFileHelper uf tf = do
  codebasePPED <- currentPPED
  hashLen <- asks codebase >>= \codebase -> liftIO (Codebase.runTransaction codebase Codebase.hashLength)
  pure $ case (uf, tf) of
    (Nothing, Nothing) -> codebasePPED
    (_, Just tf) ->
      let fileNames = UF.typecheckedToNames tf
          filePPED = PPED.makePPED (PPE.hqNamer hashLen fileNames) (PPE.suffixifyByHash fileNames)
       in filePPED `PPED.addFallback` codebasePPED
    (Just uf, _) ->
      let fileNames = UF.toNames uf
          filePPED = PPED.makePPED (PPE.hqNamer hashLen fileNames) (PPE.suffixifyByHash fileNames)
       in filePPED `PPED.addFallback` codebasePPED

mkTypeSignatureHints :: UF.UnisonFile Symbol Ann -> UF.TypecheckedUnisonFile Symbol Ann -> Map Symbol TypeSignatureHint
mkTypeSignatureHints parsedFile typecheckedFile = do
  let symbolsWithoutTypeSigs :: Map Symbol Ann
      symbolsWithoutTypeSigs =
        Map.toList (UF.terms parsedFile)
          & mapMaybe
            ( \(v, (ann, trm)) -> do
                -- We only want hints for terms without a user signature
                guard (isNothing $ Term.getTypeAnnotation trm)
                pure (v, ann)
            )
          & Map.fromList
      typeHints =
        typecheckedFile
          & UF.hashTermsId
          & Zip.zip symbolsWithoutTypeSigs
          & imapMaybe
            ( \v (ann, (_ann, ref, _wk, _trm, typ)) -> do
                name <- Name.parseText (Var.name v)
                range <- annToRange ann
                let newRangeEnd =
                      range ^. LSPTypes.start
                        & LSPTypes.character +~ fromIntegral (Text.length (Name.toText name))
                let newRange = range & LSPTypes.end .~ newRangeEnd
                pure $ TypeSignatureHint name (Referent.fromTermReferenceId ref) newRange typ
            )
   in typeHints

-- | Crawl a term and find the nodes which actually influence its return type. This is useful for narrowing down a giant
-- "This let/do block has the wrong type" into "This specific line returns the wrong type"
-- This is just a heuristic.
expressionLeafNodes :: Term.Term2 vt at ap v a -> [Term.Term2 vt at ap v a]
expressionLeafNodes abt =
  case ABT.out abt of
    ABT.Var {} -> [abt]
    ABT.Cycle r -> expressionLeafNodes r
    ABT.Abs _ r -> expressionLeafNodes r
    ABT.Tm f -> case f of
      Term.Int {} -> [abt]
      Term.Nat {} -> [abt]
      Term.Float {} -> [abt]
      Term.Boolean {} -> [abt]
      Term.Text {} -> [abt]
      Term.Char {} -> [abt]
      Term.Blank {} -> [abt]
      Term.Ref {} -> [abt]
      Term.Constructor {} -> [abt]
      Term.Request {} -> [abt]
      -- Not 100% sure whether the error should appear on the handler or action, maybe both?
      Term.Handle handler _action -> expressionLeafNodes handler
      Term.App _a _b -> [abt]
      Term.Ann a _ -> expressionLeafNodes a
      Term.List {} -> [abt]
      Term.If _cond a b -> expressionLeafNodes a <> expressionLeafNodes b
      Term.And {} -> [abt]
      Term.Or {} -> [abt]
      Term.Lam a -> expressionLeafNodes a
      Term.LetRec _isTop _bindings body -> expressionLeafNodes body
      Term.Let _isTop _bindings body -> expressionLeafNodes body
      Term.Match _a cases -> cases & foldMap \(Term.MatchCase {matchBody}) -> expressionLeafNodes matchBody
      Term.TermLink {} -> [abt]
      Term.TypeLink {} -> [abt]
