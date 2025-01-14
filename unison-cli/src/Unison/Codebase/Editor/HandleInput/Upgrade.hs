-- | @upgrade@ input handler.
module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens qualified as Lens
import Control.Monad.Reader (ask)
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.List.NonEmpty.Extra ((|>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Text.Builder qualified
import Text.Pretty.Simple (pShow)
import U.Codebase.Sqlite.DbId (ProjectId)
import Unison.Builtin.Decls qualified as Decls
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, parseAndTypecheck)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..))
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2 (typecheckedUnisonFileToBranchUpdates)
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Name.Forward (ForwardName (..))
import Unison.Name.Forward qualified as ForwardName
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED (addFallback)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED (makePPED)
import Unison.Project (ProjectBranchName)
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment (toEscapedText)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as WK
import Witch (unsafeFrom)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldName newName = do
  when (oldName == newName) do
    Cli.returnEarlyWithoutOutput

  env <- ask

  let oldPath = Path.Absolute (Path.fromList [NameSegment.libSegment, oldName])
  let newPath = Path.Absolute (Path.fromList [NameSegment.libSegment, newName])

  currentNamespace <- Cli.getCurrentProjectRoot
  let currentNamespaceSansOld = currentNamespace & Branch.step (Branch.deleteLibdep oldName)
  let currentNamespaceSansOld0 = Branch.head currentNamespaceSansOld
  let currentDeepTermsSansOld = Branch.deepTerms currentNamespaceSansOld0
  let currentDeepTypesSansOld = Branch.deepTypes currentNamespaceSansOld0
  let currentLocalNames = Branch.toNames (Branch.deleteLibdeps $ Branch.head currentNamespace)
  let currentLocalConstructorNames = forwardCtorNames currentLocalNames
  let currentDeepNamesSansOld = Branch.toNames currentNamespaceSansOld0

  oldNamespace <- Cli.expectBranch0AtPath' (Path.AbsolutePath' oldPath)
  let oldLocalNamespace = Branch.deleteLibdeps oldNamespace
  let oldLocalTerms = Branch.deepTerms oldLocalNamespace
  let oldLocalTypes = Branch.deepTypes oldLocalNamespace
  let oldNamespaceMinusLocal = maybe Branch.empty0 Branch.head (Map.lookup NameSegment.libSegment (oldNamespace ^. Branch.children))
  let oldDeepMinusLocalTerms = Branch.deepTerms oldNamespaceMinusLocal
  let oldDeepMinusLocalTypes = Branch.deepTypes oldNamespaceMinusLocal

  newNamespace <- Cli.expectBranch0AtPath' (Path.AbsolutePath' newPath)
  let newLocalNamespace = Branch.deleteLibdeps newNamespace
  let newLocalTerms = Branch.deepTerms newLocalNamespace
  let newLocalTypes = Branch.deepTypes newLocalNamespace

  -- High-level idea: we are trying to perform substitution in every term that depends on something in `old` with the
  -- corresponding thing in `new`, by first rendering the user's code with a particular pretty-print environment, then
  -- parsing it back in a particular parsing environment.
  --
  -- For example, if a user with the namespace
  --
  --     lib.old.foo#oldfoo = 17
  --     lib.new.foo#newfoo = 18
  --     mything#mything    = #oldfoo + 10
  --
  -- runs `upgrade old new`, we will first render
  --
  --     mything#mything    = #oldfoo + 10
  --
  -- as
  --
  --     mything = foo + 10
  --
  -- (note, "foo" here is the shortest unambiguous suffix of all names minus those in `old`), then parse it back in the
  -- parsing environment with names
  --
  --     lib.new.foo = #newfoo
  --
  -- resulting in
  --
  --     mything#mything2 = #newfoo + 10

  (unisonFile, printPPE) <-
    Cli.runTransactionWithRollback \abort -> do
      dependents <-
        getNamespaceDependentsOf
          currentLocalNames
          ( Set.unions
              [ keepOldLocalTermsNotInNew oldLocalTerms newLocalTerms,
                keepOldLocalTypesNotInNew oldLocalTypes newLocalTypes,
                keepOldDeepTermsStillInUse oldDeepMinusLocalTerms currentDeepTermsSansOld,
                keepOldDeepTypesStillInUse oldDeepMinusLocalTypes currentDeepTypesSansOld
              ]
          )
      unisonFile <- do
        addDefinitionsToUnisonFile
          abort
          env.codebase
          (findCtorNames Output.UOUUpgrade currentLocalNames currentLocalConstructorNames)
          dependents
          UnisonFile.emptyUnisonFile
      pure
        ( unisonFile,
          let ppe1 =
                makeOldDepPPE
                  oldName
                  newName
                  currentDeepNamesSansOld
                  (Branch.toNames oldNamespace)
                  (Branch.toNames oldLocalNamespace)
                  (Branch.toNames newLocalNamespace)
              ppe2 =
                PPED.makePPED
                  (PPE.namer (Names.fromReferenceIds dependents))
                  (PPE.suffixifyByName currentDeepNamesSansOld)
              ppe3 =
                PPED.makePPED
                  (PPE.hqNamer 10 currentDeepNamesSansOld)
                  (PPE.suffixifyByHash currentDeepNamesSansOld)
           in ppe1 `PPED.addFallback` ppe2 `PPED.addFallback` ppe3
        )

  pp@(PP.ProjectPath project projectBranch _path) <- Cli.getCurrentProjectPath
  parsingEnv <- Cli.makeParsingEnv pp currentDeepNamesSansOld
  typecheckedUnisonFile <- do
    let prettyUnisonFile = Pretty.prettyUnisonFile printPPE unisonFile
    parseAndTypecheck prettyUnisonFile parsingEnv & onNothingM do
      let getTemporaryBranchName = findTemporaryBranchName (project ^. #projectId) oldName newName
      (_temporaryBranchId, temporaryBranchName) <-
        HandleInput.Branch.createBranch
          textualDescriptionOfUpgrade
          (CreateFrom'NamespaceWithParent projectBranch currentNamespaceSansOld)
          project
          getTemporaryBranchName
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
      Cli.returnEarly $
        Output.UpgradeFailure (projectBranch ^. #name) temporaryBranchName scratchFilePath oldName newName

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase env.codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (findCtorNamesMaybe Output.UOUUpgrade currentLocalNames currentLocalConstructorNames Nothing)
        typecheckedUnisonFile
  Cli.stepAt
    textualDescriptionOfUpgrade
    ( PP.toRoot pp,
      Branch.deleteLibdep oldName . Branch.batchUpdates branchUpdates
    )
  Cli.respond (Output.UpgradeSuccess oldName newName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toEscapedText oldName, NameSegment.toEscapedText newName]

keepOldLocalTermsNotInNew :: Relation Referent Name -> Relation Referent Name -> Set TermReference
keepOldLocalTermsNotInNew oldLocalTerms newLocalTerms =
  f oldLocalTerms `Set.difference` f newLocalTerms
  where
    f :: Relation Referent Name -> Set TermReference
    f =
      Set.mapMaybe Referent.toTermReference . Relation.dom

keepOldLocalTypesNotInNew :: Relation TypeReference Name -> Relation TypeReference Name -> Set TypeReference
keepOldLocalTypesNotInNew oldLocalTypes newLocalTypes =
  Relation.dom oldLocalTypes `Set.difference` Relation.dom newLocalTypes

keepOldDeepTermsStillInUse :: Relation Referent Name -> Relation Referent Name -> Set TermReference
keepOldDeepTermsStillInUse oldDeepMinusLocalTerms currentDeepTermsSansOld =
  Relation.dom oldDeepMinusLocalTerms & Set.mapMaybe \referent -> do
    ref <- Referent.toTermReference referent
    guard (not (Relation.memberDom referent currentDeepTermsSansOld))
    pure ref

keepOldDeepTypesStillInUse :: Relation TypeReference Name -> Relation TypeReference Name -> Set TypeReference
keepOldDeepTypesStillInUse oldDeepMinusLocalTypes currentDeepTypesSansOld =
  Relation.dom oldDeepMinusLocalTypes
    & Set.filter \typ -> not (Relation.memberDom typ currentDeepTypesSansOld)

-- | @addDefinitionsToUnisonFile abort codebase doFindCtorNames definitions file@ adds all @definitions@ to @file@,
-- avoiding overwriting anything already in @file@. Every definition is put into the file with every naming it has in
-- @names@ "on the left-hand-side of the equals" (but yes type decls don't really have a LHS).
--
-- TODO: find a better module for this function, as it's used in a couple places
addDefinitionsToUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  (Maybe Int -> Name -> Either Output.Output [Name]) ->
  DefnsF (Relation Name) TermReferenceId TypeReferenceId ->
  UnisonFile Symbol Ann ->
  Transaction (UnisonFile Symbol Ann)
addDefinitionsToUnisonFile abort codebase doFindCtorNames newDefns oldUF = do
  newUF <- makeUnisonFile abort codebase doFindCtorNames newDefns
  pure (oldUF `UnisonFile.leftBiasedMerge` newUF)

makeUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  (Maybe Int -> Name -> Either Output.Output [Name]) ->
  DefnsF (Relation Name) TermReferenceId TypeReferenceId ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile abort codebase doFindCtorNames defns = do
  file <- foldM addTermComponent UnisonFile.emptyUnisonFile (Set.map Reference.idToHash (Relation.ran defns.terms))
  foldM addDeclComponent file (Set.map Reference.idToHash (Relation.ran defns.types))
  where
    addTermComponent :: UnisonFile Symbol Ann -> Hash -> Transaction (UnisonFile Symbol Ann)
    addTermComponent uf h = do
      termComponent <- Codebase.unsafeGetTermComponent codebase h
      pure $ foldl' addTermElement uf (zip termComponent [0 ..])
      where
        addTermElement :: UnisonFile Symbol Ann -> ((Term Symbol Ann, Type Symbol Ann), Reference.Pos) -> UnisonFile Symbol Ann
        addTermElement uf ((tm, tp), i) = do
          let termNames = Relation.lookupRan (Reference.Id h i) defns.terms
          foldl' (addDefinition tm tp) uf termNames
        addDefinition :: Term Symbol Ann -> Type Symbol Ann -> UnisonFile Symbol Ann -> Name -> UnisonFile Symbol Ann
        addDefinition tm tp uf (Name.toVar -> v) =
          let prependTerm to = (v, Ann.External, tm) : to
           in if isTest tp
                then uf & #watches . Lens.at WK.TestWatch . Lens.non [] Lens.%~ prependTerm
                else uf & #terms Lens.%~ Map.insert v (Ann.External, tm)

    isTest = Typechecker.isEqual (Decls.testResultListType mempty)

    -- given a dependent hash, include that component in the scratch file
    -- todo: wundefined: cut off constructor name prefixes
    addDeclComponent :: UnisonFile Symbol Ann -> Hash -> Transaction (UnisonFile Symbol Ann)
    addDeclComponent uf h = do
      declComponent <- fromJust <$> Codebase.getDeclComponent h
      foldM addDeclElement uf (zip declComponent [0 ..])
      where
        -- for each name a decl has, update its constructor names according to what exists in the namespace
        addDeclElement :: UnisonFile Symbol Ann -> (Decl Symbol Ann, Reference.Pos) -> Transaction (UnisonFile Symbol Ann)
        addDeclElement uf (decl, i) = do
          let declNames = Relation.lookupRan (Reference.Id h i) defns.types
          -- look up names for this decl's constructor based on the decl's name, and embed them in the decl definition.
          foldM (addRebuiltDefinition decl) uf declNames
          where
            -- skip any definitions that already have names, we don't want to overwrite what the user has supplied
            addRebuiltDefinition :: Decl Symbol Ann -> UnisonFile Symbol Ann -> Name -> Transaction (UnisonFile Symbol Ann)
            addRebuiltDefinition decl uf name = case decl of
              Left ed ->
                overwriteConstructorNames name ed.toDataDecl <&> \ed' ->
                  uf
                    & #effectDeclarationsId
                      %~ Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, Decl.EffectDeclaration ed')
              Right dd ->
                overwriteConstructorNames name dd <&> \dd' ->
                  uf
                    & #dataDeclarationsId
                      %~ Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, dd')

        -- Constructor names are bogus when pulled from the database, so we set them to what they should be here
        overwriteConstructorNames :: Name -> DataDeclaration Symbol Ann -> Transaction (DataDeclaration Symbol Ann)
        overwriteConstructorNames name dd =
          let constructorNames :: Transaction [Symbol]
              constructorNames =
                case doFindCtorNames (Just $ Decl.constructorCount dd) name of
                  Left err -> abort err
                  Right array | all (isJust . Name.stripNamePrefix name) array -> pure (map Name.toVar array)
                  Right array -> do
                    traceM "I ran into a situation where a type's constructors didn't match its name,"
                    traceM "in a spot where I didn't expect to be discovering that.\n\n"
                    traceM "Type Name:"
                    traceM . Text.Lazy.unpack $ pShow name
                    traceM "Constructor Names:"
                    traceM . Text.Lazy.unpack $ pShow array
                    error "Sorry for crashing."

              swapConstructorNames oldCtors =
                let (annotations, _vars, types) = unzip3 oldCtors
                 in zip3 annotations <$> constructorNames <*> pure types
           in Lens.traverseOf Decl.constructors_ swapConstructorNames dd

makeOldDepPPE ::
  NameSegment ->
  NameSegment ->
  Names ->
  Names ->
  Names ->
  Names ->
  PrettyPrintEnvDecl
makeOldDepPPE oldName newName currentDeepNamesSansOld oldDeepNames oldLocalNames newLocalNames =
  let makePPE suffixifier =
        PPE.PrettyPrintEnv termToNames typeToNames
        where
          termToNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          termToNames ref
            | inNewNamespace = []
            | hasNewLocalTermsForOldLocalNames = PPE.makeTermNames fakeLocalNames suffixifier ref
            | onlyInOldNamespace = PPE.makeTermNames fullOldDeepNames PPE.dontSuffixify ref
            | otherwise = []
            where
              inNewNamespace = Relation.memberRan ref (Names.terms newLocalNames)
              hasNewLocalTermsForOldLocalNames =
                not (Map.null (Relation.domain (Names.terms newLocalNames) `Map.restrictKeys` theOldLocalNames))
              theOldLocalNames = Relation.lookupRan ref (Names.terms oldLocalNames)
              onlyInOldNamespace = inOldNamespace && not inCurrentNamespaceSansOld
              inOldNamespace = Relation.memberRan ref (Names.terms oldDeepNames)
              inCurrentNamespaceSansOld = Relation.memberRan ref (Names.terms currentDeepNamesSansOld)
          typeToNames :: TypeReference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          typeToNames ref
            | inNewNamespace = []
            | hasNewLocalTypesForOldLocalNames = PPE.makeTypeNames fakeLocalNames suffixifier ref
            | onlyInOldNamespace = PPE.makeTypeNames fullOldDeepNames PPE.dontSuffixify ref
            | otherwise = []
            where
              inNewNamespace = Relation.memberRan ref (Names.types newLocalNames)
              hasNewLocalTypesForOldLocalNames =
                not (Map.null (Relation.domain (Names.types newLocalNames) `Map.restrictKeys` theOldLocalNames))
              theOldLocalNames = Relation.lookupRan ref (Names.types oldLocalNames)
              onlyInOldNamespace = inOldNamespace && not inCurrentNamespaceSansOld
              inOldNamespace = Relation.memberRan ref (Names.types oldDeepNames)
              inCurrentNamespaceSansOld = Relation.memberRan ref (Names.types currentDeepNamesSansOld)
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE PPE.dontSuffixify,
          suffixifiedPPE = makePPE (PPE.suffixifyByHash currentDeepNamesSansOld)
        }
  where
    -- "full" means "with lib.old.* prefix"
    fullOldDeepNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (oldName :| [NameSegment.libSegment])) oldDeepNames)
    fakeLocalNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (newName :| [NameSegment.libSegment])) oldLocalNames)

-- @findTemporaryBranchName projectId oldDepName newDepName@ finds some unused branch name in @projectId@ with a name
-- like "upgrade-<oldDepName>-to-<newDepName>".
findTemporaryBranchName :: ProjectId -> NameSegment -> NameSegment -> Transaction ProjectBranchName
findTemporaryBranchName projectId oldDepName newDepName = do
  Cli.findTemporaryBranchName projectId $
    -- First try something like
    --
    --   upgrade-unison_base_3_0_0-to-unison_base_4_0_0
    --
    -- and if that fails (which it shouldn't, but may because of symbols or something), back off to some
    -- more-guaranteed-to-work mangled name like
    --
    --   upgrade-unisonbase300-to-unisonbase400
    tryFrom @Text (mk oldDepText newDepText)
      & fromRight (unsafeFrom @Text (mk (scrub oldDepText) (scrub newDepText)))
  where
    mk :: Text -> Text -> Text
    mk old new =
      Text.Builder.run ("upgrade-" <> Text.Builder.text old <> "-to-" <> Text.Builder.text new)

    scrub :: Text -> Text
    scrub =
      Text.filter Char.isAlphaNum

    oldDepText = NameSegment.toEscapedText oldDepName
    newDepText = NameSegment.toEscapedText newDepName

-- | O(r + c * d) touches all the referents (r), and all the NameSegments (d) of all of the Con referents (c)
forwardCtorNames :: Names -> Map ForwardName (Referent, Name)
forwardCtorNames names =
  Map.fromList $
    [ (ForwardName.fromName name, (r, name))
      | (r@Referent.Con {}, rNames) <- Map.toList $ Relation.range names.terms,
        name <- Foldable.toList rNames
    ]

-- | given a decl name, find names for all of its constructors, in order.
--
-- Precondition: 'n' is an element of 'names'
findCtorNames :: Output.UpdateOrUpgrade -> Names -> Map ForwardName (Referent, Name) -> Maybe Int -> Name -> Either Output.Output [Name]
findCtorNames operation names forwardCtorNames ctorCount n =
  let declRef = case Set.lookupMin (Relation.lookupDom n names.types) of
        Nothing -> error "[findCtorNames] precondition violation: n is not an element of names"
        Just x -> x
      f = ForwardName.fromName n
      (_, centerRight) = Map.split f forwardCtorNames
      (center, _) = Map.split (incrementLastSegmentChar f) centerRight

      insertShortest :: Map ConstructorId Name -> (Referent, Name) -> Map ConstructorId Name
      insertShortest m (Referent.Con (ConstructorReference r cid) _ct, newName) | r == declRef =
        case Map.lookup cid m of
          Just existingName
            | length (Name.segments existingName) > length (Name.segments newName) ->
                Map.insert cid newName m
          Just {} -> m
          Nothing -> Map.insert cid newName m
      insertShortest m _ = m
      m = foldl' insertShortest mempty (Foldable.toList center)
      ctorCountGuess = fromMaybe (Map.size m) ctorCount
   in if Map.size m == ctorCountGuess && all (isJust . flip Map.lookup m . fromIntegral) [0 .. ctorCountGuess - 1]
        then Right $ Map.elems m
        else Left $ Output.UpdateIncompleteConstructorSet operation n m ctorCount

findCtorNamesMaybe ::
  Output.UpdateOrUpgrade ->
  Names ->
  Map ForwardName (Referent, Name) ->
  Maybe Int ->
  Name ->
  Either Output.Output (Maybe [Name])
findCtorNamesMaybe operation names forwardCtorNames ctorCount name =
  case Relation.memberDom name (Names.types names) of
    True -> Just <$> findCtorNames operation names forwardCtorNames ctorCount name
    False -> Right Nothing

-- Used by `findCtorNames` to filter `forwardCtorNames` to a narrow range which will be searched linearly.
-- >>> incrementLastSegmentChar $ ForwardName.fromName $ Name.unsafeFromText "foo.bar.quux"
-- ForwardName {toList = "foo" :| ["bar","quuy"]}
incrementLastSegmentChar :: ForwardName -> ForwardName
incrementLastSegmentChar (ForwardName segments) =
  let (initSegments, lastSegment) = (List.NonEmpty.init segments, List.NonEmpty.last segments)
      incrementedLastSegment = incrementLastCharInSegment lastSegment
   in ForwardName $
        maybe
          (List.NonEmpty.singleton incrementedLastSegment)
          (|> incrementedLastSegment)
          (List.NonEmpty.nonEmpty initSegments)
  where
    incrementLastCharInSegment :: NameSegment -> NameSegment
    incrementLastCharInSegment (NameSegment text) =
      let incrementedText =
            if Text.null text
              then text
              else Text.init text `Text.append` Text.singleton (succ $ Text.last text)
       in NameSegment incrementedText
