-- | @update@ input handler.
module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    typecheckedUnisonFileToBranchUpdates,
  )
where

import Control.Lens (mapped, (.=))
import Control.Monad.Reader.Class (ask)
import Data.Bifoldable (bifoldMap)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Reference (Reference, Reference' (..), TermReferenceId)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf2, hydrateDefns, narrowDefns, parseAndTypecheck)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference (fromId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (flattenNametrees)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

handleUpdate2 :: Cli ()
handleUpdate2 = do
  env <- ask
  tuf <- Cli.expectLatestTypecheckedFile
  let termAndDeclNames = getTermAndDeclNames tuf
  pp <- Cli.getCurrentProjectPath
  currentBranch0 <- Cli.getCurrentBranch0
  let currentBranch0ExcludingLibdeps = Branch.deleteLibdeps currentBranch0
  let namesIncludingLibdeps = Branch.toNames currentBranch0

  -- Assert that the namespace doesn't have any conflicted names
  nametree <-
    narrowDefns (Branch.deepDefns currentBranch0ExcludingLibdeps)
      & onLeft (Cli.returnEarly . Output.ConflictedDefn "update")

  let defns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
      defns =
        flattenNametrees nametree

  -- Get the number of constructors for every type declaration
  numConstructors <-
    Cli.runTransaction do
      defns.types
        & BiMultimap.dom
        & Set.toList
        & Foldable.foldlM
          ( \acc -> \case
              ReferenceBuiltin _ -> pure acc
              ReferenceDerived ref -> do
                num <- Operations.expectDeclNumConstructors ref
                pure $! Map.insert ref num acc
          )
          Map.empty

  -- Assert that the namespace doesn't have any incoherent decls
  declNameLookup <-
    Merge.checkDeclCoherency nametree numConstructors
      & onLeft (Cli.returnEarly . Output.IncoherentDeclDuringUpdate)

  Cli.respond Output.UpdateLookingForDependents

  (dependents, hydratedDependents) <-
    Cli.runTransaction do
      -- Get all dependents of things being updated
      dependents0 <-
        getNamespaceDependentsOf2
          (flattenNametrees nametree)
          (getExistingReferencesNamed termAndDeclNames (Branch.toNames currentBranch0ExcludingLibdeps))

      -- Throw away the dependents that are shadowed by the file itself
      let dependents1 :: DefnsF (Map Name) TermReferenceId TypeReferenceId
          dependents1 =
            bimap
              (`Map.withoutKeys` (Set.map Name.unsafeParseVar (UF.termNamespaceBindings tuf)))
              (`Map.withoutKeys` (Set.map Name.unsafeParseVar (UF.typeNamespaceBindings tuf)))
              dependents0

      -- Hydrate the dependents for rendering
      hydratedDependents <-
        hydrateDefns
          (Codebase.unsafeGetTermComponent env.codebase)
          Operations.expectDeclComponent
          dependents1

      pure (dependents1, hydratedDependents)

  secondTuf <- do
    case defnsAreEmpty dependents of
      -- If there are no dependents of the updates, then just use the already-typechecked file.
      True -> pure tuf
      False -> do
        Cli.respond Output.UpdateStartTypechecking

        let prettyUnisonFile =
              let ppe = makePPE 10 namesIncludingLibdeps (UF.typecheckedToNames tuf) dependents
               in makePrettyUnisonFile
                    (Pretty.prettyUnisonFile ppe (UF.discardTypes tuf))
                    (renderDefnsForUnisonFile declNameLookup ppe (over (#terms . mapped) snd hydratedDependents))

        parsingEnv <- Cli.makeParsingEnv pp namesIncludingLibdeps

        secondTuf <-
          parseAndTypecheck prettyUnisonFile parsingEnv & onNothingM do
            scratchFilePath <- fst <$> Cli.expectLatestFile
            liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
            Cli.returnEarly Output.UpdateTypecheckingFailure

        Cli.respond Output.UpdateTypecheckingSuccess

        pure secondTuf

  path <- Cli.getCurrentProjectPath
  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase env.codebase secondTuf
      typecheckedUnisonFileToBranchUpdates
        abort
        (\typeName -> Right (Map.lookup typeName declNameLookup.declToConstructors))
        secondTuf
  Cli.stepAt "update" (path, Branch.batchUpdates branchUpdates)
  #latestTypecheckedFile .= Nothing

  Cli.respond Output.Success

makePrettyUnisonFile :: Pretty ColorText -> DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) -> Pretty ColorText
makePrettyUnisonFile originalFile dependents =
  originalFile
    <> Pretty.newline
    <> Pretty.newline
    <> "-- The definitions below no longer typecheck with the changes above."
    <> Pretty.newline
    <> "-- Please fix the errors and try `update` again."
    <> Pretty.newline
    <> Pretty.newline
    <> ( dependents
           & inAlphabeticalOrder
           & let f = foldMap (\defn -> defn <> Pretty.newline <> Pretty.newline) in bifoldMap f f
       )
  where
    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . List.sortOn (Name.toText . fst) . Map.toList

-- @typecheckedUnisonFileToBranchUpdates getConstructors file@ returns a list of branch updates (suitable for passing
-- along to `batchUpdates` or some "step at" combinator) that corresponds to using all of the contents of @file@.
-- `getConstructors` returns the full constructor names of a decl, e.g. "Maybe" -> ["Maybe.Nothing", "Maybe.Just"]
--
-- For example, if the file contains
--
--     foo.bar.baz = <#foo>
--
-- then the returned updates will look like
--
--     [ ("foo.bar", insert-term("baz",<#foo>)) ]
typecheckedUnisonFileToBranchUpdates ::
  (forall void. Output -> Transaction void) ->
  -- | Returns 'Nothing' if the decl isn't in namesExcludingLibdeps,
  -- in which case we know the decl is new and do not need to generate
  -- delete actions for it.
  (Name -> Either Output (Maybe [Name])) ->
  TypecheckedUnisonFile Symbol Ann ->
  Transaction [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchUpdates abort getConstructors tuf = do
  declUpdates <- makeDeclUpdates abort
  pure $ declUpdates ++ termUpdates
  where
    makeDeclUpdates :: forall m. (forall void. Output -> Transaction void) -> Transaction [(Path, Branch0 m -> Branch0 m)]
    makeDeclUpdates abort = do
      dataDeclUpdates <- Monoid.foldMapM makeDataDeclUpdates (Map.toList $ UF.dataDeclarationsId' tuf)
      effectDeclUpdates <- Monoid.foldMapM makeEffectDeclUpdates (Map.toList $ UF.effectDeclarationsId' tuf)
      pure $ dataDeclUpdates <> effectDeclUpdates
      where
        makeDataDeclUpdates (symbol, (typeRefId, dataDecl)) = makeDeclUpdates (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclUpdates (symbol, (typeRefId, Left effectDecl))

        makeDeclUpdates :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> Transaction [(Path, Branch0 m -> Branch0 m)]
        makeDeclUpdates (symbol, (typeRefId, decl)) = do
          -- some decls will be deleted, we want to delete their
          -- constructors as well
          deleteConstructorActions <-
            ( maybe [] (map (BranchUtil.makeAnnihilateTermName . Path.splitFromName))
                <$> getConstructors (Name.unsafeParseVar symbol)
              )
              & onLeft abort
          let deleteTypeAction = BranchUtil.makeAnnihilateTypeName split
              split = splitVar symbol
              insertTypeAction = BranchUtil.makeAddTypeName split (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                let referentIdsWithNames = zip (Decl.constructorVars (Decl.asDataDecl decl)) (Decl.declConstructorReferents typeRefId decl)
                 in map
                      ( \(sym, rid) ->
                          let splitConName = splitVar sym
                           in BranchUtil.makeAddTermName splitConName (Reference.fromId <$> rid)
                      )
                      referentIdsWithNames
              deleteStuff = deleteTypeAction : deleteConstructorActions
              addStuff = insertTypeAction : insertTypeConstructorActions
          pure $ deleteStuff ++ addStuff

    termUpdates :: [(Path, Branch0 m -> Branch0 m)]
    termUpdates =
      tuf
        & UF.hashTermsId
        & Map.toList
        & foldMap \(var, (_, ref, wk, _, _)) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then
              let split = splitVar var
               in [ BranchUtil.makeAnnihilateTermName split,
                    BranchUtil.makeAddTermName split (Referent.fromTermReferenceId ref)
                  ]
            else []

    splitVar :: Symbol -> Path.Split
    splitVar = Path.splitFromName . Name.unsafeParseVar

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: DefnsF Set Name Name -> Names -> Set Reference
getExistingReferencesNamed defns names =
  bifoldMap fromTerms fromTypes defns
  where
    fromTerms :: Set Name -> Set Reference
    fromTerms =
      foldMap \name ->
        Set.map Referent.toReference (Relation.lookupDom name (Names.terms names))

    fromTypes :: Set Name -> Set TypeReference
    fromTypes =
      foldMap \name ->
        Relation.lookupDom name (Names.types names)

-- @getTermAndDeclNames file@ returns the names of the terms and decls defined in a typechecked Unison file.
getTermAndDeclNames :: (Var v) => TypecheckedUnisonFile v a -> DefnsF Set Name Name
getTermAndDeclNames tuf =
  Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms =
      UF.hashTermsId tuf
        & Map.foldMapWithKey \var (_, _, wk, _, _) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then Set.singleton (Name.unsafeParseVar var)
            else Set.empty
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeParseVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeParseVar . Decl.constructorVars

-- The big picture behind PPE building, though there are many details:
--
--   * We are updating old references to new references by rendering old references as names that are then parsed
--     back to resolve to new references (the world's weirdest implementation of AST substitution).
--
--   * We have to render names that refer to definitions in the file with a different suffixification strategy
--     (namely, "suffixify by name") than names that refer to things in the codebase.
--
--     This is because you *may* refer to aliases that share a suffix by that suffix for definitions in the
--     codebase, but not in the file.
--
--     For example, the following file will fail to parse:
--
--       one.foo = 10
--       two.foo = 10
--       hey = foo + foo -- "Which foo do you mean? There are two."
--
--     However, the following file will not fail to parse, if `one.foo` and `two.foo` are aliases in the codebase:
--
--       hey = foo + foo
makePPE ::
  Int ->
  Names ->
  Names ->
  DefnsF (Map Name) TermReferenceId TypeReferenceId ->
  PrettyPrintEnvDecl
makePPE hashLen namespaceNames initialFileNames dependents =
  PPED.addFallback
    ( let names = initialFileNames <> Names.fromUnconflictedReferenceIds dependents
       in PPED.makePPED (PPE.namer names) (PPE.suffixifyByName (Names.shadowing names namespaceNames))
    )
    ( PPED.makePPED
        (PPE.hqNamer hashLen namespaceNames)
        -- We don't want to over-suffixify for a reference in the namespace. For example, say we have "foo.bar" in the
        -- namespace and "oink.bar" in the file. "bar" may be a unique suffix among the namespace names, but would be
        -- ambiguous in the context of namespace + file names.
        --
        -- So, we use `shadowing`, which starts with the LHS names (the namespace), and adds to it names from the
        -- RHS (the initial file names, i.e. what was originally saved) that don't already exist in the LHS.
        (PPE.suffixifyByHash (Names.shadowing namespaceNames initialFileNames))
    )
