module Unison.Codebase.Editor.HandleInput.Update
  ( handleUpdate,
    doSlurpAdds,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.ABT as ABT
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.NamesUtils (displayNames)
import Unison.Cli.PrettyPrintUtils (prettyPrintEnvDecl)
import Unison.Cli.TypeCheck (typecheckFile)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchUtil as BranchUtil
import Unison.Codebase.Editor.HandleInput.MetadataUtils (addDefaultMetadata)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Propagate as Propagate
import qualified Unison.Codebase.Editor.Slurp as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.SlurpResult (SlurpResult (..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Name (Name)
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import Unison.Reference (Reference (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import qualified Unison.Runtime.IOSource as IOSource
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Name as Name (toVar, unsafeFromVar)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.UnisonFile.Type (UnisonFile (UnisonFileId))
import qualified Unison.Util.Map as Map (remap, upsert)
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Set as Set
import qualified Unison.Var as Var
import Unison.WatchKind (WatchKind)
import qualified Unison.WatchKind as WK

-- | Handle an @update@ command.
handleUpdate :: Input -> OptionalPatch -> Set Name -> Cli ()
handleUpdate input optionalPatch requestedNames = do
  Cli.Env {codebase} <- ask
  currentPath' <- Cli.getCurrentPath
  let patchPath =
        case optionalPatch of
          NoPatch -> Nothing
          DefaultPatch -> Just Cli.defaultPatchPath
          UsePatch p -> Just p
  slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0
  sr <- getSlurpResultForUpdate requestedNames slurpCheckNames
  let addsAndUpdates :: SlurpComponent
      addsAndUpdates = Slurp.updates sr <> Slurp.adds sr
      fileNames :: Names
      fileNames = UF.typecheckedToNames (Slurp.originalFile sr)
      -- todo: display some error if typeEdits or termEdits itself contains a loop
      typeEdits :: [(Name, Reference, Reference)]
      typeEdits = do
        v <- Set.toList (SC.types (updates sr))
        let n = Name.unsafeFromVar v
        let oldRefs0 = Names.typesNamed slurpCheckNames n
        let newRefs = Names.typesNamed fileNames n
        case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
          Nothing -> error (reportBug "E722145" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
          Just (oldRefs, newRef) -> do
            oldRef <- Foldable.toList oldRefs
            [(n, oldRef, newRef)]
      hashTerms :: Map Reference (Type Symbol Ann)
      hashTerms = Map.fromList (toList hashTerms0)
        where
          hashTerms0 = (\(r, _wk, _tm, typ) -> (r, typ)) <$> UF.hashTerms (Slurp.originalFile sr)
      termEdits :: [(Name, Reference, Reference)]
      termEdits = do
        v <- Set.toList (SC.terms (updates sr))
        let n = Name.unsafeFromVar v
        let oldRefs0 = Names.refTermsNamed slurpCheckNames n
        let newRefs = Names.refTermsNamed fileNames n
        case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
          Nothing -> error (reportBug "E936103" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
          Just (oldRefs, newRef) -> do
            oldRef <- Foldable.toList oldRefs
            [(n, oldRef, newRef)]
      termDeprecations :: [(Name, Referent)]
      termDeprecations =
        [ (n, r)
          | (_, oldTypeRef, _) <- typeEdits,
            (n, r) <- Names.constructorsForType oldTypeRef slurpCheckNames
        ]
  patchOps <- for patchPath \patchPath -> do
    ye'ol'Patch <- Cli.getPatchAt patchPath
    -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
    -- with (a0 -> a') in patch'.
    -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
    -- we must know the type of a0, a, a'.
    let -- we need:
        -- all of the `old` references from the `new` edits,
        -- plus all of the `old` references for edits from patch we're replacing
        collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
        collectOldForTyping new old = foldl' f mempty (new ++ fromOld)
          where
            f acc (r, _r') = Set.insert r acc
            newLHS = Set.fromList . fmap fst $ new
            fromOld :: [(Reference, Reference)]
            fromOld =
              [ (r, r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old, Set.member r' newLHS
              ]
        neededTypes = collectOldForTyping (map (\(_, old, new) -> (old, new)) termEdits) ye'ol'Patch

    allTypes :: Map Reference (Type v Ann) <-
      (liftIO . Codebase.runTransaction codebase) do
        fmap Map.fromList . for (toList neededTypes) $ \r ->
          (r,) . fromMaybe (Type.builtin External "unknown type")
            <$> Codebase.getTypeOfTerm codebase r

    let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 hashTerms) of
          (Just t1, Just t2)
            | Typechecker.isEqual t1 t2 -> TermEdit.Same
            | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
            | otherwise -> TermEdit.Different
          e ->
            error $
              "compiler bug: typing map not constructed properly\n"
                <> "typing "
                <> show r1
                <> " "
                <> show r2
                <> " : "
                <> show e

        updatePatch :: Patch -> Patch
        updatePatch p = foldl' step2 p' termEdits
          where
            p' = foldl' step1 p typeEdits
            step1 p (_, r, r') = Patch.updateType r (TypeEdit.Replace r') p
            step2 p (_, r, r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
        (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
        updatePatches :: Monad m => Branch0 m -> m (Branch0 m)
        updatePatches = Branch.modifyPatches seg updatePatch
    pure (updatePatch ye'ol'Patch, updatePatches, p)

  when (Slurp.hasAddsOrUpdates sr) $ do
    -- take a look at the `updates` from the SlurpResult
    -- and make a patch diff to record a replacement from the old to new references
    Cli.stepManyAtMNoSync
      ( [ ( Path.unabsolute currentPath',
            pure . doSlurpUpdates typeEdits termEdits termDeprecations
          ),
          ( Path.unabsolute currentPath',
            pure . doSlurpAdds addsAndUpdates (Slurp.originalFile sr)
          )
        ]
          ++ case patchOps of
            Nothing -> []
            Just (_, update, p) -> [(Path.unabsolute p, update)]
      )
    Cli.runTransaction
      . Codebase.addDefsToCodebase codebase
      . Slurp.filterUnisonFile sr
      $ Slurp.originalFile sr
  ppe <- prettyPrintEnvDecl =<< displayNames (Slurp.originalFile sr)
  Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
  whenJust patchOps \(updatedPatch, _, _) ->
    void $ propagatePatchNoSync updatedPatch currentPath'
  addDefaultMetadata addsAndUpdates
  Cli.syncRoot case patchPath of
    Nothing -> "update.nopatch"
    Just p ->
      p & Path.unsplit'
        & Path.resolve @_ @_ @Path.Absolute currentPath'
        & tShow

getSlurpResultForUpdate :: Set Name -> Names -> Cli SlurpResult
getSlurpResultForUpdate requestedNames slurpCheckNames = do
  let slurp :: TypecheckedUnisonFile Symbol Ann -> SlurpResult
      slurp file =
        Slurp.slurpFile file (Set.map Name.toVar requestedNames) Slurp.UpdateOp slurpCheckNames

  let termRefToNames :: TermReferenceId -> Set Symbol
      termRefToNames =
        Set.map Name.toVar . Names.namesForReferent slurpCheckNames . Referent.fromTermReferenceId

  let nameToTermRefs :: Symbol -> Set TermReference
      nameToTermRefs = Names.refTermsNamed slurpCheckNames . Name.unsafeFromVar

  slurp1 <- do
    Cli.Env {codebase} <- ask
    unisonFile0 <- Cli.expectLatestTypecheckedFile

    -- Here, we identify whether there are any "implicit terms", which are terms that should be brought into the latest
    -- typechecked Unsion file and re-typechecked, which also re-discovers components.
    --
    -- The running example here will be one in which the current namespace already has the following terms stored.
    -- (For simplicity, we will ignore the limitation that mutually recursive definitions must be top-level arrows;
    -- pretend this is Haskell's let).
    --
    --   ping = pong + 1   (reference = #pingpong.ping)
    --   pong = ping + 2   (reference = #pingpong.pong)
    --   wham = pong + 3   (reference = #wham)
    --
    -- The user re-defines "ping" in their scratch file thus:
    --
    --   ping = wham + 4   (reference = #newping)
    --
    -- Note that, pre-update, we had two components [ping,pong] and [wham]. But after the update, since the new `ping`
    -- refers to the old `wham`, which refers to the old `pong`, which refers to the old `ping`, we really want to end
    -- up with a single component in the end, [ping,pong,wham].

    -- First, compute an initial slurp, which will identify the initial set of definitions we are updating ({"ping"}).
    -- Also, this will be the slurp that we fall back on, in case re-typechecking another Unison file with implicit
    -- terms in it fails.
    let slurp0 = slurp unisonFile0

    -- Grab some interim info out of the original slurp.
    --
    -- Running example:
    --
    --   "ping" => (#newping, Nothing, <#wham + 4>, <Nat>)
    let nameToInterimInfo :: Map Symbol (TermReferenceId, Maybe WatchKind, Term Symbol Ann, Type Symbol Ann)
        nameToInterimInfo =
          UF.hashTermsId (Slurp.originalFile slurp0)

    -- Get the set of names that are being updated.
    --
    -- Running example:
    --
    --   { "ping" }
    let namesBeingUpdated :: Set Symbol
        namesBeingUpdated =
          SC.terms (Slurp.updates slurp0)

    -- Associate each such name with the set of old (already in the codebase) and new (in the scratch file) references
    -- that it's associated with.
    --
    -- Running example:
    --
    --   "ping" => ({ #pingpong.ping }, #newping)
    let updatedNameToRefs :: Map Symbol (Set TermReference, TermReferenceId)
        updatedNameToRefs =
          Map.fromSet
            ( \name ->
                case Map.lookup name nameToInterimInfo of
                  Nothing -> error (reportBug "E798907" "no interim ref for name")
                  Just (interimRef, _, _, _) -> (nameToTermRefs name, interimRef)
            )
            namesBeingUpdated

    -- Identify all of the implicit terms, which are:
    --
    --   - Both:
    --     - Either:
    --         - (A) A component-mate of a term that's being updated.
    --         - Both:
    --           - (B) A dependent of a term that's being updated.
    --           - (C) A dependency of the new term.
    --     - (D) Not being updated.
    --
    -- FIXME Additionally, we have one more temporary requirement.
    --
    --   - (E) The term has at least one unambiguous (unconflicted) name in the codebase.
    --
    -- This works around two fixable issues:
    --
    --   1. If the term has no names in the namespace, then we can't successfully put it into the Unison file anyway and
    --      forward it through the typecheck + slurp process, because slurping currently assumes that it can freely
    --      convert back-and-forth between vars and names.
    --
    --   2. If the term has only ambiguous/conflicted names, then putting one of them in the Unison file and proceeding
    --      to do an update would have the undesirable side-effect of resolving the conflict.
    --
    -- FIXME don't bother for type-changing updates
    --
    -- In our running example, the full list of component-mates (A) of the terms being updated is:
    --
    --   [ #pingpong.ping, #pingpong.pong ]
    --
    -- And because #pingpong.ping is being updated, due to (D), only #pingpong.pong remains.
    --
    -- Furthermore, #wham is both a dependent of #pingpong (B), and a dependency of #newping, so it too is an implicit
    -- term.
    --
    -- Running example:
    --
    --   #pingpong.pong => (<#pingpong.ping + 2>, "pong")
    --   #wham          => (<#pingpong.pong + 3>, "wham")
    implicitTerms :: Map TermReferenceId (Term Symbol Ann, Symbol) <-
      liftIO do
        Codebase.withConnection codebase \conn ->
          Sqlite.runTransaction conn do
            -- Running example:
            --
            --   #pingpong => #newping
            let oldHashToInterimHash :: Map Hash Hash
                oldHashToInterimHash =
                  updatedNameToRefs & foldMap \(oldRefs, interimRef) ->
                    let interimHash = Reference.idToHash interimRef
                     in Map.fromList $
                          oldRefs
                            & Set.toList
                            & mapMaybe Reference.toHash
                            & map (,interimHash)

            let hashToImplicitTerms :: Hash -> Sqlite.Transaction (Map TermReferenceId (Term Symbol Ann, Symbol))
                hashToImplicitTerms hash = do
                  -- Running example (for `oldHash` iteration):
                  --
                  --   [ (<#pingpong.pong + 1>, <Nat>),
                  --     (<#pingpong.ping + 2>, <Nat>)
                  --   ]
                  terms <- Codebase.unsafeGetTermComponent codebase hash
                  let keep :: TermReferenceId -> Maybe Symbol
                      keep ref =
                        if notBeingUpdated names
                          then Foldable.find nameIsUnconflicted names -- (E)
                          else Nothing
                        where
                          -- (D) After getting the entire component of `oldHash`, which has at least one member being
                          -- updated, we want to keep only the members that are *not* being updated (i.e. those who have
                          -- no name that is being updated).
                          --
                          -- Running example, first time through (processing #pingpong.ping):
                          --
                          --   Set.disjoint { "ping" } { "ping" } is false, so don't add to the map.
                          --
                          -- Running example, second time through (processing #pingpong.pong):
                          --
                          --   Set.disjoint { "ping" } { "pong" } is true, so add
                          --   #pingpong.pong => (<#pingpong.ping + 2>, { "pong" })) to the map.
                          notBeingUpdated = Set.disjoint namesBeingUpdated
                          nameIsUnconflicted name = Set.size (nameToTermRefs name) == 1
                          names = termRefToNames ref
                  pure $
                    terms
                      -- Running example:
                      --
                      --   [ (#pingpong.ping, (<#pingpong.pong + 1>, <Nat>)),
                      --     (#pingpong.pong, (<#pingpong.ping + 2>, <Nat>))
                      --   ]
                      & Reference.componentFor hash
                      & List.foldl'
                        ( \acc (ref, (term, _typ)) ->
                            case keep ref of
                              Nothing -> acc
                              Just name -> Map.insert ref (term, name) acc
                        )
                        Map.empty

            if Map.null oldHashToInterimHash
              then pure Map.empty
              else do
                Sqlite.savepoint do
                  -- Compute the actual interim decl/term components in the latest typechecked file. These aren't quite
                  -- given in the unison file structure itself - in the `topLevelComponents'` field we have the
                  -- components in some arbitrary order (I *think*), each tagged with its stringy name, and in the
                  -- `hashTermsId` field we have all of the individual terms organized by reference.
                  let interimDeclComponents :: [(Hash, [Decl Symbol Ann])]
                      interimDeclComponents =
                        decls UF.dataDeclarationsId' Right ++ decls UF.effectDeclarationsId' Left
                        where
                          decls ::
                            (TypecheckedUnisonFile Symbol Ann -> Map Symbol (TypeReferenceId, decl)) ->
                            (decl -> Decl Symbol Ann) ->
                            [(Hash, [Decl Symbol Ann])]
                          decls project inject =
                            slurp0
                              & Slurp.originalFile
                              & project
                              & Map.elems
                              & recomponentize
                              & over (mapped . _2 . mapped) inject
                      interimTermComponents :: [(Hash, [(Term Symbol Ann, Type Symbol Ann)])]
                      interimTermComponents =
                        nameToInterimInfo
                          & Map.elems
                          & map (\(ref, _wk, term, typ) -> (ref, (term, typ)))
                          & componentize
                          & uncomponentize

                  -- Insert each interim component into the codebase proper. Note: this relies on the codebase interface
                  -- being smart enough to handle out-of-order components (i.e. inserting a dependent before a
                  -- dependency). That's currently how the codebase interface works, but maybe in the future it'll grow
                  -- a precondition that components can only be inserted after their dependencies.
                  for_ interimDeclComponents \(hash, decls) -> Codebase.putTypeDeclarationComponent codebase hash decls
                  for_ interimTermComponents \(hash, terms) -> Codebase.putTermComponent codebase hash terms

                  terms <-
                    let interimHashes :: Set Hash
                        interimHashes = Set.fromList (map fst interimTermComponents)
                     in Map.toList oldHashToInterimHash & foldMapM \(oldHash, interimHash) -> do
                          hashes <-
                            Queries.loadObjectIdForAnyHash oldHash >>= \case
                              -- better would be to short-circuit all the way to the user and say, actually we can't
                              -- perform this update at all, due to some intervening delete (e.g. some sort of
                              -- hard-reset or garbage collection on the codebase)
                              Nothing -> pure Set.empty
                              Just oldOid -> do
                                interimOid <- Queries.expectObjectIdForPrimaryHash interimHash
                                betweenOids <- Queries.getDependenciesBetweenTerms interimOid oldOid
                                (Set.\\ interimHashes) <$> Set.traverse Queries.expectPrimaryHashByObjectId betweenOids
                          foldMapM hashToImplicitTerms (oldHash : Set.toList hashes)
                  pure (Left terms) -- left = rollback to savepoint
    if Map.null implicitTerms
      then pure slurp0
      else do
        -- We found some implicit terms, so it's time to:
        --
        --   1. Reconstruct a new unison file out of the latest typechecked unison file, plus all of the implicit terms,
        --      taking care to adjust their references to each other, so that the proper components are discovered.
        --
        --   2. Re-typecheck, and if it that succeeds, use the resulting typechecked unison file and slurp.

        -- Remap the references contained in each implicit term, then add back in the slurped interim terms and unhash
        -- the collection. The unhashing process will invent a fresh variable name for each term.
        --
        -- Running example:
        --
        --   #newping       => ("fresh1", <"fresh3" + 4>)
        --   #pingpong.pong => ("fresh2", <"fresh1" + 2>)
        --   #wham          => ("fresh3", <"fresh2" + 3>)
        let refToGeneratedNameAndTerm :: Map TermReferenceId (Symbol, Term Symbol Ann)
            refToGeneratedNameAndTerm =
              -- Running example:
              --
              --   #pingpong.pong => (<#pingpong.ping + 2>, { "pong" })
              --   #wham          => (<#pingpong.pong + 3>, { "wham" })
              implicitTerms
                -- Running example:
                --
                --   #pingpong.pong => <#newping + 2>
                --   #wham          => <#pingpong.pong + 3>
                & Map.map (\(term, _names) -> rewrite term)
                -- Running example:
                --
                --   #newping       => <#wham + 4>
                --   #pingpong.pong => <#newping + 2>
                --   #wham          => <#pingpong.pong + 3>
                & Map.union interimRefToTerm
                & Term.unhashComponent
              where
                -- Running example:
                --
                --   #newping => <#wham + 4>
                interimRefToTerm :: Map TermReferenceId (Term Symbol Ann)
                interimRefToTerm =
                  Map.remap (\(_var, (ref, _wk, term, _typ)) -> (ref, term)) nameToInterimInfo
                -- Running example: apply the following reference mapping everwhere in a term:
                --
                --   #pingpong.ping -> #newping
                --   ref            -> ref
                rewrite :: Term Symbol Ann -> Term Symbol Ann
                rewrite =
                  rewriteTermReferences (foldMap toMapping updatedNameToRefs)
                  where
                    toMapping ::
                      (Set TermReference, TermReferenceId) ->
                      Map TermReference TermReferenceId
                    toMapping (oldRefs, interimRef) =
                      foldMap (\oldRef -> Map.singleton oldRef interimRef) oldRefs

        let unisonFile :: UnisonFile Symbol Ann
            unisonFile =
              UnisonFileId
                { dataDeclarationsId = UF.dataDeclarationsId' (Slurp.originalFile slurp0),
                  effectDeclarationsId = UF.effectDeclarationsId' (Slurp.originalFile slurp0),
                  -- Running example:
                  --
                  --   fresh1 = fresh3 + 4
                  --   fresh2 = fresh1 + 2
                  --   fresh3 = fresh2 + 3
                  terms = Map.elems refToGeneratedNameAndTerm,
                  -- In the context of this update, whatever watches were in the latest typechecked Unison file are
                  -- irrelevant, so we don't need to copy them over.
                  watches = Map.empty
                }
        result <- liftIO (Codebase.runTransaction codebase (typecheckFile codebase [] unisonFile))
        case runIdentity (Result.toMaybe result) of
          Just (Right file0) -> do
            -- Map each name generated by unhashing back to the name it should have in the Unison file we're going to
            -- typecheck.
            --
            -- Running example:
            --
            --   "fresh1" -> "ping"
            --   "fresh2" -> "pong"
            --   "fresh3" -> "wham"
            let generatedNameToName :: Map Symbol Symbol
                generatedNameToName =
                  refToGeneratedNameAndTerm & Map.remap \(ref, (generatedName, _term)) ->
                    ( generatedName,
                      case Map.lookup ref interimRefToName of
                        Just name -> name
                        Nothing ->
                          case Map.lookup ref implicitTerms of
                            Just (_term, name) -> name
                            Nothing -> error (reportBug "E836680" "ref not interim nor implicit")
                    )
                  where
                    -- Associate each term name being updated with its interim reference.
                    --
                    -- Running example:
                    --
                    --   #newping => "ping"
                    interimRefToName :: Map TermReferenceId Symbol
                    interimRefToName =
                      Map.remap (\(name, (ref, _wk, _term, _typ)) -> (ref, name)) nameToInterimInfo

            let renameTerm ::
                  (Symbol, Term Symbol Ann, Type Symbol Ann) ->
                  (Symbol, Term Symbol Ann, Type Symbol Ann)
                renameTerm (generatedName, term, typ) =
                  ( case Map.lookup generatedName generatedNameToName of
                      Just name -> name
                      Nothing -> error (reportBug "E440546" "no name for generated name"),
                    ABT.renames generatedNameToName term,
                    typ
                  )

            let file1 :: TypecheckedUnisonFile Symbol Ann
                file1 =
                  UF.typecheckedUnisonFile
                    (file0 ^. #dataDeclarationsId')
                    (file0 ^. #effectDeclarationsId')
                    ((file0 ^. #topLevelComponents') & over (mapped . mapped) renameTerm)
                    ((file0 ^. #watchComponents) & over (mapped . _2 . mapped) renameTerm)

            pure (slurp file1)
          _ -> pure slurp0

  pure slurp1

rewriteTermReferences :: Ord v => Map TermReference TermReferenceId -> Term v a -> Term v a
rewriteTermReferences mapping =
  ABT.rebuildUp \term ->
    case term of
      Term.Ref ref0 ->
        case Map.lookup ref0 mapping of
          Nothing -> term
          Just ref1 -> Term.Ref (Reference.fromId ref1)
      _ -> term

-- updates the namespace for adding `slurp`
doSlurpAdds ::
  forall m.
  Monad m =>
  SlurpComponent ->
  TypecheckedUnisonFile Symbol Ann ->
  (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
  where
    typeActions = map doType . toList $ SC.types slurp
    termActions =
      map doTerm . toList $
        SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
    names = UF.typecheckedToNames uf
    tests = Set.fromList $ fst <$> UF.watchesOfKind WK.TestWatch (UF.discardTypes uf)
    (isTestType, isTestValue) = IOSource.isTest
    md v =
      if Set.member v tests
        then Metadata.singleton isTestType isTestValue
        else Metadata.empty
    doTerm :: Symbol -> (Path, Branch0 m -> Branch0 m)
    doTerm v = case toList (Names.termsNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeFromVar v)
         in BranchUtil.makeAddTermName split r (md v)
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple terms named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    doType :: Symbol -> (Path, Branch0 m -> Branch0 m)
    doType v = case toList (Names.typesNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeFromVar v)
         in BranchUtil.makeAddTypeName split r Metadata.empty
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple types named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates ::
  Monad m =>
  [(Name, TypeReference, TypeReference)] ->
  [(Name, TermReference, TermReference)] ->
  [(Name, Referent)] ->
  (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits deprecated b0 =
  Branch.batchUpdates (typeActions <> termActions <> deprecateActions) b0
  where
    typeActions = join . map doType $ typeEdits
    termActions = join . map doTerm $ termEdits
    deprecateActions = join . map doDeprecate $ deprecated
      where
        doDeprecate (n, r) = [BranchUtil.makeDeleteTermName (Path.splitFromName n) r]

    -- we copy over the metadata on the old thing
    -- todo: if the thing being updated, m, is metadata for something x in b0
    -- update x's md to reference `m`
    doType :: (Name, TypeReference, TypeReference) -> [(Path, Branch0 m -> Branch0 m)]
    doType (n, old, new) =
      let split = Path.splitFromName n
          oldMd = BranchUtil.getTypeMetadataAt split old b0
       in [ BranchUtil.makeDeleteTypeName split old,
            BranchUtil.makeAddTypeName split new oldMd
          ]
    doTerm :: (Name, TermReference, TermReference) -> [(Path, Branch0 m -> Branch0 m)]
    doTerm (n, old, new) =
      [ BranchUtil.makeDeleteTermName split (Referent.Ref old),
        BranchUtil.makeAddTermName split (Referent.Ref new) oldMd
      ]
      where
        split = Path.splitFromName n
        -- oldMd is the metadata linked to the old definition
        -- we relink it to the new definition
        oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync :: Patch -> Path.Absolute -> Cli Bool
propagatePatchNoSync patch scopePath =
  Cli.time "propagatePatchNoSync" do
    Cli.stepAtNoSync' (Path.unabsolute scopePath, Propagate.propagateAndApply patch)

recomponentize :: [(Reference.Id, a)] -> [(Hash, [a])]
recomponentize =
  uncomponentize . componentize

-- Misc. helper: convert a component in listy-form to mappy-form.
componentize :: [(Reference.Id, a)] -> Map Hash (Map Reference.Pos a)
componentize =
  foldl' step Map.empty
  where
    step :: Map Hash (Map Reference.Pos a) -> (Reference.Id, a) -> Map Hash (Map Reference.Pos a)
    step acc (Reference.Id hash pos, x) =
      Map.upsert
        ( \case
            Nothing -> Map.singleton pos x
            Just acc1 -> Map.insert pos x acc1
        )
        hash
        acc

-- Misc. helper: convert a component in mappy-form to listy-form.
uncomponentize :: Map Hash (Map Reference.Pos a) -> [(Hash, [a])]
uncomponentize =
  over (mapped . _2) Map.elems . Map.toList
