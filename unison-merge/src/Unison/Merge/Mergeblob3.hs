module Unison.Merge.Mergeblob3
  ( Mergeblob3 (..),
    makeMergeblob3,
  )
where

import Control.Lens (mapped)
import Data.Align (align)
import Data.Bifoldable (bifoldMap)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.These (These (..))
import Data.Zip (unzip)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup (..), expectConstructorNames)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Merge.Mergeblob2 (Mergeblob2 (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith, zipDefnsWith3, zipDefnsWith4)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Prelude hiding (unzip)

data Mergeblob3 = Mergeblob3
  { libdeps :: Names,
    stageOne :: DefnsF (Map Name) Referent TypeReference,
    stageTwo :: DefnsF (Map Name) Referent TypeReference,
    uniqueTypeGuids :: Map Name Text,
    -- `unparsedFile` (no mergetool) xor `unparsedSoloFiles` (yes mergetool) are ultimately given to the user
    unparsedFile :: Pretty ColorText,
    unparsedSoloFiles :: ThreeWay (Pretty ColorText)
  }

makeMergeblob3 ::
  Mergeblob2 libdep ->
  TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  Names ->
  Names ->
  TwoWay Text ->
  Mergeblob3
makeMergeblob3 blob dependents0 libdeps lcaLibdeps authors =
  let conflictsNames :: TwoWay (DefnsF Set Name Name)
      conflictsNames =
        bimap Map.keysSet Map.keysSet <$> blob.conflicts

      -- Identify the unconflicted dependents we need to pull into the Unison file (either first for typechecking, if
      -- there aren't conflicts, or else for manual conflict resolution without a typechecking step, if there are)
      dependents :: TwoWay (DefnsF Set Name Name)
      dependents =
        filterDependents
          conflictsNames
          blob.soloUpdatesAndDeletes
          ( let f :: Set TermReferenceId -> Referent -> NESet Name -> Set Name
                f deps defn0 names
                  | Just defn <- Referent.toTermReferenceId defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
                g :: Set TypeReferenceId -> TypeReference -> NESet Name -> Set Name
                g deps defn0 names
                  | ReferenceDerived defn <- defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
             in zipDefnsWith
                  (\defns deps -> Map.foldMapWithKey (f deps) (BiMultimap.domain defns))
                  (\defns deps -> Map.foldMapWithKey (g deps) (BiMultimap.domain defns))
                  <$> ThreeWay.forgetLca blob.defns
                  <*> dependents0
          )

      ppe :: PrettyPrintEnvDecl
      ppe =
        makePrettyPrintEnv
          (defnsToNames <$> blob.defns)
          libdeps
          lcaLibdeps

      renderedConflicts :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
      renderedDependents :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
      (renderedConflicts, renderedDependents) =
        renderConflictsAndDependents
          blob.declNameLookups
          (ThreeWay.forgetLca blob.hydratedDefns)
          conflictsNames
          dependents
          ppe

      renderedLcaConflicts :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
      renderedLcaConflicts =
        renderLcaConflicts
          blob.lcaDeclNameLookup
          blob.hydratedDefns.lca
          conflictsNames
          ppe
   in Mergeblob3
        { libdeps,
          stageOne =
            makeStageOne
              blob.declNameLookups
              conflictsNames
              blob.unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range blob.defns.lca),
          uniqueTypeGuids = makeUniqueTypeGuids (ThreeWay.forgetLca blob.hydratedDefns),
          stageTwo =
            makeStageTwo
              blob.declNameLookups
              conflictsNames
              blob.unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range <$> blob.defns),
          unparsedFile = makePrettyUnisonFile authors renderedConflicts renderedDependents,
          unparsedSoloFiles =
            ThreeWay
              { alice = renderedConflicts.alice,
                bob = renderedConflicts.bob,
                lca = renderedLcaConflicts
              }
              <&> \conflicts -> makePrettySoloUnisonFile conflicts renderedDependents
        }

filterDependents ::
  (Ord name) =>
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name)
filterDependents conflicts soloUpdatesAndDeletes dependents0 =
  -- There is some subset of Alice's dependents (and ditto for Bob of course) that we don't ultimately want/need to put
  -- into the scratch file: those for which any of the following are true:
  --
  --   1. It is Alice-conflicted (since we only want to return *unconflicted* things).
  --   2. It was deleted by Bob.
  --   3. It was updated by Bob and not updated by Alice.
  let dependents1 =
        zipDefnsWith Set.difference Set.difference
          <$> dependents0
          <*> (conflicts <> TwoWay.swap soloUpdatesAndDeletes)

      -- Of the remaining dependents, it's still possible that the maps are not disjoint. But whenever the same name key
      -- exists in Alice's and Bob's dependents, the value will either be equal (by Unison hash)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #alice} } }
      --
      -- ...or synhash-equal (i.e. the term or type received different auto-propagated updates)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #bob}   } }
      --
      -- So, we can arbitrarily keep Alice's, because they will render the same.
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {}                } }
      dependents2 =
        dependents1 & over #bob \bob ->
          zipDefnsWith Set.difference Set.difference bob dependents1.alice
   in dependents2

makeStageOne ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF (Map Name) term typ ->
  DefnsF (Map Name) term typ
makeStageOne declNameLookups conflicts unconflicts dependents =
  zipDefnsWith3 makeStageOneV makeStageOneV unconflicts (f conflicts <> f dependents)
  where
    f :: TwoWay (DefnsF Set Name Name) -> DefnsF Set Name Name
    f defns =
      fold (refIdsToNames <$> declNameLookups <*> defns)

makeStageOneV :: Unconflicts v -> Set Name -> Map Name v -> Map Name v
makeStageOneV unconflicts namesToDelete =
  (`Map.withoutKeys` namesToDelete) . Unconflicts.apply unconflicts

makeStageTwo ::
  forall term typ.
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF Set Name Name) ->
  ThreeWay (DefnsF (Map Name) term typ) ->
  DefnsF (Map Name) term typ
makeStageTwo declNameLookups conflicts unconflicts dependents defns =
  zipDefnsWith4 makeStageTwoV makeStageTwoV defns.lca aliceBiasedDependents unconflicts aliceConflicts
  where
    aliceConflicts :: DefnsF (Map Name) term typ
    aliceConflicts =
      zipDefnsWith
        (\defns conflicts -> Map.restrictKeys defns (conflicts <> aliceConstructorsOfTypeConflicts))
        Map.restrictKeys
        defns.alice
        conflicts.alice

    aliceConstructorsOfTypeConflicts :: Set Name
    aliceConstructorsOfTypeConflicts =
      foldMap
        (Set.fromList . DeclNameLookup.expectConstructorNames declNameLookups.alice)
        conflicts.alice.types

    aliceBiasedDependents :: DefnsF (Map Name) term typ
    aliceBiasedDependents =
      TwoWay.twoWay
        (zipDefnsWith (Map.unionWith const) (Map.unionWith const))
        (zipDefnsWith Map.restrictKeys Map.restrictKeys <$> ThreeWay.forgetLca defns <*> dependents)

makeStageTwoV :: Map Name v -> Map Name v -> Unconflicts v -> Map Name v -> Map Name v
makeStageTwoV lca dependents unconflicts conflicts =
  Map.unionWith const conflicts (Unconflicts.apply unconflicts (Map.unionWith const dependents lca))

-- Given just named term/type reference ids, fill out all names that occupy the term and type namespaces. This is simply
-- the given names plus all of the types' constructors.
--
-- For example, if the input is
--
--   declNameLookup = {
--     "Maybe" => ["Maybe.Nothing", "Maybe.Just"]
--   }
--   defns = {
--     terms = { "foo" => #foo }
--     types = { "Maybe" => #Maybe }
--   }
--
-- then the output is
--
--   defns = {
--     terms = { "foo", "Maybe.Nothing", "Maybe.Just" }
--     types = { "Maybe" }
--   }
refIdsToNames :: DeclNameLookup -> DefnsF Set Name Name -> DefnsF Set Name Name
refIdsToNames declNameLookup =
  bifoldMap goTerms goTypes
  where
    goTerms :: Set Name -> DefnsF Set Name Name
    goTerms terms =
      Defns {terms, types = Set.empty}

    goTypes :: Set Name -> DefnsF Set Name Name
    goTypes types =
      Defns
        { terms = foldMap (Set.fromList . expectConstructorNames declNameLookup) types,
          types
        }

renderConflictsAndDependents ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set Name Name) ->
  PrettyPrintEnvDecl ->
  ( TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)),
    TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
  )
renderConflictsAndDependents declNameLookups hydratedDefns conflicts dependents ppe =
  unzip $
    ( \declNameLookup (conflicts, dependents) ->
        let render = renderDefnsForUnisonFile declNameLookup ppe . over (#terms . mapped) snd
         in (render conflicts, render dependents)
    )
      <$> declNameLookups
      <*> hydratedConflictsAndDependents
  where
    hydratedConflictsAndDependents ::
      TwoWay
        ( DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann),
          DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)
        )
    hydratedConflictsAndDependents =
      ( \as bs cs ->
          ( zipDefnsWith Map.restrictKeys Map.restrictKeys as bs,
            zipDefnsWith Map.restrictKeys Map.restrictKeys as cs
          )
      )
        <$> hydratedDefns
        <*> conflicts
        <*> dependents

renderLcaConflicts ::
  PartialDeclNameLookup ->
  DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann) ->
  TwoWay (DefnsF Set Name Name) ->
  PrettyPrintEnvDecl ->
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
renderLcaConflicts partialDeclNameLookup hydratedDefns conflicts ppe =
  let hydratedConflicts = zipDefnsWith Map.restrictKeys Map.restrictKeys hydratedDefns (fold conflicts)
   in renderDefnsForUnisonFile declNameLookup ppe (over (#terms . mapped) snd hydratedConflicts)
  where
    -- We allow the LCA of a merge to have missing constructor names, yet we do need to render *something* in a file
    -- for a mergetool (if one is configured). So, we make the partial decl name lookup total by making bogus
    -- constructor names as necessary.
    declNameLookup :: DeclNameLookup
    declNameLookup =
      DeclNameLookup
        { constructorToDecl = partialDeclNameLookup.constructorToDecl,
          declToConstructors =
            makeTotal <$> partialDeclNameLookup.declToConstructors
        }
      where
        makeTotal :: [Maybe Name] -> [Name]
        makeTotal names0 =
          case sequence names0 of
            Just names -> names
            Nothing ->
              snd $
                List.mapAccumL
                  makeSomethingUp
                  (foldMap (maybe Set.empty Set.singleton) names0)
                  names0
          where
            makeSomethingUp :: Set Name -> Maybe Name -> (Set Name, Name)
            makeSomethingUp taken = \case
              Just name -> (taken, name)
              Nothing ->
                let name = freshen 0 "Unnamed"
                    !taken1 = Set.insert name taken
                 in (taken1, name)
              where
                freshen :: Int -> Text -> Name
                freshen i name0
                  | Set.member name taken = freshen (i + 1) name0
                  | otherwise = name
                  where
                    name :: Name
                    name =
                      Name.unsafeParseText (name0 <> if i == 0 then Text.empty else Text.pack (show i))

-- Create a PPE that uses Alice's names whenever possible, falling back to Bob's names only when Alice doesn't have any,
-- and falling back to the LCA after that.
--
-- This results in a file that "looks familiar" to Alice (the one merging in Bob's changes), and avoids superfluous
-- textual conflicts that would arise from preferring Bob's names for Bob's code (where his names differ).
--
-- The LCA names are not used unless we need to render LCA definitions for a mergetool, but we add them to the PPE in
-- all cases anyway. If this is very expensive, we could consider omitting them in the case that no mergetool is
-- configured.
--
-- Note that LCA names can make name quality slightly worse. For example, "foo.bar" might exist in the LCA, but deleted
-- in Alice and Bob, and nonetheless prevent some "qux.bar" from rendering as "bar". That seems fine.
makePrettyPrintEnv :: ThreeWay Names -> Names -> Names -> PrettyPrintEnvDecl
makePrettyPrintEnv names libdepsNames lcaLibdeps =
  PPED.makePPED
    ( PPE.namer
        ( Names.preferring
            (Names.preferring names.alice names.bob <> libdepsNames)
            (names.lca <> lcaLibdeps)
        )
    )
    (PPE.suffixifyByName (fold names <> libdepsNames))

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames defns =
  Names
    { terms = Relation.fromMap (BiMultimap.range defns.terms),
      types = Relation.fromMap (BiMultimap.range defns.types)
    }

makePrettyUnisonFile ::
  TwoWay Text ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettyUnisonFile authors conflicts dependents =
  fold
    [ conflicts
        -- Merge the two maps together into one, remembering who authored what
        & TwoWay.twoWay (zipDefnsWith align align)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each conflict, types then terms (even though a type can conflict with a term, in which case they
        -- would not be adjacent in the file), with an author comment above each conflicted thing
        & ( let f =
                  foldMap \case
                    This x -> alice x
                    That y -> bob y
                    These x y -> alice x <> bob y
                  where
                    alice = prettyBinding (Just (Pretty.text authors.alice))
                    bob = prettyBinding (Just (Pretty.text authors.bob))
             in bifoldMap f f
          ),
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      let thereAre defns = TwoWay.or (not . defnsAreEmpty <$> defns)
       in if thereAre conflicts && thereAre dependents
            then
              fold
                [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
                  "-- conflicted definitions above.\n\n"
                ]
            else mempty,
      makePrettyDependents dependents
    ]
  where
    prettyBinding maybeComment binding =
      fold
        [ case maybeComment of
            Nothing -> mempty
            Just comment -> "-- " <> comment <> "\n",
          binding,
          "\n\n"
        ]

makePrettySoloUnisonFile ::
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettySoloUnisonFile conflicts dependents =
  fold
    [ conflicts
        & inAlphabeticalOrder
        & let f = foldMap (<> "\n\n") in bifoldMap f f,
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      if not (defnsAreEmpty conflicts) && TwoWay.or (not . defnsAreEmpty <$> dependents)
        then
          fold
            [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
              "-- conflicted definitions.\n\n"
            ]
        else mempty,
      -- Include all dependents when invoking this function with alice/bob/lca conflicts, because we don't want any diff
      -- here – we want the mergetool to copy over all dependents after resolving the real conflicts above the fold.
      makePrettyDependents dependents
    ]

makePrettyDependents :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) -> Pretty ColorText
makePrettyDependents =
  -- Merge dependents together into one map (they are disjoint)
  TwoWay.twoWay (zipDefnsWith Map.union Map.union)
    >>>
    -- Sort alphabetically
    inAlphabeticalOrder
    -- Render each dependent, types then terms, without bothering to comment attribution
    >>> (let f = foldMap (<> "\n\n") in bifoldMap f f)

inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
inAlphabeticalOrder =
  bimap f f
  where
    f = map snd . List.sortOn (Name.toText . fst) . Map.toList

-- Given Alice's and Bob's hydrated defns, make a mapping from unique type name to unique type GUID, preferring Alice's
-- GUID if they both have one.
makeUniqueTypeGuids ::
  TwoWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Map Name Text
makeUniqueTypeGuids hydratedDefns =
  let -- Start off with just Alice's GUIDs
      aliceGuids :: Map Name Text
      aliceGuids =
        Map.mapMaybe (declGuid . snd) hydratedDefns.alice.types

      -- Define a helper that adds a Bob GUID only if it's not already in the map (so, preferring Alice)
      addBobGuid :: Map Name Text -> (Name, (TypeReferenceId, Decl Symbol Ann)) -> Map Name Text
      addBobGuid acc (name, (_, bobDecl)) =
        Map.alter
          ( \case
              Nothing -> bobGuid
              Just aliceGuid -> Just aliceGuid
          )
          name
          acc
        where
          bobGuid :: Maybe Text
          bobGuid =
            declGuid bobDecl

      -- Tumble in all of Bob's GUIDs with that helper
      allTheGuids :: Map Name Text
      allTheGuids =
        List.foldl' addBobGuid aliceGuids (Map.toList hydratedDefns.bob.types)
   in allTheGuids
  where
    declGuid :: Decl v a -> Maybe Text
    declGuid decl =
      case (DataDeclaration.asDataDecl decl).modifier of
        DataDeclaration.Structural -> Nothing
        DataDeclaration.Unique guid -> Just guid
