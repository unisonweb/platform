{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.Completion
  ( completionHandler,
    completionItemResolveHandler,
    namesToCompletionTree,
    -- Exported for transcript tests
    completionsForQuery,
  )
where

import Control.Comonad.Cofree
import Control.Lens hiding (List, (:<))
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.Extra (nubOrdOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Text.Megaparsec qualified as Megaparsec
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LSP.FileAnalysis
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (toText)
import Unison.Syntax.Name qualified as Name (nameP, parseText, toText)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import UnliftIO qualified

completionHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentCompletion -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond . maybe (Right $ InL mempty) (Right . InR . InL) =<< runMaybeT do
    let fileUri = (m ^. params . textDocument . uri)
    (range, prefix) <- VFS.completionPrefix (m ^. params . textDocument . uri) (m ^. params . position)
    ppe <- PPED.suffixifiedPPE <$> lift currentPPED
    codebaseCompletions <- lift getCodebaseCompletions
    let (isIncomplete, matches) = completionsForQuery codebaseCompletions prefix
    let defCompletionItems =
          matches
            & mapMaybe \(path, fqn, dep) ->
              let biasedPPE = PPE.biasTo [fqn] ppe
                  hqName = LD.fold (PPE.types biasedPPE) (PPE.terms biasedPPE) dep
               in hqName <&> \hqName -> mkDefCompletionItem fileUri range (HQ'.toName hqName) fqn path (HQ'.toText hqName) dep

    let itemDefaults = Nothing
    pure . CompletionList isIncomplete itemDefaults $ defCompletionItems
  where

completionsForQuery :: CompletionTree -> Text -> (Bool, [(Text, Name, LabeledDependency)])
completionsForQuery codebaseCompletions prefix =
  let defMatches = matchCompletions codebaseCompletions prefix
      (isIncomplete, defCompletions) =
        defMatches
          -- sort shorter names first
          & sortOn (matchSortCriteria . view _2)
          & nubOrdOn (\(p, _name, ref) -> (p, ref))
          & fmap (over _1 Path.toText)
          & (False,)
   in (isIncomplete, defCompletions)

-- Takes at most the specified number of completions, but also indicates with a boolean
-- whether there were more completions remaining so we can pass that along to the client.
-- takeCompletions :: Int -> [a] -> (Bool, [a])
-- takeCompletions 0 xs = (not $ null xs, [])
-- takeCompletions _ [] = (False, [])
-- takeCompletions n (x : xs) = second (x :) $ takeCompletions (pred n) xs

mkDefCompletionItem :: Uri -> Range -> Name -> Name -> Text -> Text -> LabeledDependency -> CompletionItem
mkDefCompletionItem fileUri range relativeName fullyQualifiedName path suffixified dep =
  CompletionItem
    { _label = lbl,
      _labelDetails = Nothing,
      _kind = case dep of
        LD.TypeReference _ref -> Just CompletionItemKind_Class
        LD.TermReferent ref -> case ref of
          Referent.Con {} -> Just CompletionItemKind_Constructor
          Referent.Ref {} -> Just CompletionItemKind_Value,
      _tags = Nothing,
      _detail = Just (Name.toText fullyQualifiedName),
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText =
        let (nls, ns, fn) = matchSortCriteria fullyQualifiedName
         in Just $ Text.intercalate "|" [paddedInt nls, paddedInt ns, Name.toText fn],
      _filterText = Just path,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Just (InL $ TextEdit range suffixified),
      _textEditText = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _data_ = Just $ Aeson.toJSON $ CompletionItemDetails {dep, relativeName, fullyQualifiedName, fileUri}
    }
  where
    -- Pads an integer with zeroes so it sorts lexicographically in the right order
    --
    -- >>> paddedInt 1
    -- "00001"
    paddedInt :: Int -> Text
    paddedInt n =
      Text.justifyRight 5 '0' (Text.pack $ show n)
    -- We should generally show the longer of the path or suffixified name in the label,
    -- it helps the user understand the difference between options which may otherwise look
    -- the same.
    --
    -- E.g. if I type "ma" then the suffixied options might be: List.map, Bag.map, but the
    -- path matches are just "map" and "map" since the query starts at that segment, so we
    -- show the suffixified version to disambiguate.
    --
    -- However, if the user types "base.List.ma" then the matching path is "base.List.map" and
    -- the suffixification is just "List.map", so we use the path in this case because it more
    -- closely matches what the user actually typed.
    --
    -- This is what's felt best to me, anecdotally.
    lbl =
      if Text.length path > Text.length suffixified
        then path
        else suffixified

-- | LSP clients sort completions using a text field, so we have to convert Unison's sort criteria to text.
matchSortCriteria :: Name -> (Int, Int, Name)
matchSortCriteria fqn =
  (numLibSegments, numSegments, fqn)
  where
    numSegments :: Int
    numSegments =
      Name.countSegments fqn
    numLibSegments :: Int
    numLibSegments =
      Name.reverseSegments fqn
        & Foldable.toList
        & List.filter (== NameSegment.libSegment)
        & List.length

-- | Generate a completion tree from a set of names.
-- A completion tree is a suffix tree over the path segments of each name it contains.
-- The goal is to allow fast completion of names by any partial path suffix.
--
-- The tree is generated by building a trie where all possible suffixes of a name are
-- reachable from the root of the trie, with sharing over subtrees to improve memory
-- residency.
--
-- Currently we don't "summarize" all of the children of a node in the node itself, and
-- instead you have to crawl all the children to get the actual completions.
--
-- TODO: Would it be worthwhile to perform compression or include child summaries on the suffix tree?
-- I suspect most namespace trees won't actually compress very well since each node is likely
-- to have terms/types at it.
--
-- E.g. From the names:
-- * alpha.beta.Nat
-- * alpha.Text
-- * foxtrot.Text
--
-- It will generate a tree like the following, where each bullet is a possible completion:
--
-- .
-- ├── foxtrot
-- │   └── Text
-- │       └── * foxtrot.Text (##Text)
-- ├── beta
-- │   └── Nat
-- │       └── * alpha.beta.Nat (##Nat)
-- ├── alpha
-- │   ├── beta
-- │   │   └── Nat
-- │   │       └── * alpha.beta.Nat (##Nat)
-- │   └── Text
-- │       └── * alpha.Text (##Text)
-- ├── Text
-- │   ├── * foxtrot.Text (##Text)
-- │   └── * alpha.Text (##Text)
-- └── Nat
--     └── * alpha.beta.Nat (##Nat)
namesToCompletionTree :: Names -> CompletionTree
namesToCompletionTree Names {terms, types} =
  let typeCompls =
        Relation.domain types
          & ifoldMap
            ( \name refs ->
                refs
                  & Monoid.whenM (not . isDefinitionDoc $ name)
                  & Set.map \ref -> (name, LD.typeRef ref)
            )
      termCompls =
        Relation.domain terms
          & ifoldMap
            ( \name refs ->
                refs
                  & Monoid.whenM (not . isDefinitionDoc $ name)
                  & Set.map \ref -> (name, LD.referent ref)
            )
   in foldMap (uncurry nameToCompletionTree) (typeCompls <> termCompls)
  where
    -- It's  annoying to see _all_ the definition docs in autocomplete so we filter them out.
    -- Special docs like "README" will still appear since they're not named 'doc'
    isDefinitionDoc name =
      case Name.reverseSegments name of
        (doc :| _) -> doc == NameSegment.docSegment

nameToCompletionTree :: Name -> LabeledDependency -> CompletionTree
nameToCompletionTree name ref =
  let (lastSegment :| prefix) = Name.reverseSegments name
      complMap = helper (Map.singleton lastSegment (Set.singleton (name, ref) :< mempty)) prefix
   in CompletionTree (mempty :< complMap)
  where
    -- We build the tree bottom-up rather than top-down so we can take 'share' submaps for
    -- improved memory residency, each  call is passed the submap that we built under the
    -- current reversed path prefix.
    helper ::
      Map
        NameSegment
        (Cofree (Map NameSegment) (Set (Name, LabeledDependency))) ->
      [NameSegment] ->
      Map
        NameSegment
        (Cofree (Map NameSegment) (Set (Name, LabeledDependency)))
    helper subMap revPrefix = case revPrefix of
      [] -> subMap
      (ns : rest) ->
        mergeSubmaps (helper (Map.singleton ns (mempty :< subMap)) rest) subMap
      where
        mergeSubmaps = Map.unionWith (\a b -> unCompletionTree $ CompletionTree a <> CompletionTree b)

-- | Crawl the completion tree and return all valid prefix-based completions alongside their
-- Path from the provided prefix, and their full name.
--
-- E.g. if the term "alpha.beta.gamma.map (#abc)" exists in the completion map, and the query is "beta" the result would
-- be:
--
-- @@
-- [(["beta", "gamma", "map"], "alpha.beta.gamma.map", TermReferent #abc)]
-- @@
matchCompletions :: CompletionTree -> Text -> [(Path, Name, LabeledDependency)]
matchCompletions (CompletionTree tree) txt =
  case Megaparsec.runParser (Name.nameP <* Megaparsec.eof) "" (Text.unpack txt) of
    Left _ -> []
    Right name -> matchSegments (Foldable.toList (Name.segments name)) (Set.toList <$> tree)
  where
    matchSegments :: [NameSegment] -> Cofree (Map NameSegment) [(Name, LabeledDependency)] -> [(Path, Name, LabeledDependency)]
    matchSegments xs (currentMatches :< subtreeMap) =
      case xs of
        [] ->
          let current = currentMatches <&> (\(name, def) -> (mempty, name, def))
           in (current <> mkDefMatches subtreeMap)
        [prefix] ->
          ifoldMap (\ns -> consPathPrefix ns . matchSegments [])
            . Map.takeWhileAntitone (Text.isPrefixOf (NameSegment.toUnescapedText prefix) . NameSegment.toUnescapedText)
            $ Map.dropWhileAntitone (< prefix) subtreeMap
        ns : rest -> consPathPrefix ns . foldMap (matchSegments rest) $ Map.lookup ns subtreeMap
    consPathPrefix :: NameSegment -> [(Path, Name, LabeledDependency)] -> [(Path, Name, LabeledDependency)]
    consPathPrefix ns = over (mapped . _1) (Path.resolve $ Path.singleton ns)
    mkDefMatches :: Map NameSegment (Cofree (Map NameSegment) [(Name, LabeledDependency)]) -> [(Path, Name, LabeledDependency)]
    mkDefMatches xs = do
      (ns, (matches :< rest)) <- Map.toList xs
      let childMatches = mkDefMatches rest <&> over _1 (Path.resolve $ Path.singleton ns)
      let currentMatches = matches <&> \(name, dep) -> (Path.singleton ns, name, dep)
      currentMatches <> childMatches

-- | Called to resolve additional details for a completion item that the user is considering.
completionItemResolveHandler :: Msg.TRequestMessage 'Msg.Method_CompletionItemResolve -> (Either Msg.ResponseError CompletionItem -> Lsp ()) -> Lsp ()
completionItemResolveHandler message respond = do
  let completion :: CompletionItem
      completion = message ^. params
  respond . maybe (Right completion) Right =<< runMaybeT do
    case Aeson.fromJSON <$> (completion ^. data_) of
      Just (Aeson.Success (CompletionItemDetails {dep, fullyQualifiedName, relativeName, fileUri})) -> do
        pped <- lift $ ppedForFile fileUri

        builtinsAsync <- liftIO . UnliftIO.async $ UnliftIO.evaluate IOSource.typecheckedFile
        checkBuiltinsReady <- pure $ maybe False (either (const False) $ const True) <$> UnliftIO.poll builtinsAsync
        renderedDocs <-
          -- We don't want to block the type signature hover info if the docs are taking a long time to render;
          -- We know it's also possible to write docs that eval forever, so the timeout helps
          -- protect against that.
          lift (UnliftIO.timeout 2_000_000 (LSPQ.markdownDocsForFQN fileUri (HQ.NameOnly fullyQualifiedName)))
            >>= ( \case
                    Nothing ->
                      checkBuiltinsReady >>= \case
                        False -> pure ["\n---\n🔜 Doc renderer is initializing, try again in a few seconds."]
                        True -> pure ["\n---\n⏳ Timeout evaluating docs"]
                    Just [] -> pure []
                    -- Add some space from the type signature
                    Just xs@(_ : _) -> pure ("\n---\n" : xs)
                )
        case dep of
          LD.TermReferent ref -> do
            typ <- LSPQ.getTypeOfReferent fileUri ref
            let renderedType = ": " <> (Text.pack $ TypePrinter.prettyStr (Just typeWidth) (PPED.suffixifiedPPE pped) typ)
            let doc = toMarkup (Text.unlines $ ["``` unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
            pure $ (completion {_detail = Just renderedType, _documentation = Just doc} :: CompletionItem)
          LD.TypeReference ref ->
            case ref of
              Reference.Builtin {} -> do
                let renderedBuiltin = ": <builtin>"
                let doc = toMarkup (Text.unlines $ ["``` unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
                pure $ (completion {_detail = Just renderedBuiltin, _documentation = Just doc} :: CompletionItem)
              Reference.DerivedId refId -> do
                decl <- LSPQ.getTypeDeclaration fileUri refId
                let renderedDecl =
                      ": "
                        <> ( Text.pack . Pretty.toPlain typeWidth . Pretty.syntaxToColor $
                               DeclPrinter.prettyDecl
                                 pped
                                 DeclPrinter.RenderUniqueTypeGuids'No
                                 ref
                                 (HQ.NameOnly relativeName)
                                 decl
                           )
                let doc = toMarkup (Text.unlines $ ["``` unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
                pure $ (completion {_detail = Just renderedDecl, _documentation = Just doc} :: CompletionItem)
      _ -> empty
  where
    toMarkup txt = InR $ MarkupContent {_kind = MarkupKind_Markdown, _value = txt}
    -- Completion windows can be very small, so this seems like a good default
    typeWidth = Pretty.Width 20

-- | Data which will be provided back to us in the completion resolve handler when the user considers this completion.
data CompletionItemDetails = CompletionItemDetails
  { dep :: LD.LabeledDependency,
    relativeName :: Name,
    fullyQualifiedName :: Name,
    fileUri :: Uri
  }

instance Aeson.ToJSON CompletionItemDetails where
  toJSON CompletionItemDetails {dep, relativeName, fullyQualifiedName, fileUri} =
    Aeson.object
      [ "relativeName" Aeson..= Name.toText relativeName,
        "fullyQualifiedName" Aeson..= Name.toText fullyQualifiedName,
        "fileUri" Aeson..= fileUri,
        "dep" Aeson..= ldJSON dep
      ]
    where
      ldJSON :: LD.LabeledDependency -> Aeson.Value
      ldJSON = \case
        LD.TypeReference ref -> Aeson.object ["kind" Aeson..= ("type" :: Text), "ref" Aeson..= Reference.toText ref]
        LD.TermReferent ref -> Aeson.object ["kind" Aeson..= ("term" :: Text), "ref" Aeson..= Referent.toText ref]

instance Aeson.FromJSON CompletionItemDetails where
  parseJSON = Aeson.withObject "CompletionItemDetails" \obj -> do
    dep <- ((obj Aeson..: "dep") >>= ldParser)
    relativeName <- (obj Aeson..: "relativeName" >>= maybe (fail "Invalid name in CompletionItemDetails") pure . Name.parseText)
    fullyQualifiedName <- (obj Aeson..: "fullyQualifiedName" >>= maybe (fail "Invalid name in CompletionItemDetails") pure . Name.parseText)
    fileUri <- obj Aeson..: "fileUri"
    pure $ CompletionItemDetails {..}
    where
      ldParser :: Aeson.Value -> Aeson.Parser LD.LabeledDependency
      ldParser = Aeson.withObject "LabeledDependency" \obj -> do
        kind <- obj Aeson..: "kind"
        case kind of
          ("type" :: Text) -> LD.TypeReference <$> (obj Aeson..: "ref" >>= either (const $ fail "Invalid Reference in LabeledDependency") pure . Reference.fromText)
          ("term" :: Text) -> LD.TermReferent <$> (obj Aeson..: "ref" >>= maybe (fail "Invalid Referent in LabeledDependency") pure . Referent.fromText)
          _ -> fail "Invalid LabeledDependency kind"
