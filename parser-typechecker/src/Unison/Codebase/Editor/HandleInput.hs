{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- todo: delete
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- todo: delete
{-# OPTIONS_GHC -Wno-unused-local-binds #-} -- todo: delete
{-# OPTIONS_GHC -Wno-unused-matches #-} -- todo: delete

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}

module Unison.Codebase.Editor.HandleInput (loop, loopState0, LoopState(..), parseSearchType) where

import Unison.Prelude

import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.DisplayThing
import qualified Unison.Codebase.Editor.DisplayThing as DT
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.RemoteRepo

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad.State            ( StateT
                                                )
import           Control.Monad.Trans.Except     ( ExceptT(..), runExceptT)
import           Data.Bifunctor                 ( second )
import           Data.Configurator.Types        ( Config )
import           Data.Configurator              ()
import qualified Data.Graph as Graph
import qualified Data.List                      as List
import           Data.List                      ( partition, sortOn )
import           Data.List.Extra                (nubOrd, intercalate, sort)
import           Data.Maybe                     ( fromJust
                                                )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq(..) )
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.Metadata      as Metadata
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Path           ( Path
                                                , Path'(..) )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.NameSegment   as NameSegment
import qualified Unison.Codebase.Reflog        as Reflog
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Hash                   as Hash
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name(Name) )
import           Unison.Names3                  ( Names(..), Names0
                                                , pattern Names0 )
import qualified Unison.Names2                 as Names
import qualified Unison.Names3                 as Names3
import qualified Unison.Parsers                as Parsers
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Result                  ( pattern Result )
import qualified Unison.ShortHash as SH
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Util.List               ( uniqueBy )
import qualified Unison.Util.Relation          as R
import           Unison.Util.TransitiveClosure  (transitiveClosure)
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Codebase.TermEdit (TermEdit(..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.Typechecker as Typechecker
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Runtime.IOSource       ( isTest, ioReference )
import qualified Unison.Runtime.IOSource as IOSource
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Pretty            as P
import           Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Monoid            as Monoid
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.ConstructorType (ConstructorType)
import qualified Unison.Lexer as L
import Unison.Codebase.Editor.SearchResult' (SearchResult')
import qualified Unison.Codebase.Editor.SearchResult' as SR'
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)
import Unison.Type (Type)
import qualified Unison.Builtin as Builtin
import Unison.Codebase.NameSegment (NameSegment(..))
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Editor.Propagate as Propagate
import qualified Unison.CommandLine.DisplayValues as DisplayValues
import qualified Control.Error.Util as ErrorUtil

type F m i v = Free (Command m i v)
type Term v a = Term.AnnotatedTerm v a

-- type (Action m i v) a
type Action m i v = MaybeT (StateT (LoopState m v) (F m i v))

_liftToAction :: m a -> Action m i v a
_liftToAction = lift . lift . Free.eval . Eval

data LoopState m v
  = LoopState
      { _root :: Branch m
      -- the current position in the namespace
      , _currentPath :: Path.Absolute

      -- TBD
      -- , _activeEdits :: Set Branch.EditGuid

      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann)

      -- The previous user input. Used to request confirmation of
      -- questionable user commands.
      , _lastInput :: Maybe Input

      -- A 1-indexed list of strings that can be referenced by index at the
      -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
      -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
      , _numberedArgs :: [String]
      }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch m -> Path.Absolute -> LoopState m v
loopState0 b p = LoopState b p Nothing Nothing Nothing []

type Action' m v = Action m (Either Event Input) v

defaultPatchNameSegment :: NameSegment
defaultPatchNameSegment = "patch"

loop :: forall m v . (Monad m, Var v) => Action m (Either Event Input) v ()
loop = do
  uf           <- use latestTypecheckedFile
  root'        <- use root
  currentPath' <- use currentPath
  latestFile'  <- use latestFile
  currentBranch' <- getAt currentPath'
  e           <- eval Input
  hqLength    <- eval CodebaseHashLength
  sbhLength   <- eval BranchHashLength
  let
      sbh = SBH.fromHash sbhLength
      root0 = Branch.head root'
      currentBranch0 = Branch.head currentBranch'
      defaultPatchPath :: PatchPath
      defaultPatchPath = (Path' $ Left currentPath', defaultPatchNameSegment)
      resolveSplit' :: (Path', a) -> (Path, a)
      resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      resolveToAbsolute :: Path' -> Path.Absolute
      resolveToAbsolute = Path.toAbsolutePath currentPath'
      getAtSplit :: Path.Split -> Maybe (Branch m)
      getAtSplit p = BranchUtil.getBranch p root0
      getAtSplit' :: Path.Split' -> Maybe (Branch m)
      getAtSplit' = getAtSplit . resolveSplit'
      getPatchAtSplit' :: Path.Split' -> Action' m v (Maybe Patch)
      getPatchAtSplit' s = do
        let (p, seg) = Path.toAbsoluteSplit currentPath' s
        b <- getAt p
        eval . Eval $ Branch.getMaybePatch seg (Branch.head b)
      getHQ'TermsIncludingHistorical p =
        getTermsIncludingHistorical (resolveSplit' p) root0
      getHQ'Terms p = BranchUtil.getTerm (resolveSplit' p) root0
      getHQ'Types p = BranchUtil.getType (resolveSplit' p) root0
      getTypes :: Path.Split' -> Set Reference
      getTypes = getHQ'Types . fmap HQ'.NameOnly
      getTerms :: Path.Split' -> Set Referent
      getTerms = getHQ'Terms . fmap HQ'.NameOnly
      getPatchAt :: Path.Split' -> Action' m v Patch
      getPatchAt patchPath' = do
        let (p, seg) = Path.toAbsoluteSplit currentPath' patchPath'
        b <- getAt p
        eval . Eval $ Branch.getPatch seg (Branch.head b)
      withFile ambient sourceName lexed@(text, tokens) k = do
        let
          getHQ = \case
            L.Backticks s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.WordyId s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.SymbolyId s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.Hash sh -> Just (HQ.HashOnly sh)
            _         -> Nothing
          hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
        parseNames :: Names <- makeHistoricalParsingNames hqs
        Result notes r <- eval $ Typecheck ambient parseNames sourceName lexed
        case r of
          -- Parsing failed
          Nothing -> respond $
            ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (Left errNames) -> do
            ppe <- prettyPrintEnv =<< makeShadowedPrintNamesFromHQ hqs errNames
            respond $
              TypeErrors text ppe [ err | Result.TypeError err <- toList notes ]
          Just (Right uf) -> k uf

  case e of
    Left (IncomingRootBranch hashes) ->
      respond . WarnIncomingRootBranch $ Set.map (SBH.fromHash sbhLength) hashes
    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if maybe False snd latestFile'
        then modifying latestFile (fmap (const False) <$>)
        else do
          let lexed = L.lexer (Text.unpack sourceName) (Text.unpack text)
          withFile [] sourceName (text, lexed) $ \unisonFile -> do
            sr <- toSlurpResult unisonFile <$> slurpResultNames0
            hnames <- makeShadowedPrintNamesFromLabeled
                        (UF.termSignatureExternalLabeledDependencies unisonFile)
                        (UF.typecheckedToNames0 unisonFile)
            ppe <- prettyPrintEnv hnames
            eval (Notify $ Typechecked sourceName ppe sr unisonFile)
            r <- eval . Evaluate ppe $ unisonFile
            case r of
              Left e -> respond $ EvaluationFailure e
              Right (bindings, e) -> do
                let e' = Map.map go e
                    go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
                when (not $ null e') $
                  eval . Notify $ Evaluated text ppe bindings e'
                latestFile .= Just (Text.unpack sourceName, False)
                latestTypecheckedFile .= Just unisonFile
    Right input ->
      let
        ifConfirmed = ifM (confirmedCommand input)
        branchNotFound = respond . BranchNotFound input
        branchNotFound' = respond . BranchNotFound input . Path.unsplit'
        patchNotFound :: Path.Split' -> _
        patchNotFound s = respond $ PatchNotFound input s
        patchExists :: Path.Split' -> _
        patchExists s = respond $ PatchAlreadyExists input s
        typeNotFound = respond . TypeNotFound input
        termNotFound = respond . TermNotFound input
        typeConflicted src = respond . TypeAmbiguous input src
        termConflicted src = respond . TermAmbiguous input (Left src)
        hashConflicted src = respond . HashAmbiguous input src
        branchExists dest _x = respond $ BranchAlreadyExists input dest
        branchExistsSplit = branchExists . Path.unsplit'
        typeExists dest = respond . TypeAlreadyExists input dest
        termExists dest = respond . TermAlreadyExists input dest
        inputDescription :: Text
        inputDescription = case input of
          ForkLocalBranchI src dest -> "fork " <> hp' src <> " " <> p' dest
          MergeLocalBranchI src dest -> "merge " <> p' src <> " " <> p' dest
          ResetRootI src -> "reset-root " <> hp' src
          AliasTermI src dest -> "alias.term " <> hqs' src <> " " <> ps' dest
          AliasTypeI src dest -> "alias.type" <> hqs' src <> " " <> ps' dest
          MoveTermI src dest -> "move.term " <> hqs' src <> " " <> ps' dest
          MoveTypeI src dest -> "move.type " <> hqs' src <> " " <> ps' dest
          MoveBranchI src dest -> "move.namespace " <> ops' src <> " " <> ps' dest
          MovePatchI src dest -> "move.patch " <> ps' src <> " " <> ps' dest
          CopyPatchI src dest -> "copy.patch " <> ps' src <> " " <> ps' dest
          DeleteTermI def -> "delete.term " <> hqs' def
          DeleteTypeI def -> "delete.type" <> hqs' def
          DeleteBranchI opath -> "delete.namespace " <> ops' opath
          DeletePatchI path -> "delete.patch " <> ps' path
          ResolveTermI srcH targetH p ->
            "resolve.term " <> SH.toText srcH <> " "
                            <> SH.toText targetH <> " "
                            <> opatch p
          ResolveTypeI srcH targetH p ->
            "resolve.type " <> SH.toText srcH <> " "
                            <> SH.toText targetH <> " "
                            <> opatch p
          ResolveTermNameI path -> "resolve.termName " <> hqs' path
          ResolveTypeNameI path -> "resolve.typeName " <> hqs' path
          AddI _selection -> "add"
          UpdateI p _selection -> "update " <> opatch p
          PropagatePatchI p scope -> "patch " <> ps' p <> " " <> p' scope
          UndoI{} -> "undo"
          ExecuteI s -> "execute " <> Text.pack s
          LinkI from to -> "link " <> hqs' from <> " " <> hqs' to
          UnlinkI from to -> "unlink " <> hqs' from <> " " <> hqs' to
          UpdateBuiltinsI -> "builtins.update"
          MergeBuiltinsI -> "builtins.merge"
          PullRemoteBranchI orepo dest -> "pull " <> Text.pack (show orepo) <> " " <> p' dest
          PushRemoteBranchI{} -> wat
          PreviewMergeLocalBranchI{} -> wat
          SwitchBranchI{} -> wat
          NamesI{} -> wat
          TodoI{} -> wat
          ListEditsI{} -> wat
          HistoryI{} -> wat
          TestI{} -> wat
          LinksI{} -> wat
          SearchByNameI{} -> wat
          FindShallowI{} -> wat
          FindPatchI{} -> wat
          ShowDefinitionI{} -> wat
          DisplayI{} -> wat
          DocsI{} -> wat
          ShowDefinitionByPrefixI{} -> wat
          ShowReflogI{} -> wat
          DebugBranchHistoryI{} -> wat
          QuitI{} -> wat
          DeprecateTermI{} -> undefined
          DeprecateTypeI{} -> undefined
          AddTermReplacementI{} -> undefined
          AddTypeReplacementI{} -> undefined
          RemoveTermReplacementI{} -> undefined
          RemoveTypeReplacementI{} -> undefined
          where
          hp' = either (Text.pack . show) p'
          p' = Text.pack . show . Path.toAbsolutePath currentPath'
          ops' = maybe "." ps'
          opatch = ps' . fromMaybe defaultPatchPath
          wat = error $ show input ++ " is not expected to alter the branch"
          hqs' (p, hq) =
            Monoid.unlessM (Path.isRoot' p) (p' p) <> "." <> Text.pack (show hq)
          ps' = p' . Path.unsplit'
        stepAt = Unison.Codebase.Editor.HandleInput.stepAt inputDescription
        stepManyAt = Unison.Codebase.Editor.HandleInput.stepManyAt inputDescription
        stepManyAtM = Unison.Codebase.Editor.HandleInput.stepManyAtM inputDescription
        updateAtM = Unison.Codebase.Editor.HandleInput.updateAtM inputDescription
      in case input of
      ShowReflogI -> do
        entries <- fmap (convertEntries Nothing []) $ eval LoadReflog
        numberedArgs .=
          fmap (('#':) . SBH.toString . Output.hash) entries
        respond $ ShowReflog entries
        where
        -- reverses & formats entries, adds synthetic entries when there is a
        -- discontinuity in the reflog.
        convertEntries :: Maybe Branch.Hash
                       -> [Output.ReflogEntry]
                       -> [Reflog.Entry]
                       -> [Output.ReflogEntry]
        convertEntries _ acc [] = acc
        convertEntries Nothing acc entries@(Reflog.Entry old _ _ : _) =
          convertEntries
            (Just old)
            (Output.ReflogEntry (SBH.fromHash sbhLength old) "(initial reflogged namespace)" : acc)
            entries
        convertEntries (Just lastHash) acc entries@(Reflog.Entry old new reason : rest) =
          if lastHash /= old then
            convertEntries
              (Just old)
              (Output.ReflogEntry (SBH.fromHash sbhLength old) "(external change)" : acc)
              entries
          else
            convertEntries
              (Just new)
              (Output.ReflogEntry (SBH.fromHash sbhLength new) reason : acc)
              rest

      ResetRootI src0 ->
        case src0 of
          Left hash -> resolveShortBranchHash input hash >>= \case
            Left output -> respond output
            Right newRoot -> do
              updateRoot root' newRoot inputDescription
              success
          Right path' -> do
            newRoot <- getAt $ Path.toAbsolutePath currentPath' path'
            if Branch.isEmpty newRoot then respond $ BranchNotFound input path'
            else do
              updateRoot root' newRoot inputDescription
              success
      ForkLocalBranchI src0 dest0 -> do
        let tryUpdateDest srcb dest0 = do
              let dest = Path.toAbsolutePath currentPath' dest0
              -- if dest isn't empty: leave dest unchanged, and complain.
              ok <- updateAtM dest $ \destb ->
                pure (if Branch.isEmpty destb then srcb else destb)
              if ok then success else respond $ BadDestinationBranch input dest0
        case src0 of
          Left hash -> resolveShortBranchHash input hash >>= \case
            Left output -> respond output
            Right srcb -> tryUpdateDest srcb dest0
          Right path' -> do
            srcb <- getAt $ Path.toAbsolutePath currentPath' path'
            if Branch.isEmpty srcb then respond $ BranchNotFound input path'
            else tryUpdateDest srcb dest0
      MergeLocalBranchI src0 dest0 -> do
        let [src, dest] = Path.toAbsolutePath currentPath' <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          destb <- getAt dest
          merged <- eval . Eval $ Branch.merge srcb destb
          b <- updateAtM dest $ const (pure merged)
          if b then do
            respond (ShowDiff input (Branch.namesDiff destb merged))
            patch <- getPatchAt defaultPatchPath
            void $ propagatePatch inputDescription patch dest
          else respond (NothingTodo input)

      PreviewMergeLocalBranchI src0 dest0 -> do
        let [src, dest] = Path.toAbsolutePath currentPath' <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          destb <- getAt dest
          merged <- eval . Eval $ Branch.merge srcb destb
          if merged == destb then respond (NothingTodo input)
          else respond $ ShowDiff input (Branch.namesDiff destb merged)

      -- move the root to a sub-branch
      MoveBranchI Nothing dest -> do
        b <- use root
        stepManyAt [ (Path.empty, const Branch.empty0)
                   , BranchUtil.makeSetBranch (resolveSplit' dest) b ]
        success

      MoveBranchI (Just src) dest ->
        maybe (branchNotFound' src) srcOk (getAtSplit' src)
        where
        srcOk b = maybe (destOk b) (branchExistsSplit dest) (getAtSplit' dest)
        destOk b = do
          stepManyAt
            [ BranchUtil.makeSetBranch (resolveSplit' src) Branch.empty
            , BranchUtil.makeSetBranch (resolveSplit' dest) b ]
          success -- could give rando stats about new defns

      MovePatchI src dest -> do
        psrc <- getPatchAtSplit' src
        pdest <- getPatchAtSplit' dest
        case (psrc, pdest) of
          (Nothing, _) -> patchNotFound src
          (_, Just _) -> patchExists dest
          (Just p, Nothing) -> do
            stepManyAt [
              BranchUtil.makeDeletePatch (resolveSplit' src),
              BranchUtil.makeReplacePatch (resolveSplit' dest) p ]
            success

      CopyPatchI src dest -> do
        psrc <- getPatchAtSplit' src
        pdest <- getPatchAtSplit' dest
        case (psrc, pdest) of
          (Nothing, _) -> patchNotFound src
          (_, Just _) -> patchExists dest
          (Just p, Nothing) -> do
            stepAt (BranchUtil.makeReplacePatch (resolveSplit' dest) p)
            success

      DeletePatchI src -> do
        psrc <- getPatchAtSplit' src
        case psrc of
          Nothing -> patchNotFound src
          Just _ -> do
            stepAt (BranchUtil.makeDeletePatch (resolveSplit' src))
            success

      DeleteBranchI Nothing ->
        ifConfirmed
            (do
              stepAt (Path.empty, const Branch.empty0)
              respond DeletedEverything)
            (respond DeleteEverythingConfirmation)

      DeleteBranchI (Just p) ->
        maybe (branchNotFound' p) go $ getAtSplit' p
        where
        go (Branch.head -> b) = do
          (failed, failedDependents) <-
            let rootNames = Branch.toNames0 root0
                toDelete = Names.prefix0
                  (Path.toName . Path.unsplit . resolveSplit' $ p)
                  (Branch.toNames0 b)
            in getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then do
            stepAt $ BranchUtil.makeSetBranch (resolveSplit' p) Branch.empty
            -- Looks similar to the 'toDelete' above... investigate me! ;)
            let deletedNames =
                  Names.prefix0
                    (Path.toName' (Path.unsplit' p))
                    (Branch.toNames0 b)
                diff = Names3.diff0 deletedNames mempty
            respond $ ShowDiff input diff
          else do
            failed <- loadSearchResults $ Names.asSearchResults failed
            failedDependents <- loadSearchResults $ Names.asSearchResults failedDependents
            ppe <- prettyPrintEnv =<<
              makePrintNamesFromLabeled'
                (foldMap SR'.labeledDependencies $ failed <> failedDependents)
            respond $ CantDelete input ppe failed failedDependents

      SwitchBranchI path' -> do
        path <- use $ currentPath . to (`Path.toAbsolutePath` path')
        currentPath .= path
        branch' <- getAt path
        when (Branch.isEmpty branch') (respond $ CreatedNewBranch path)

      HistoryI resultsCap diffCap from -> case from of
        Left hash -> resolveShortBranchHash input hash >>= \case
          Left output -> respond output
          Right b -> do
            hashLen <- eval BranchHashLength
            doHistory 0 b []
        Right path' -> do
          path <- use $ currentPath . to (`Path.toAbsolutePath` path')
          branch' <- getAt path
          if Branch.isEmpty branch' then respond $ CreatedNewBranch path
          else do
            hashLen <- eval BranchHashLength
            doHistory 0 branch' []
        where
          doHistory !n b acc =
            if maybe False (n >=) resultsCap then
              respond $ History diffCap acc (PageEnd (sbh $ Branch.headHash b) n)
            else case Branch._history b of
              Causal.One{} ->
                respond $ History diffCap acc (EndOfLog . sbh $ Branch.headHash b)
              Causal.Merge{..} ->
                respond $ History diffCap acc (MergeTail (sbh $ Branch.headHash b) . map sbh $ Map.keys tails)
              Causal.Cons{..} -> do
                b' <- fmap Branch.Branch . eval . Eval $ snd tail
                let elem = (sbh $ Branch.headHash b, Branch.namesDiff b' b)
                doHistory (n+1) b' (elem : acc)

      UndoI -> do
        prev <- eval . Eval $ Branch.uncons root'
        case prev of
          Nothing ->
            respond . CantUndo $ if Branch.isOne root' then CantUndoPastStart
                                 else CantUndoPastMerge
          Just (_, prev) -> do
            updateRoot root' prev inputDescription
            respond $ ShowDiff input (Branch.namesDiff prev root')

      AliasTermI src dest -> case (toList (getHQ'Terms src), toList (getTerms dest)) of
        ([r],       []) -> do
          stepAt (BranchUtil.makeAddTermName (resolveSplit' dest) r (oldMD r))
          success
        ([_], rs@(_:_)) -> termExists dest (Set.fromList rs)
        ([],         _) -> termNotFound src
        (rs,         _) -> termConflicted src (Set.fromList rs)
        where
        p = resolveSplit' src
        oldMD r = BranchUtil.getTermMetadataAt p r root0

      AliasTypeI src dest -> case (toList (getHQ'Types src), toList (getTypes dest)) of
        ([r],       []) -> do
          stepAt (BranchUtil.makeAddTypeName (resolveSplit' dest) r (oldMD r))
          success
        ([_], rs@(_:_)) -> typeExists dest (Set.fromList rs)
        ([],         _) -> typeNotFound src
        (rs,         _) -> typeConflicted src (Set.fromList rs)
        where
        p = resolveSplit' src
        oldMD r = BranchUtil.getTypeMetadataAt p r root0

      NamesI thing -> do
        len <- eval CodebaseHashLength
        parseNames0 <- basicParseNames0
        let filtered = case thing of
              HQ.HashOnly shortHash ->
                Names.filterBySHs (Set.singleton shortHash) parseNames0
              HQ.HashQualified n sh ->
                Names.filterByHQs (Set.singleton $ HQ'.HashQualified n sh) parseNames0
              HQ.NameOnly n ->
                Names.filterByHQs (Set.singleton $ HQ'.NameOnly n) parseNames0
        printNames0 <- basicPrettyPrintNames0
        let printNames = Names printNames0 mempty
        let terms' ::Set (Referent, Set HQ'.HashQualified)
            terms' = (`Set.map` Names.termReferents filtered) $
                        \r -> (r, Names3.termName len r printNames)
            types' :: Set (Reference, Set HQ'.HashQualified)
            types' = (`Set.map` Names.typeReferences filtered) $
                        \r -> (r, Names3.typeName len r printNames)
        respond $ ListNames (toList terms') (toList types')
--          let (p, hq) = p0
--              namePortion = HQ'.toName hq
--          case hq of
--            HQ'.NameOnly _ ->
--              respond $ uncurry ListNames (results p namePortion)
--            HQ'.HashQualified _ sh -> let
--              (terms, types) = results p namePortion
--              -- filter terms and types based on `sh : ShortHash`
--              terms' = filter (Reference.isPrefixOf sh . Referent.toReference . fst) terms
--              types' = filter (Reference.isPrefixOf sh . fst) types
--              in respond $ ListNames terms' types'
--          where
--            results p namePortion = let
--              name = Path.toName . Path.unprefix currentPath' . Path.snoc' p
--                   $ namePortion
--              ns = prettyPrintNames0
--              terms = [ (r, Names.namesForReferent ns r)
--                      | r <- toList $ Names.termsNamed ns name ]
--              types = [ (r, Names.namesForReference ns r)
--                      | r <- toList $ Names.typesNamed ns name ]
--              in (terms, types)

      LinkI src mdValue -> do
        let srcle = toList (getHQ'Terms src)
            srclt = toList (getHQ'Types src)
            (parent, _last) = resolveSplit' src
            mdValuel = toList (getHQ'Terms mdValue)
        case (srcle, srclt, mdValuel) of
          (srcle, srclt, [Referent.Ref mdValue])
            | length srcle < 2 && length srclt < 2 -> do
              mdType <- eval $ LoadTypeOfTerm mdValue
              case mdType of
                Nothing -> respond $ LinkFailure input
                Just ty -> do
                  stepAt (parent, step (Type.toReference ty))
                  success
                where
                step mdType b0 = let
                  tmUpdates terms = foldl' go terms srcle where
                    go terms src = Metadata.insert (src, mdType, mdValue) terms
                  tyUpdates types = foldl' go types srclt where
                    go types src = Metadata.insert (src, mdType, mdValue) types
                  in over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
          _ -> respond $ LinkFailure input

      UnlinkI src mdValue -> do
        let srcle = toList (getHQ'Terms src)
            srclt = toList (getHQ'Types src)
            (parent, _last) = resolveSplit' src
            mdValuel = toList (getHQ'Terms mdValue)
        case (srcle, srclt, mdValuel) of
          (srcle, srclt, [Referent.Ref mdValue])
            | length srcle < 2 && length srclt < 2 -> do
              mdType <- eval $ LoadTypeOfTerm mdValue
              case mdType of
                Nothing -> respond $ LinkFailure input
                Just ty -> do
                  stepAt (parent, step (Type.toReference ty))
                  success
                where
                step mdType b0 = let
                  tmUpdates terms = foldl' go terms srcle where
                    go terms src = Metadata.delete (src, mdType, mdValue) terms
                  tyUpdates types = foldl' go types srclt where
                    go types src = Metadata.delete (src, mdType, mdValue) types
                  in over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
          _ -> respond $ LinkFailure input

      -- > links List.map (.Docs .English)
      -- > links List.map -- give me all the
      -- > links Optional License
      LinksI src mdTypeStr -> getLinks input src (Right mdTypeStr) >>= \case
        Left e -> respond e
        Right (ppe, out) -> do
          numberedArgs .= fmap (HQ.toString . view _1) out
          respond $ ListOfLinks ppe out

      DocsI src -> getLinks input src (Left $ Set.singleton DD.docRef) >>= \case
        Left e -> respond e
        Right (ppe, out) -> case out of
          [(name, ref, tm)] -> do
            names <- basicPrettyPrintNames0
            doDisplay ConsoleLocation (Names3.Names names mempty) (Referent.Ref ref)
          out -> do
            numberedArgs .= fmap (HQ.toString . view _1) out
            respond $ ListOfLinks ppe out

      MoveTermI src dest ->
        case (toList (getHQ'Terms src), toList (getTerms dest)) of
          ([r], []) -> do
            stepManyAt
              [ BranchUtil.makeDeleteTermName p r
              , BranchUtil.makeAddTermName (resolveSplit' dest) r (mdSrc r)]
            success
          ([_], rs) -> termExists dest (Set.fromList rs)
          ([],   _) -> termNotFound src
          (rs,   _) -> termConflicted src (Set.fromList rs)
        where p = resolveSplit' (HQ'.toName <$> src)
              mdSrc r = BranchUtil.getTermMetadataAt p r root0

      MoveTypeI src dest ->
        case (toList (getHQ'Types src), toList (getTypes dest)) of
          ([r], []) -> do
            stepManyAt
              [ BranchUtil.makeDeleteTypeName p r
              , BranchUtil.makeAddTypeName (resolveSplit' dest) r (mdSrc r) ]
            success
          ([_], rs) -> typeExists dest (Set.fromList rs)
          ([], _)   -> typeNotFound src
          (rs, _)   -> typeConflicted src (Set.fromList rs)
        where
        p = resolveSplit' (HQ'.toName <$> src)
        mdSrc r = BranchUtil.getTypeMetadataAt p r root0

      DeleteTypeI hq -> case toList (getHQ'Types hq) of
        [] -> typeNotFound hq
        [r] -> goMany (Set.singleton r)
        (Set.fromList -> rs) -> ifConfirmed (goMany rs) (typeConflicted hq rs)
        where
        resolvedPath = resolveSplit' (HQ'.toName <$> hq)
        makeDelete = BranchUtil.makeDeleteTypeName resolvedPath
        goMany rs = do
          let rootNames = Branch.toNames0 root0
              -- these names are relative to the root
              toDelete = Names0 mempty (R.fromList . fmap (name,) $ toList rs)
                where name = Path.toName . Path.unsplit $ resolvedPath
          (failed, failedDependents) <-
            getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <-
              loadSearchResults $ Names.asSearchResults failed
            failedDependents <-
              loadSearchResults $ Names.asSearchResults failedDependents
            ppe <- prettyPrintEnv =<<
              makePrintNamesFromLabeled'
                (foldMap SR'.labeledDependencies $ failed <> failedDependents)
            respond $ CantDelete input ppe failed failedDependents

      -- like the previous
      DeleteTermI hq -> case toList (getHQ'Terms hq) of
        [] -> termNotFound hq
        [r] -> goMany (Set.singleton r)
        (Set.fromList -> rs) -> ifConfirmed (goMany rs) (termConflicted hq rs)
        where
        resolvedPath = resolveSplit' (HQ'.toName <$> hq)
        makeDelete = BranchUtil.makeDeleteTermName resolvedPath
        goMany rs = do
          let rootNames = Branch.toNames0 root0
              -- these names are relative to the root
              toDelete = Names0 (R.fromList . fmap (name,) $ toList rs) mempty
                where name = Path.toName . Path.unsplit $ resolvedPath
          (failed, failedDependents) <-
            getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <-
              loadSearchResults $ Names.asSearchResults failed
            failedDependents <-
              loadSearchResults $ Names.asSearchResults failedDependents
            ppe <- prettyPrintEnv =<<
              makePrintNamesFromLabeled'
                (foldMap SR'.labeledDependencies $ failed <> failedDependents)
            respond $ CantDelete input ppe failed failedDependents

      DisplayI outputLoc s@(HQ.unsafeFromString -> hq) -> do
        parseNames <- (`Names3.Names` mempty) <$> basicPrettyPrintNames0
        let results = Names3.lookupHQTerm hq parseNames
        if Set.null results then
          respond $ SearchTermsNotFound [hq]
        else if Set.size results > 1 then
          respond $ TermAmbiguous input (Right hq) results
        else doDisplay outputLoc parseNames (Set.findMin results)

      ShowDefinitionI outputLoc (fmap HQ.unsafeFromString -> hqs) -> do
        parseNames <- makeHistoricalParsingNames $ Set.fromList hqs
        let resultss = searchBranchExact hqLength parseNames hqs
            (misses, hits) = partition (\(_, results) -> null results) (zip hqs resultss)
            results = List.sort . uniqueBy SR.toReferent $ hits >>= snd
        results' <- loadSearchResults results
        let termTypes :: Map.Map Reference (Type v Ann)
            termTypes =
              Map.fromList
                [ (r, t) | SR'.Tm _ (Just t) (Referent.Ref r) _ <- results' ]
            (collatedTypes, collatedTerms) = collateReferences
              (mapMaybe SR'.tpReference results')
              (mapMaybe SR'.tmReferent results')
        -- load the `collatedTerms` and types into a Map Reference.Id Term/Type
        -- for later
        loadedDerivedTerms <-
          fmap (Map.fromList . catMaybes) . for (toList collatedTerms) $ \case
            Reference.DerivedId i -> fmap (i,) <$> eval (LoadTerm i)
            _ -> pure Nothing
        loadedDerivedTypes <-
          fmap (Map.fromList . catMaybes) . for (toList collatedTypes) $ \case
            Reference.DerivedId i -> fmap (i,) <$> eval (LoadType i)
            _ -> pure Nothing
        -- Populate DisplayThings for the search results, in anticipation of
        -- displaying the definitions.
        loadedDisplayTerms :: Map Reference (DisplayThing (Term v Ann)) <-
         fmap Map.fromList . for (toList collatedTerms) $ \case
          r@(Reference.DerivedId i) -> do
            let tm = Map.lookup i loadedDerivedTerms
            -- We add a type annotation to the term using if it doesn't
            -- already have one that the user provided
            pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
              Nothing        -> MissingThing i
              Just (tm, typ) -> case tm of
                Term.Ann' _ _ -> RegularThing tm
                _ -> RegularThing (Term.ann (ABT.annotation tm) tm typ)
          r@(Reference.Builtin _) -> pure (r, BuiltinThing)
        let loadedDisplayTypes :: Map Reference (DisplayThing (DD.Decl v Ann))
            loadedDisplayTypes =
              Map.fromList . (`fmap` toList collatedTypes) $ \case
                r@(Reference.DerivedId i) ->
                  (r,) . maybe (MissingThing i) RegularThing
                       $ Map.lookup i loadedDerivedTypes
                r@(Reference.Builtin _) -> (r, BuiltinThing)
        -- the SR' deps include the result term/type names, and the
        let deps = foldMap SR'.labeledDependencies results'
                <> foldMap Term.labeledDependencies loadedDerivedTerms
        printNames <- makePrintNamesFromLabeled' deps

        -- We might like to make sure that the user search terms get used as
        -- the names in the pretty-printer, but the current implementation
        -- doesn't.
        ppe <- prettyPrintEnv printNames
        let loc = case outputLoc of
              ConsoleLocation    -> Nothing
              FileLocation path  -> Just path
              LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
        do
          when (not $ null loadedDisplayTypes && null loadedDisplayTerms) $
            eval . Notify $
              DisplayDefinitions loc ppe loadedDisplayTypes loadedDisplayTerms
          when (not $ null misses) $
            eval . Notify . SearchTermsNotFound $ fmap fst misses
          -- We set latestFile to be programmatically generated, if we
          -- are viewing these definitions to a file - this will skip the
          -- next update for that file (which will happen immediately)
          latestFile .= ((, True) <$> loc)

      FindPatchI -> do
        let patches =
              [ Path.toName $ Path.snoc p seg
              | (p, b) <- Branch.toList0 currentBranch0
              , (seg, _) <- Map.toList (Branch._edits b) ]
        respond $ ListOfPatches $ Set.fromList patches
        numberedArgs .= fmap Name.toString patches

      FindShallowI pathArg -> do
        prettyPrintNames0 <- basicPrettyPrintNames0
        ppe <- prettyPrintEnv $ Names prettyPrintNames0 mempty
        hashLen <- eval CodebaseHashLength
        let pathArgAbs = Path.toAbsolutePath currentPath' pathArg
        b0 <- Branch.head <$> getAt pathArgAbs
        let
          hqTerm b0 ns r =
            let refs = Star3.lookupD1 ns . _terms $ b0
            in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLen $ HQ'.fromNamedReferent ns r
          hqType b0 ns r =
            let refs = Star3.lookupD1 ns . _types $ b0
            in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLen $ HQ'.fromNamedReference ns r
          defnCount b =
            (length . R.ran . Star3.d1 . deepTerms $ Branch.head b) +
            (length . R.ran . Star3.d1 . deepTypes $ Branch.head b)
          patchCount b = (length . deepEdits $ Branch.head b)

        termEntries <- for (R.toList . Star3.d1 $ _terms b0) $
          \(r, ns) -> do
            ot <- loadReferentType r
            pure $ ShallowTermEntry r (hqTerm b0 ns r) ot
        let
          typeEntries =
            [ ShallowTypeEntry r (hqType b0 ns r)
            | (r, ns) <- R.toList . Star3.d1 $ _types b0 ]
          branchEntries =
            [ ShallowBranchEntry ns (defnCount b)
            | (ns, b) <- Map.toList $ _children b0 ]
          patchEntries =
            [ ShallowPatchEntry ns
            | (ns, (_h, mp)) <- Map.toList $ _edits b0 ]
        let
          entries :: [ShallowListEntry v Ann]
          entries = sort $ termEntries ++ typeEntries ++ branchEntries
          entryToHQString :: ShallowListEntry v Ann -> String
          -- caching the result as an absolute path, for easier jumping around
          entryToHQString e = fixup $ case e of
            ShallowTypeEntry _ hq   -> HQ'.toString hq
            ShallowTermEntry _ hq _ -> HQ'.toString hq
            ShallowBranchEntry ns _ -> NameSegment.toString ns
            ShallowPatchEntry ns    -> NameSegment.toString ns
            where
            fixup s =
              if last pathArgStr == '.'
              then pathArgStr ++ s
              else pathArgStr ++ "." ++ s
            pathArgStr = show pathArgAbs
        numberedArgs .= fmap entryToHQString entries
        respond $ ListShallow ppe entries
        where

      SearchByNameI isVerbose _showAll ws -> do
        prettyPrintNames0 <- basicPrettyPrintNames0
        -- results became an Either to accommodate `parseSearchType` returning an error
        results <- runExceptT $ case ws of
          -- no query, list everything
          [] -> pure . listBranch $ Branch.head currentBranch'

          -- type query
          ":" : ws -> ExceptT (parseSearchType input (unwords ws)) >>= \typ -> ExceptT $ do
            let named = Branch.deepReferents (Branch.head root')
            matches <- fmap toList . eval $ GetTermsOfType typ
            matches <- filter (`Set.member` named) <$>
              if null matches then do
                respond NoExactTypeMatches
                fmap toList . eval $ GetTermsMentioningType typ
              else pure matches
            let results =
                  -- in verbose mode, aliases are shown, so we collapse all
                  -- aliases to a single search result; in non-verbose mode,
                  -- a separate result may be shown for each alias
                  (if isVerbose then uniqueBy SR.toReferent else id) $
                  searchResultsFor prettyPrintNames0 matches []
            pure . pure $ results

          -- name query
          (map HQ.unsafeFromString -> qs) -> do
            ns <- lift $ basicPrettyPrintNames0
            let srs = searchBranchScored ns fuzzyNameDistance qs
            pure $ uniqueBy SR.toReferent srs

        case results of
          Left error -> respond error
          Right results -> do
            numberedArgs .= fmap searchResultToHQString results
            results' <- loadSearchResults results
            ppe <- prettyPrintEnv =<<
              makePrintNamesFromLabeled'
                (foldMap SR'.labeledDependencies results')
            respond $ ListOfDefinitions ppe isVerbose results'

      ResolveTypeNameI hq ->
        zeroOneOrMore (getHQ'Types hq) (typeNotFound hq) go (typeConflicted hq)
        where
        conflicted = getHQ'Types (fmap HQ'.toNameOnly hq)
        makeDelete =
          BranchUtil.makeDeleteTypeName (resolveSplit' (HQ'.toName <$> hq))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermNameI hq -> do
        refs <- getHQ'TermsIncludingHistorical hq
        zeroOneOrMore refs (termNotFound hq) go (termConflicted hq)
        where
        conflicted = getHQ'Terms (fmap HQ'.toNameOnly hq)
        makeDelete =
          BranchUtil.makeDeleteTermName (resolveSplit' (HQ'.toName <$> hq))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermI from to patchPath -> do
        let patchPath' = fromMaybe defaultPatchPath patchPath
        patch <- getPatchAt patchPath'
        fromRefs <- eval $ ReferencesByShortHash from
        toRefs <- eval $ ReferencesByShortHash to
        let go :: Reference.Id
               -> Reference.Id
               -> Action m (Either Event Input) v ()
            go fid tid = do
              let fr = DerivedId fid
                  tr = DerivedId tid
              mft <- eval $ LoadTypeOfTerm fr
              mtt <- eval $ LoadTypeOfTerm tr
              case (mft, mtt) of
                (Nothing, _) -> respond $ TermNotFound' input fid
                (_, Nothing) -> respond $ TermNotFound' input tid
                (Just ft, Just tt) -> do
                  let
                      patch' =
                        -- The modified patch
                        over Patch.termEdits
                          (R.insert fr (Replace tr (TermEdit.typing tt ft))
                           . R.deleteDom fr)
                          patch
                      (patchPath'', patchName) = resolveSplit' patchPath'
                  -- Save the modified patch
                  stepAtM (inputDescription <> " (1/2)")
                          (patchPath'',
                           Branch.modifyPatches patchName (const patch'))
                  -- Apply the modified patch to the current path
                  -- since we might be able to propagate further.
                  void $ propagatePatch inputDescription patch' currentPath'
                  -- Say something
                  success
        zeroOneOrMore
          fromRefs
          (respond $ SearchTermsNotFound [HQ.HashOnly from])
          (\r -> zeroOneOrMore toRefs
                               (respond $ SearchTermsNotFound [HQ.HashOnly to])
                               (go r)
                               (hashConflicted to .
                                 Set.map (Referent.Ref . DerivedId)))
          (hashConflicted from .
            Set.map (Referent.Ref . DerivedId))

      ResolveTypeI from to patchPath -> do
        let patchPath' = fromMaybe defaultPatchPath patchPath
        patch <- getPatchAt patchPath'
        fromRefs <- eval $ ReferencesByShortHash from
        toRefs <- eval $ ReferencesByShortHash to
        let go :: Reference.Id
               -> Reference.Id
               -> Action m (Either Event Input) v ()
            go fid tid = do
              let fr = DerivedId fid
                  tr = DerivedId tid
                  patch' =
                    -- The modified patch
                    over Patch.typeEdits
                      (R.insert fr (TypeEdit.Replace tr) . R.deleteDom fr) patch
                  (patchPath'', patchName) = resolveSplit' patchPath'
              -- Save the modified patch
              stepAtM (inputDescription <> " (1/2)")
                      (patchPath'', Branch.modifyPatches patchName (const patch'))
              -- Apply the modified patch to the current path
              -- since we might be able to propagate further.
              void $ propagatePatch inputDescription patch' currentPath'
              -- Say something
              success
        zeroOneOrMore
          fromRefs
          (respond $ SearchTermsNotFound [HQ.HashOnly from])
          (\r -> zeroOneOrMore toRefs
                               (respond $ SearchTermsNotFound [HQ.HashOnly to])
                               (go r)
                               (hashConflicted to .
                                 Set.map (Referent.Ref . DerivedId)))
          (hashConflicted from .
            Set.map (Referent.Ref . DerivedId))

      AddI hqs -> case uf of
        Nothing -> respond $ NoUnisonFile input
        Just uf -> do
          sr <- Slurp.disallowUpdates
              . applySelection hqs uf
              . toSlurpResult uf
             <$> slurpResultNames0
          when (Slurp.isNonempty sr) $ do
            stepAt ( Path.unabsolute currentPath'
                   , doSlurpAdds (Slurp.adds sr) uf)
            eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
          ppe <- prettyPrintEnv =<<
            makeShadowedPrintNamesFromLabeled
              (UF.termSignatureExternalLabeledDependencies uf)
              (UF.typecheckedToNames0 uf)
          respond $ SlurpOutput input ppe sr

      UpdateI maybePatchPath hqs -> case uf of
        Nothing -> respond $ NoUnisonFile input
        Just uf -> do
          let patchPath = fromMaybe defaultPatchPath maybePatchPath
          slurpCheckNames0 <- slurpResultNames0
          currentPathNames0 <- currentPathNames0
          let sr = applySelection hqs uf
                 . toSlurpResult uf
                 $ slurpCheckNames0
          let fileNames0 = UF.typecheckedToNames0 uf
              -- todo: display some error if typeEdits or termEdits itself contains a loop
              typeEdits :: Map Name (Reference, Reference)
              typeEdits = Map.fromList $ map f (toList $ SC.types (updates sr)) where
                f v = case (toList (Names.typesNamed slurpCheckNames0 n)
                           ,toList (Names.typesNamed fileNames0 n)) of
                  ([old],[new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                  ++ Var.nameStr v ++ " but got: "
                                  ++ show otherwise
                  where n = Name.fromVar v
              hashTerms :: Map Reference (Type v Ann)
              hashTerms = Map.fromList (toList hashTerms0) where
                hashTerms0 = (\(r, _, typ) -> (r, typ)) <$> UF.hashTerms uf
              termEdits :: Map Name (Reference, Reference)
              termEdits = Map.fromList $ map g (toList $ SC.terms (updates sr)) where
                g v = case ( toList (Names.refTermsNamed slurpCheckNames0 n)
                           , toList (Names.refTermsNamed fileNames0 n)) of
                  ([old], [new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                 ++ Var.nameStr v ++ " but got: "
                                 ++ show otherwise
                  where n = Name.fromVar v
              termDeprecations :: [(Name, Referent)]
              termDeprecations =
                [ (n, r) | (oldTypeRef,_) <- Map.elems typeEdits
                         , (n, r) <- Names3.constructorsForType0 oldTypeRef currentPathNames0 ]

          ye'ol'Patch <- getPatchAt patchPath
          -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
          -- with (a0 -> a') in patch'.
          -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
          -- we must know the type of a0, a, a'.
          let
            -- we need:
            -- all of the `old` references from the `new` edits,
            -- plus all of the `old` references for edits from patch we're replacing
            collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
            collectOldForTyping new old = foldl' f mempty (new ++ fromOld) where
              f acc (r, _r') = Set.insert r acc
              newLHS = Set.fromList . fmap fst $ new
              fromOld :: [(Reference, Reference)]
              fromOld = [ (r,r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old
                                 , Set.member r' newLHS ]
            neededTypes = collectOldForTyping (toList termEdits) ye'ol'Patch

          allTypes :: Map Reference (Type v Ann) <-
            fmap Map.fromList . for (toList neededTypes) $ \r ->
              (r,) . fromJust <$> (eval . LoadTypeOfTerm) r

          let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 hashTerms) of
                (Just t1, Just t2)
                  | Typechecker.isEqual t1 t2 -> TermEdit.Same
                  | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
                  | otherwise -> TermEdit.Different
                e -> error $ "compiler bug: typing map not constructed properly\n" <>
                  "typing " <> show r1 <> " " <> show r2 <> " : " <> show e

          let updatePatch :: Patch -> Patch
              updatePatch p = foldl' step2 p' termEdits
                where
                p' = foldl' step1 p typeEdits
                step1 p (r,r') = Patch.updateType r (TypeEdit.Replace r') p
                step2 p (r,r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
              (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
              updatePatches :: Branch0 m -> m (Branch0 m)
              updatePatches = Branch.modifyPatches seg updatePatch

          when (Slurp.isNonempty sr) $ do
          -- take a look at the `updates` from the SlurpResult
          -- and make a patch diff to record a replacement from the old to new references
            stepManyAtM
              [( Path.unabsolute currentPath'
               , pure . doSlurpUpdates typeEdits termEdits termDeprecations)
              ,( Path.unabsolute currentPath'
               , pure . doSlurpAdds (Slurp.updates sr <> Slurp.adds sr) uf)
              ,( Path.unabsolute p, updatePatches )]
            eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
          let fileNames0 = UF.typecheckedToNames0 uf
          ppe <- prettyPrintEnv =<<
            makeShadowedPrintNamesFromLabeled
              (UF.termSignatureExternalLabeledDependencies uf)
              (UF.typecheckedToNames0 uf)
          respond $ SlurpOutput input ppe sr
          -- propagatePatch prints TodoOutput
          void $ propagatePatch inputDescription (updatePatch ye'ol'Patch) currentPath'

      TodoI patchPath branchPath' -> do
        patch <- getPatchAt (fromMaybe defaultPatchPath patchPath)
        names <- makePrintNamesFromLabeled' $ Patch.labeledDependencies patch
        ppe <- prettyPrintEnv names
        branch <- getAt $ Path.toAbsolutePath currentPath' branchPath'
        let names0 = Branch.toNames0 (Branch.head branch)
        -- showTodoOutput only needs the local references
        -- to check for obsolete defs
        showTodoOutput ppe patch names0

      TestI showOk showFail -> do
        let
          testTerms = Star3.fact . Star3.select1D3 isTest
                    . Branch.deepTerms $ currentBranch0
          testRefs = Set.fromList [ r | Referent.Ref r <- toList testTerms ]
          oks results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.okConstructorId && ref == DD.testResultRef ]
          fails results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.failConstructorId && ref == DD.testResultRef ]
        cachedTests <- fmap Map.fromList . eval $ LoadWatches UF.TestWatch testRefs
        let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
        names <- makePrintNamesFromLabeled' $
          LD.referents testTerms <>
          LD.referents [ DD.okConstructorReferent, DD.failConstructorReferent ]
        ppe <- prettyPrintEnv names
        respond $ TestResults stats ppe showOk showFail
                    (oks cachedTests) (fails cachedTests)
        let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
        unless (Set.null toCompute) $ do
          let total = Set.size toCompute
          computedTests <- fmap join . for (toList toCompute `zip` [1..]) $ \(r,n) ->
            case r of
              Reference.DerivedId rid -> do
                tm <- eval $ LoadTerm rid
                case tm of
                  Nothing -> [] <$ respond (TermNotFound' input rid)
                  Just tm -> do
                    respond $ TestIncrementalOutputStart ppe (n,total) r tm
                    tm' <- eval (Evaluate1 ppe tm) <&> \case
                      Left e -> Term.seq External
                        [ DD.failResult External (Text.pack $ P.toANSI 80 ("\n" <> e)) ]
                      Right tm' -> tm'
                    eval $ PutWatch UF.TestWatch rid tm'
                    respond $ TestIncrementalOutputEnd ppe (n,total) r tm'
                    pure [(r, tm')]
              r -> error $ "unpossible, tests can't be builtins: " <> show r
          let m = Map.fromList computedTests
          respond $ TestResults Output.NewlyComputed ppe showOk showFail (oks m) (fails m)

      -- ListBranchesI ->
      --   eval ListBranches >>= respond . ListOfBranches currentBranchName'
      -- DeleteBranchI branchNames -> withBranches branchNames $ \bnbs -> do
      --   uniqueToDelete <- prettyUniqueDefinitions bnbs
      --   let deleteBranches b =
      --         traverse (eval . DeleteBranch) b >> respond (Success input)
      --   if (currentBranchName' `elem` branchNames)
      --     then respond DeletingCurrentBranch
      --     else if null uniqueToDelete
      --       then deleteBranches branchNames
      --       else ifM (confirmedCommand input)
      --                (deleteBranches branchNames)
      --                (respond . DeleteBranchConfirmation $ uniqueToDelete)

      PropagatePatchI patchPath scopePath -> do
        patch <- getPatchAt patchPath
        updated <- propagatePatch inputDescription patch (resolveToAbsolute scopePath)
        unless updated (respond $ NothingToPatch patchPath scopePath)

      ExecuteI main -> addRunMain main uf >>= \case
        Nothing -> do
          names0 <- basicPrettyPrintNames0
          ppe <- prettyPrintEnv (Names3.Names names0 mempty)
          respond $ NoMainFunction input main ppe (mainTypes External)
        Just unisonFile -> do
          ppe <- executePPE unisonFile
          eval $ Execute ppe unisonFile

      -- UpdateBuiltinsI -> do
      --   stepAt updateBuiltins
      --   checkTodo

      MergeBuiltinsI -> do
          let names0 = Builtin.names0
                       <> UF.typecheckedToNames0 IOSource.typecheckedFile
          let b0 = BranchUtil.addFromNames0 names0 Branch.empty0
          let srcb = Branch.one b0
          _ <- updateAtM (Path.consAbsolute "builtin" currentPath') $ \destb ->
                 eval . Eval $ Branch.merge srcb destb
          success

      ListEditsI maybePath -> do
        let (p, seg) =
              maybe (Path.toAbsoluteSplit currentPath' defaultPatchPath)
                    (Path.toAbsoluteSplit currentPath')
                    maybePath
        patch <- eval . Eval . Branch.getPatch seg . Branch.head =<< getAt p
        ppe <- prettyPrintEnv =<<
          makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        respond $ ListEdits patch ppe

      PullRemoteBranchI mayRepo path -> do
        let p = Path.toAbsolutePath currentPath' path
        case mayRepo of
          Just repo ->
            loadRemoteBranchAt input inputDescription repo p
          Nothing -> do
            repoUrl <- eval . ConfigLookup $ "GitUrl" <> Text.pack (show p)
            case repoUrl of
              Just url -> loadRemoteBranchAt input inputDescription (GitRepo url "master") p
              Nothing ->
                eval . Notify $ NoConfiguredGitUrl Pull path

      PushRemoteBranchI mayRepo path -> do
        let p = Path.toAbsolutePath currentPath' path
        b <- getAt p
        case mayRepo of
          Just repo -> syncRemoteRootBranch input repo b
          Nothing -> do
            repoUrl <-
              eval . ConfigLookup $ gitUrlKey p
            case repoUrl of
              Just url -> syncRemoteRootBranch input (GitRepo url "master") b
              Nothing ->
                eval . Notify $ NoConfiguredGitUrl Push path
      DebugBranchHistoryI ->
        eval . Notify . DumpBitBooster (Branch.headHash currentBranch') =<<
          (eval . Eval $ Causal.hashToRaw (Branch._history currentBranch'))

      DeprecateTermI {} -> notImplemented
      DeprecateTypeI {} -> notImplemented
      AddTermReplacementI {} -> notImplemented
      AddTypeReplacementI {} -> notImplemented
      RemoveTermReplacementI {} -> notImplemented
      RemoveTypeReplacementI {} -> notImplemented
      ShowDefinitionByPrefixI {} -> notImplemented
      UpdateBuiltinsI -> notImplemented
      QuitI -> MaybeT $ pure Nothing
     where
      notImplemented = eval $ Notify NotImplemented
      success = respond $ Success input
      gitUrlKey p = Text.intercalate "." . toList $ "GitUrl" :<| fmap
        NameSegment.toText
        (Path.toSeq $ Path.unabsolute p)
  case e of
    Right input -> lastInput .= Just input
    _ -> pure ()
 -- where
  {-
  doMerge branchName b = do
    updated <- eval $ SyncBranch branchName b
    -- updated is False if `branchName` doesn't exist.
    -- Not sure why you were updating a nonexistent branch, but under the
    -- assumption that it just got deleted somehow, I guess, we'll write
    -- it to disk now.
    unless updated $ do
      written <- eval $ NewBranch b branchName
      unless written (disappearingBranchBomb branchName)
  disappearingBranchBomb branchName =
    error
      $  "The branch named "
      <> Text.unpack branchName
      <> " disappeared from storage. "
      <> "I tried to put it back, but couldn't. Everybody panic!"
  -}

doDisplay :: Var v => OutputLocation -> Names -> Referent -> Action' m v ()
doDisplay outputLoc names r = do
  let tm = Term.fromReferent External r
  ppe <- prettyPrintEnv names
  latestFile' <- use latestFile
  let
    loc = case outputLoc of
      ConsoleLocation    -> Nothing
      FileLocation path  -> Just path
      LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
    evalTerm r = fmap ErrorUtil.hush . eval $ Evaluate1 ppe (Term.ref External r)
    loadTerm (Reference.DerivedId r) = eval $ LoadTerm r
    loadTerm r = pure Nothing
    loadDecl (Reference.DerivedId r) = eval $ LoadType r
    loadDecl _ = pure Nothing
  rendered <- DisplayValues.displayTerm ppe loadTerm loadTypeOfTerm evalTerm loadDecl tm
  respond $ DisplayRendered loc rendered
  -- We set latestFile to be programmatically generated, if we
  -- are viewing these definitions to a file - this will skip the
  -- next update for that file (which will happen immediately)
  latestFile .= ((, True) <$> loc)

getLinks :: (Var v, Monad m)
         => Input
         -> Path.HQSplit'
         -> Either (Set Reference) (Maybe String)
         -> Action' m v (Either (Output v)
                                (PPE.PrettyPrintEnv,
                                 [(HQ.HashQualified, Reference, Maybe (Type v Ann))]))
getLinks input src mdTypeStr = do
  root0 <- Branch.head <$> use root
  currentPath' <- use currentPath
  let resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      getHQ'Terms p = BranchUtil.getTerm (resolveSplit' p) root0
      getHQ'Types p = BranchUtil.getType (resolveSplit' p) root0
      srcle = toList (getHQ'Terms src) -- list of matching src terms
      srclt = toList (getHQ'Types src) -- list of matching src types
      p = resolveSplit' src -- ex: the parent of `List.map` - `List`
      mdTerms = foldl' Metadata.merge mempty [
        BranchUtil.getTermMetadataUnder p r root0 | r <- srcle ]
      mdTypes = foldl' Metadata.merge mempty [
        BranchUtil.getTypeMetadataUnder p r root0 | r <- srclt ]
      allMd = Metadata.merge mdTerms mdTypes
  selection <- case mdTypeStr of
    Left s -> pure $ Right s
    Right Nothing -> pure $ Right (Map.keysSet allMd)
    Right (Just mdTypeStr) -> parseType input mdTypeStr <&> \case
      Left e -> Left e
      Right typ -> Right (Set.singleton (Type.toReference typ))
  case selection of
    Left e -> pure (Left e)
    Right selection -> do
      let allMd' = Map.restrictKeys allMd selection
          allRefs = toList (Set.unions (Map.elems allMd'))
          results :: Set Reference = Set.unions $ Map.elems allMd'
      sigs <- for (toList results) $
        \r -> loadTypeOfTerm (Referent.Ref r)
      let deps = Set.map LD.termRef results <>
                 Set.unions [ Set.map LD.typeRef . Type.dependencies $ t | Just t <- sigs ]
      ppe <- prettyPrintEnv =<< makePrintNamesFromLabeled' deps
      let sortedSigs = sortOn snd (toList results `zip` sigs)
      let out = [(PPE.termName ppe (Referent.Ref r), r, t) | (r, t) <- sortedSigs ]
      pure (Right (ppe, out))

resolveShortBranchHash ::
  Input -> ShortBranchHash -> Action' m v (Either (Output v) (Branch m))
resolveShortBranchHash input hash = do
  hashSet <- eval $ BranchHashesByPrefix hash
  len <- eval BranchHashLength
  case Set.toList hashSet of
    []  -> pure . Left $ NoBranchWithHash input hash
    [h] -> fmap Right . eval $ LoadLocalBranch h
    _   -> pure . Left $ BranchHashAmbiguous input hash (Set.map (SBH.fromHash len) hashSet)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch :: (Monad m, Var v) =>
  Text -> Patch -> Path.Absolute -> Action' m v Bool
propagatePatch inputDescription patch scopePath = do
  changed <- do
    names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
    updateAtM (inputDescription <> " (patch propagation)")
              scopePath
              (lift . lift . Propagate.propagateAndApply patch)
  when changed $ do
    scope <- getAt scopePath
    let names0 = Branch.toNames0 (Branch.head scope)
    -- this will be different AFTER the update succeeds
    names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
    ppe <- prettyPrintEnv names
    showTodoOutput ppe patch names0
  pure changed

showTodoOutput :: PrettyPrintEnv -> Patch -> Names0 -> Action' m v ()
showTodoOutput ppe patch names0 = do
  todo <- checkTodo patch names0
  numberedArgs .=
    (Text.unpack . Reference.toText . view _2 <$>
       fst (TO.todoFrontierDependents todo))
  respond $ TodoOutput ppe todo

checkTodo :: Patch -> Names0 -> Action m i v (TO.TodoOutput v Ann)
checkTodo patch names0 = do
  f <- computeFrontier (eval . GetDependents) patch names0
  let dirty = R.dom f
      frontier = R.ran f
  (frontierTerms, frontierTypes) <- loadDisplayInfo frontier
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo dirty
  -- todo: something more intelligent here?
  let scoreFn = const 1
  remainingTransitive <-
    frontierTransitiveDependents (eval . GetDependents) names0 frontier
  let
    scoredDirtyTerms =
      List.sortOn (view _1) [ (scoreFn r, r, t) | (r,t) <- dirtyTerms ]
    scoredDirtyTypes =
      List.sortOn (view _1) [ (scoreFn r, r, t) | (r,t) <- dirtyTypes ]
  pure $
    TO.TodoOutput
      (Set.size remainingTransitive)
      (frontierTerms, frontierTypes)
      (scoredDirtyTerms, scoredDirtyTypes)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
  frontierTransitiveDependents ::
    Monad m => (Reference -> m (Set Reference)) -> Names0 -> Set Reference -> m (Set Reference)
  frontierTransitiveDependents dependents names0 rs = do
    let branchDependents r = Set.filter (Names.contains names0) <$> dependents r
    tdeps <- transitiveClosure branchDependents rs
    -- we don't want the frontier in the result
    pure $ tdeps `Set.difference` rs

-- (d, f) when d is "dirty" (needs update),
--             f is in the frontier (an edited dependency of d),
--         and d depends on f
-- a ⋖ b = a depends directly on b
-- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
computeFrontier :: forall m . Monad m
         => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
         -> Patch
         -> Names0
         -> m (R.Relation Reference Reference)
computeFrontier getDependents patch names = let
  edited :: Set Reference
  edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)
  addDependents :: R.Relation Reference Reference -> Reference -> m (R.Relation Reference Reference)
  addDependents dependents ref =
    (\ds -> R.insertManyDom ds ref dependents) . Set.filter (Names.contains names)
      <$> getDependents ref
  in do
    -- (r,r2) ∈ dependsOn if r depends on r2
    dependsOn <- foldM addDependents R.empty edited
    -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
    pure $ R.filterDom (not . flip Set.member edited) dependsOn

eval :: Command m i v a -> Action m i v a
eval = lift . lift . Free.eval

confirmedCommand :: Input -> Action m i v Bool
confirmedCommand i = do
  i0 <- use lastInput
  pure $ Just i == i0

listBranch :: Branch0 m -> [SearchResult]
listBranch (Branch.toNames0 -> b) =
  List.sortOn (\s -> (SR.name s, s)) (Names.asSearchResults b)

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ'.toString $ HQ'.requalify n r
  SR.Tp' n r _ -> HQ'.toString $ HQ'.requalify n (Referent.Ref r)
  _ -> error "unpossible match failure"

-- Return a list of definitions whose names fuzzy match the given queries.
fuzzyNameDistance :: Name -> Name -> Maybe _ -- MatchArray
fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
  Find.simpleFuzzyScore q n

-- return `name` and `name.<everything>...`
_searchBranchPrefix :: Branch m -> Name -> [SearchResult]
_searchBranchPrefix b n = case Path.unsnoc (Path.fromName n) of
  Nothing -> []
  Just (init, last) -> case Branch.getAt init b of
    Nothing -> []
    Just b -> Names.asSearchResults . Names.prefix0 n $ names0
      where
      lastName = Path.toName (Path.singleton last)
      subnames = Branch.toNames0 . Branch.head $
                   Branch.getAt' (Path.singleton last) b
      rootnames =
        Names.filter (== lastName) .
        Branch.toNames0 . set Branch.children mempty $ Branch.head b
      names0 = rootnames <> Names.prefix0 lastName subnames

searchResultsFor :: Names0 -> [Referent] -> [Reference] -> [SearchResult]
searchResultsFor ns terms types =
  [ Names.termSearchResult ns (Names.termName ns ref) ref | ref <- terms ] <>
  [ Names.typeSearchResult ns (Names.typeName ns ref) ref | ref <- types ]

searchBranchScored :: forall score. (Ord score)
              => Names0
              -> (Name -> Name -> Maybe score)
              -> [HQ.HashQualified]
              -> [SearchResult]
searchBranchScored names0 score queries =
  nubOrd . fmap snd . toList $ searchTermNamespace <> searchTypeNamespace
  where
  searchTermNamespace = foldMap do1query queries
    where
    do1query :: HQ.HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.terms $ names0)
    score1hq :: HQ.HashQualified -> (Name, Referent) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` Referent.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Referent.toShortHash ref ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.termSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty
  searchTypeNamespace = foldMap do1query queries
    where
    do1query :: HQ.HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.types $ names0)
    score1hq :: HQ.HashQualified -> (Name, Reference) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` Reference.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Reference.toShortHash ref ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.typeSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty

-- Separates type references from term references and returns types and terms,
-- respectively. For terms that are constructors, turns them into their data
-- types.
collateReferences
  :: Foldable f
  => Foldable g
  => f Reference -- types requested
  -> g Referent -- terms requested, including ctors
  -> (Set Reference, Set Reference)
collateReferences (toList -> types) (toList -> terms) =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = [ r | Referent.Con r _ _ <- terms ]
  in  (Set.fromList types' <> Set.fromList types, Set.fromList terms')

-- | The output list (of lists) corresponds to the query list.
searchBranchExact :: Int -> Names -> [HQ.HashQualified] -> [[SearchResult]]
searchBranchExact len names queries = let
  searchTypes :: HQ.HashQualified -> [SearchResult]
  searchTypes query =
    -- a bunch of references will match a HQ ref.
    let refs = toList $ Names3.lookupHQType query names in
    refs <&> \r ->
      let hqNames = Names3.typeName len r names in
      let primaryName =
            last . sortOn (\n -> HQ.matchesNamedReference (HQ'.toName n) r query)
                 $ toList hqNames in
      let aliases = Set.delete primaryName hqNames in
      SR.typeResult primaryName r aliases
  searchTerms :: HQ.HashQualified -> [SearchResult]
  searchTerms query =
    -- a bunch of references will match a HQ ref.
    let refs = toList $ Names3.lookupHQTerm query names in
    refs <&> \r ->
      let hqNames = Names3.termName len r names in
      let primaryName =
            last . sortOn (\n -> HQ.matchesNamedReferent (HQ'.toName n) r query)
                 $ toList hqNames in
      let aliases = Set.delete primaryName hqNames in
      SR.termResult primaryName r aliases
  in [ searchTypes q <> searchTerms q | q <- queries ]

respond :: Output v -> Action m i v ()
respond output = eval $ Notify output

loadRemoteBranchAt
  :: Var v
  => Monad m
  => Input
  -> Text
  -> RemoteRepo
  -> Path.Absolute
  -> Action' m v ()
loadRemoteBranchAt input inputDescription repo p = do
  b <- eval (LoadRemoteRootBranch repo)
  case b of
    Left  e -> eval . Notify $ GitError input e
    Right b -> do
      changed <- updateAtM inputDescription p (doMerge b)
      when changed $ do
        merged <- getAt p
        patch  <- eval . Eval $ Branch.getPatch defaultPatchNameSegment
                                                (Branch.head merged)
        void $ propagatePatch inputDescription patch p
 where
  doMerge b b0 = do
    merged <- eval . Eval $ Branch.merge b b0
    respond $ ShowDiff input (Branch.namesDiff b0 merged)
    pure merged

syncRemoteRootBranch
  :: Var v => Monad m => Input -> RemoteRepo -> Branch m -> Action m i v ()
syncRemoteRootBranch input repo b = do
  e <- eval $ SyncRemoteRootBranch repo b
  either (eval . Notify . GitError input) (const . respond $ Success input) e

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use root <&> fromMaybe Branch.empty . Branch.getAt p

-- Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM :: Applicative m
          => Text
          -> Path.Absolute
          -> (Branch m -> Action m i v (Branch m))
          -> Action m i v Bool
updateAtM reason (Path.Absolute p) f = do
  b <- use root
  b' <- Branch.modifyAtM p f b
  updateRoot b b' reason
  pure $ b /= b'

stepAt :: forall m i v. Applicative m
       => Text
       -> (Path, Branch0 m -> Branch0 m)
       -> Action m i v ()
stepAt cause = stepManyAt @m @[] cause . pure

stepAtM :: forall m i v. Monad m
        => Text
        -> (Path, Branch0 m -> m (Branch0 m))
        -> Action m i v ()
stepAtM cause = stepManyAtM @m @[] cause . pure

stepManyAt :: (Applicative m, Foldable f)
           => Text
           -> f (Path, Branch0 m -> Branch0 m)
           -> Action m i v ()
stepManyAt reason actions = do
    b <- use root
    let b' = Branch.stepManyAt actions b
    updateRoot b b' reason

stepManyAtM :: (Monad m, Foldable f)
           => Text
           -> f (Path, Branch0 m -> m (Branch0 m))
           -> Action m i v ()
stepManyAtM reason actions = do
    b <- use root
    b' <- eval . Eval $ Branch.stepManyAtM actions b
    updateRoot b b' reason

updateRoot :: Branch m -> Branch m -> Text -> Action m i v ()
updateRoot old new reason = when (old /= new) $ do
  root .= new
  eval $ SyncLocalRootBranch new
  eval $ AppendToReflog reason old new

-- cata for 0, 1, or more elements of a Foldable
-- tries to match as lazily as possible
zeroOneOrMore :: Foldable f => f a -> b -> (a -> b) -> (f a -> b) -> b
zeroOneOrMore f zero one more = case toList f of
  _ : _ : _ -> more f
  a : _ -> one a
  _ -> zero

-- Goal: If `remaining = root - toBeDeleted` contains definitions X which
-- depend on definitions Y not in `remaining` (which should also be in
-- `toBeDeleted`), then complain by returning (Y, X).
getEndangeredDependents :: forall m. Monad m
                        => (Reference -> m (Set Reference))
                        -> Names0
                        -> Names0
                        -> m (Names0, Names0)
getEndangeredDependents getDependents toDelete root = do
  let remaining  = root `Names.difference` toDelete
      toDelete', remaining', extinct :: Set Reference
      toDelete'  = Names.allReferences toDelete
      remaining' = Names.allReferences remaining          -- left over after delete
      extinct    = toDelete'  `Set.difference` remaining' -- deleting and not left over
      accumulateDependents m r = getDependents r <&> \ds -> Map.insert r ds m
  dependentsOfExtinct :: Map Reference (Set Reference) <-
    foldM accumulateDependents mempty extinct
  let orphaned, endangered, failed :: Set Reference
      orphaned   = fold dependentsOfExtinct
      endangered = orphaned `Set.intersection` remaining'
      failed = Set.filter hasEndangeredDependent extinct
      hasEndangeredDependent r = any (`Set.member` endangered)
                                     (dependentsOfExtinct Map.! r)
  pure ( Names.restrictReferences failed toDelete
       , Names.restrictReferences endangered root `Names.difference` toDelete)

-- Applies the selection filter to the adds/updates of a slurp result,
-- meaning that adds/updates should only contain the selection or its transitive
-- dependencies, any unselected transitive dependencies of the selection will
-- be added to `extraDefinitions`.
applySelection :: forall v a. Var v =>
  [HQ'.HashQualified] -> UF.TypecheckedUnisonFile v a -> SlurpResult v -> SlurpResult v
applySelection [] _ = id
applySelection hqs file = \sr@SlurpResult{..} ->
  sr { adds = adds `SC.intersection` closed
     , updates = updates `SC.intersection` closed
     , extraDefinitions = closed `SC.difference` selection
     }
  where
  selectedNames0 =
    Names.filterByHQs (Set.fromList hqs) (UF.typecheckedToNames0 file)
  selection, closed :: SlurpComponent v
  selection = SlurpComponent selectedTypes selectedTerms
  closed = SC.closeWithDependencies file selection
  selectedTypes, selectedTerms :: Set v
  selectedTypes = Set.map var $ R.dom (Names.types selectedNames0)
  selectedTerms = Set.map var $ R.dom (Names.types selectedNames0)

var :: Var v => Name -> v
var name = Var.named (Name.toText name)

toSlurpResult :: forall v. Var v => UF.TypecheckedUnisonFile v Ann -> Names0 -> SlurpResult v
toSlurpResult uf existingNames =
  Slurp.subtractComponent (conflicts <> ctorCollisions) $
  SlurpResult uf mempty adds dups mempty conflicts updates
              termCtorCollisions ctorTermCollisions termAliases typeAliases
              mempty
  where
  fileNames0 = UF.typecheckedToNames0 uf

  sc :: R.Relation Name Referent -> R.Relation Name Reference -> SlurpComponent v
  sc terms types = SlurpComponent { terms = Set.map var (R.dom terms)
                                  , types = Set.map var (R.dom types) }

  -- conflict (n,r) if n is conflicted in names0
  conflicts :: SlurpComponent v
  conflicts = sc terms types where
    terms = R.filterDom (conflicted . Names.termsNamed existingNames) (Names.terms fileNames0)
    types = R.filterDom (conflicted . Names.typesNamed existingNames) (Names.types fileNames0)
    conflicted s = Set.size s > 1

  ctorCollisions :: SlurpComponent v
  ctorCollisions =
    mempty { SC.terms = termCtorCollisions <> ctorTermCollisions }

  -- termCtorCollision (n,r) if (n, r' /= r) exists in existingNames and r is Ref and r' is Con
  termCtorCollisions :: Set v
  termCtorCollisions = Set.fromList
    [ var n | (n, Referent.Ref{}) <- R.toList (Names.terms fileNames0)
            , [r@Referent.Con{}] <- [toList $ Names.termsNamed existingNames n]
            -- ignore collisions w/ ctors of types being updated
            , Set.notMember (Referent.toReference r) typesToUpdate
            ]

  -- the set of typerefs that are being updated by this file
  typesToUpdate :: Set Reference
  typesToUpdate = Set.fromList
    [ r | (n,r') <- R.toList (Names.types fileNames0)
        , r <- toList (Names.typesNamed existingNames n)
        , r /= r' ]

  -- ctorTermCollisions (n,r) if (n, r' /= r) exists in names0 and r is Con and r' is Ref
  -- except we relaxed it to where r' can be Con or Ref
  -- what if (n,r) and (n,r' /= r) exists in names and r, r' are Con
  ctorTermCollisions :: Set v
  ctorTermCollisions = Set.fromList
    [ var n | (n, Referent.Con{}) <- R.toList (Names.terms fileNames0)
            , r <- toList $ Names.termsNamed existingNames n
            -- ignore collisions w/ ctors of types being updated
            , Set.notMember (Referent.toReference r) typesToUpdate
            ]

  -- duplicate (n,r) if (n,r) exists in names0
  dups :: SlurpComponent v
  dups = sc terms types where
    terms = R.intersection (Names.terms existingNames) (Names.terms fileNames0)
    types = R.intersection (Names.types existingNames) (Names.types fileNames0)

  -- update (n,r) if (n,r' /= r) exists in names0 and r, r' are Ref
  updates :: SlurpComponent v
  updates = SlurpComponent (Set.fromList types) (Set.fromList terms) where
    terms = [ var n | (n,r'@Referent.Ref{}) <- R.toList (Names.terms fileNames0)
                    , [r@Referent.Ref{}] <- [toList $ Names.termsNamed existingNames n]
                    , r' /= r ]
    types = [ var n | (n,r') <- R.toList (Names.types fileNames0)
                    , [r] <- [toList $ Names.typesNamed existingNames n]
                    , r' /= r ]

  -- alias (n, r) if (n' /= n, r) exists in names0
  termAliases :: Map v (Set Name)
  termAliases = Map.fromList
    [ (var n, aliases)
    | (n, r@Referent.Ref{}) <- R.toList $ Names.terms fileNames0
    , aliases <- [Set.delete n $ R.lookupRan r (Names.terms existingNames)]
    , not (null aliases)
    , let v = var n
    , Set.notMember v (SC.terms dups)
    ]

  typeAliases :: Map v (Set Name)
  typeAliases = Map.fromList
    [ (v, aliases)
    | (n, r) <- R.toList $ Names.types fileNames0
    , aliases <- [Set.delete n $ R.lookupRan r (Names.types existingNames)]
    , not (null aliases)
    , let v = var n
    , Set.notMember v (SC.types dups)
    ]

  -- add (n,r) if n doesn't exist and r doesn't exist in names0
  adds = sc terms types where
    terms = addTerms (Names.terms existingNames) (Names.terms fileNames0)
    types = addTypes (Names.types existingNames) (Names.types fileNames0)
    addTerms existingNames = R.filter go where
      go (n, r@Referent.Ref{}) = (not . R.memberDom n) existingNames
                              && (not . R.memberRan r) existingNames
      go _ = False
    addTypes existingNames = R.filter go where
      go (n, r) = (not . R.memberDom n) existingNames
               && (not . R.memberRan r) existingNames



filterBySlurpResult :: Ord v
           => SlurpResult v
           -> UF.TypecheckedUnisonFile v Ann
           -> UF.TypecheckedUnisonFile v Ann
filterBySlurpResult SlurpResult{..} UF.TypecheckedUnisonFile{..} =
  UF.TypecheckedUnisonFile datas effects tlcs watches hashTerms'
  where
  keep = updates <> adds
  keepTerms = SC.terms keep
  keepTypes = SC.types keep
  hashTerms' = Map.restrictKeys hashTerms keepTerms
  datas = Map.restrictKeys dataDeclarations' keepTypes
  effects = Map.restrictKeys effectDeclarations' keepTypes
  tlcs = filter (not.null) $ fmap (List.filter filterTLC) topLevelComponents'
  watches = filter (not.null.snd) $ fmap (second (List.filter filterTLC)) watchComponents
  filterTLC (v,_,_) = Set.member v keepTerms

-- updates the namespace for adding `slurp`
doSlurpAdds :: forall m v. (Applicative m, Var v)
            => SlurpComponent v
            -> UF.TypecheckedUnisonFile v Ann
            -> (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.stepManyAt0 (typeActions <> termActions)
  where
  typeActions = map doType . toList $ SC.types slurp
  termActions = map doTerm . toList $
    SC.terms slurp <> Slurp.constructorsFor (SC.types slurp) uf
  names = UF.typecheckedToNames0 uf
  tests = Set.fromList $ fst <$> UF.watchesOfKind UF.TestWatch (UF.discardTypes uf)
  (isTestType, isTestValue) = isTest
  md v =
    if Set.member v tests then Metadata.singleton isTestType isTestValue
    else Metadata.empty
  doTerm :: v -> (Path, Branch0 m -> Branch0 m)
  doTerm v = case toList (Names.termsNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTermName split r (md v)
    wha -> error $ "Unison bug, typechecked file w/ multiple terms named "
                <> Var.nameStr v <> ": " <> show wha
  doType :: v -> (Path, Branch0 m -> Branch0 m)
  doType v = case toList (Names.typesNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTypeName split r Metadata.empty
    wha -> error $ "Unison bug, typechecked file w/ multiple types named "
                <> Var.nameStr v <> ": " <> show wha
  errorEmptyVar = error "encountered an empty var name"
  errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates :: Applicative m
               => Map Name (Reference, Reference)
               -> Map Name (Reference, Reference)
               -> [(Name, Referent)]
               -> (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits deprecated b0 =
  Branch.stepManyAt0 (typeActions <> termActions <> deprecateActions) b0
  where
  typeActions = join . map doType . Map.toList $ typeEdits
  termActions = join . map doTerm . Map.toList $ termEdits
  deprecateActions = join . map doDeprecate $ deprecated where
    doDeprecate (n, r) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split -> [BranchUtil.makeDeleteTermName split r]

  -- we copy over the metadata on the old thing
  -- todo: if the thing being updated, m, is metadata for something x in b0
  -- update x's md to reference `m`
  doType, doTerm ::
    (Name, (Reference, Reference)) -> [(Path, Branch0 m -> Branch0 m)]
  doType (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTypeName split old
                  , BranchUtil.makeAddTypeName split new oldMd ]
      where
      oldMd = BranchUtil.getTypeMetadataAt split old b0
  doTerm (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTermName split (Referent.Ref old)
                  , BranchUtil.makeAddTermName split (Referent.Ref new) oldMd ]
      where
      oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0
  errorEmptyVar = error "encountered an empty var name"

loadSearchResults :: Ord v => [SR.SearchResult] -> Action m i v [SearchResult' v Ann]
loadSearchResults = traverse loadSearchResult
  where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- loadReferentType r
      pure $ SR'.Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- loadTypeDisplayThing r
      pure $ SR'.Tp name dt r aliases

loadDisplayInfo ::
  Set Reference -> Action m i v ([(Reference, Maybe (Type v Ann))]
                                ,[(Reference, DisplayThing (DD.Decl v Ann))])
loadDisplayInfo refs = do
  termRefs <- filterM (eval . IsTerm) (toList refs)
  typeRefs <- filterM (eval . IsType) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> eval (LoadTypeOfTerm r)
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayThing r
  pure (terms, types)

loadReferentType :: Referent -> _ (Maybe (Type _ _))
loadReferentType = \case
  Referent.Ref r -> eval $ LoadTypeOfTerm r
  Referent.Con r cid _ -> getTypeOfConstructor r cid
  where
  getTypeOfConstructor :: Reference -> Int -> Action m i v (Maybe (Type v Ann))
  getTypeOfConstructor (Reference.DerivedId r) cid = do
    maybeDecl <- eval $ LoadType r
    pure $ case maybeDecl of
      Nothing -> Nothing
      Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
  getTypeOfConstructor r cid =
    error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

loadTermDisplayThing :: Ord v => Reference -> _ (DisplayThing (Term v _))
loadTermDisplayThing r = case r of
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id -> do
    tm <- eval (LoadTerm id)
    case tm of
      Nothing -> pure $ MissingThing id
      Just tm@(Term.Ann' _ _) -> pure $ RegularThing tm
      Just tm -> do
        ty <- eval $ LoadTypeOfTerm r
        case ty of
          Nothing -> pure $ MissingThing id
          Just ty -> pure $ RegularThing (Term.ann (ABT.annotation tm) tm ty)

loadTypeDisplayThing :: Reference -> _ (DisplayThing (DD.Decl _ _))
loadTypeDisplayThing = \case
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id ->
    maybe (MissingThing id) RegularThing <$> eval (LoadType id)

lexedSource :: Monad m => SourceName -> Source -> Action' m v (Names, LexedSource)
lexedSource name src = do
  let tokens = L.lexer (Text.unpack name) (Text.unpack src)
      getHQ = \case
        L.Backticks s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.WordyId   s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.SymbolyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.Hash      sh          -> Just (HQ.HashOnly sh)
        _                       -> Nothing
      hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
  parseNames <- makeHistoricalParsingNames hqs
  pure (parseNames, (src, tokens))

prettyPrintEnv :: Names -> Action' m v PPE.PrettyPrintEnv
prettyPrintEnv ns = eval CodebaseHashLength <&> (`PPE.fromNames` ns)

parseSearchType :: (Monad m, Var v)
  => Input -> String -> Action' m v (Either (Output v) (Type v Ann))
parseSearchType input typ = fmap Type.removeAllEffectVars <$> parseType input typ

parseType :: (Monad m, Var v)
  => Input -> String -> Action' m v (Either (Output v) (Type v Ann))
parseType input src = do
  -- `show Input` is the name of the "file" being lexed
  (names, lexed) <- lexedSource (Text.pack $ show input) (Text.pack src)
  e <- eval $ ParseType names lexed
  pure $ case e of
    Left err -> Left $ TypeParseError input src err
    Right typ -> case Type.bindNames mempty (Names3.currentNames names)
                    $ Type.generalizeLowercase mempty typ of
      Left es -> Left $ ParseResolutionFailures input src (toList es)
      Right typ -> Right typ

-- todo: likely broken when dealing with definitions with `.` in the name;
-- we don't have a spec for it yet.
resolveHQName :: Path.Absolute -> Name -> Name
resolveHQName (Path.unabsolute -> p) n =
  if p == Path.empty then n else case Name.toString n of
    '.' : _ : _ -> n
    _ -> Name.joinDot (Path.toName p) n

makeShadowedPrintNamesFromLabeled ::
  Monad m => Set LabeledDependency -> Names0 -> Action' m v Names
makeShadowedPrintNamesFromLabeled deps shadowing = do
  root <- use root
  currentPath <- use currentPath
  (_missing, rawHistoricalNames) <-
    eval . Eval $ Branch.findHistoricalRefs deps root
  basicNames0 <- basicPrettyPrintNames0
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    Names3.shadowing
      shadowing
      (Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames))

getTermsIncludingHistorical
  :: Monad m => Path.HQSplit -> Branch0 m -> Action' m v (Set Referent)
getTermsIncludingHistorical (p, hq) b = case Set.toList refs of
  [] -> case hq of
    HQ'.HashQualified n hs -> do
      names <- findHistoricalHQs
        $ Set.fromList [HQ.HashQualified (Name (NameSegment.toText n)) hs]
      pure . R.ran $ Names.terms names
    _ -> pure Set.empty
  _ -> pure refs
  where refs = BranchUtil.getTerm (p, hq) b

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Monad m => Set HQ.HashQualified -> Action' m v Names0
findHistoricalHQs lexedHQs0 = do
  root <- use root
  currentPath <- use currentPath
  let
    -- omg this nightmare name-to-path parsing code is littered everywhere.
    -- We need to refactor so that the absolute-ness of a name isn't represented
    -- by magical text combinations.
    -- Anyway, this function takes a name, tries to determine whether it is
    -- relative or absolute, and tries to return the corresponding name that is
    -- /relative/ to the root.
    preprocess n@(Name (Text.unpack -> t)) = case t of
      -- some absolute name that isn't just "."
      '.' : t@(_:_)  -> Name . Text.pack $ t
      -- something in current path
      _ ->  if Path.isRoot currentPath then n
            else Name.joinDot (Path.toName . Path.unabsolute $ currentPath) n

    lexedHQs = Set.map (fmap preprocess) . Set.filter HQ.hasHash $ lexedHQs0
  (_missing, rawHistoricalNames) <- eval . Eval $ Branch.findHistoricalHQs lexedHQs root
  pure rawHistoricalNames

makeShadowedPrintNamesFromHQ :: Monad m => Set HQ.HashQualified -> Names0 -> Action' m v Names
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames0 <- basicPrettyPrintNames0
  currentPath <- use currentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    Names3.shadowing
      shadowing
      (Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames))

makePrintNamesFromLabeled'
  :: Monad m => Set LabeledDependency -> Action' m v Names
makePrintNamesFromLabeled' deps = do
  root                           <- use root
  currentPath                    <- use currentPath
  (_missing, rawHistoricalNames) <- eval . Eval $ Branch.findHistoricalRefs
    deps
    root
  basicNames0 <- basicPrettyPrintNames0
  pure $ Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames)

-- a version of makeHistoricalPrintNames for printing errors for a file that didn't hash
makePrintNamesFromHQ :: Monad m => Set HQ.HashQualified -> Action' m v Names
makePrintNamesFromHQ lexedHQs = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames0 <- basicPrettyPrintNames0
  currentPath <- use currentPath
  pure $ Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames)


-- Any absolute names in the input which have `currentPath` as a prefix
-- are converted to names relative to current path. All other names are
-- converted to absolute names. For example:
--
-- e.g. if currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names0 -> Names0
fixupNamesRelative currentPath' = Names3.map0 fixName where
  prefix = Path.toName (Path.unabsolute currentPath')
  fixName n = if currentPath' == Path.absoluteEmpty then n else
    fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

makeHistoricalParsingNames ::
  Monad m => Set HQ.HashQualified -> Action' m v Names
makeHistoricalParsingNames lexedHQs = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames0 <- basicParseNames0
  currentPath <- use currentPath
  pure $ Names basicNames0
               (Names3.makeAbsolute0 rawHistoricalNames <>
                 fixupNamesRelative currentPath rawHistoricalNames)

basicParseNames0, basicPrettyPrintNames0, slurpResultNames0 :: Functor m => Action' m v Names0
basicParseNames0 = fst <$> basicNames0'
basicPrettyPrintNames0 = snd <$> basicNames0'
-- we check the file against everything we can reference during parsing
slurpResultNames0 = basicParseNames0

currentPathNames0 :: Functor m => Action' m v Names0
currentPathNames0 = do
  currentPath' <- use currentPath
  currentBranch' <- getAt currentPath'
  pure $ Branch.toNames0 (Branch.head currentBranch')

-- implementation detail of baseicParseNames0 and basicPrettyPrintNames0
basicNames0' :: Functor m => Action' m v (Names0, Names0)
basicNames0' = do
  root' <- use root
  currentPath' <- use currentPath
  currentBranch' <- getAt currentPath'
  let root0 = Branch.head root'
      absoluteRootNames0 = Names3.makeAbsolute0 (Branch.toNames0 root0)
      currentBranch0 = Branch.head currentBranch'
      currentPathNames0 = Branch.toNames0 currentBranch0
      -- all names, but with local names in their relative form only, rather
      -- than absolute; external names appear as absolute
      currentAndExternalNames0 = currentPathNames0 `Names3.unionLeft0` absDot externalNames where
        absDot = Names.prefix0 (Name.Name "")
        externalNames = rootNames `Names.difference` pathPrefixed currentPathNames0
        rootNames = Branch.toNames0 root0
        pathPrefixed = case Path.unabsolute currentPath' of
          Path.Path (toList -> []) -> id
          p -> Names.prefix0 (Path.toName p)
      -- parsing should respond to local and absolute names
      parseNames00 = currentPathNames0 <> absoluteRootNames0
      -- pretty-printing should use local names where available
      prettyPrintNames00 = currentAndExternalNames0
  pure (parseNames00, prettyPrintNames00)

-- {IO} ()
ioUnit :: Ord v => a -> Type v a
ioUnit a = Type.effect a [Type.ref a ioReference] (Type.ref a DD.unitRef)

-- '{IO} ()
nullaryMain :: Ord v => a -> Type v a
nullaryMain a = Type.arrow a (Type.ref a DD.unitRef) (ioUnit a)

mainTypes :: Ord v => a -> [Type v a]
mainTypes a = [nullaryMain a]

-- Given a typechecked file with a main function called `mainName`
-- of the type `'{IO} ()`, adds an extra binding which
-- forces the `main` function.
--
-- If that function doesn't exist in the typechecked file, the
-- codebase is consulted.
addRunMain
  :: (Monad m, Var v)
  => String
  -> Maybe (TypecheckedUnisonFile v Ann)
  -> Action' m v (Maybe (TypecheckedUnisonFile v Ann))
addRunMain mainName Nothing = do
  parseNames0 <- basicParseNames0
  case HQ.fromString mainName of
    Nothing -> pure Nothing
    Just hq -> do
      -- note: not allowing historical search
      let refs = Names3.lookupHQTerm hq (Names3.Names parseNames0 mempty)
      let a = External
      case toList refs of
        [] -> pure Nothing
        [Referent.Ref ref] -> do
          typ <- eval $ LoadTypeOfTerm ref
          case typ of
            Just typ | Typechecker.isSubtype typ (nullaryMain a) -> do
              let runMain = DD.forceTerm a a (Term.ref a ref)
              let v = Var.named (HQ.toText hq)
              pure . Just $ UF.typecheckedUnisonFile mempty mempty [[(v, runMain, typ)]] mempty
            _ -> pure Nothing
        _ -> pure Nothing
addRunMain mainName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == mainName) . view _1) components
  case mainComponent of
    [(v, tm, ty)] -> pure $ let
      v2 = Var.freshIn (Set.fromList [v]) v
      a = ABT.annotation tm
      in
      if Typechecker.isSubtype ty (nullaryMain a) then Just $ let
        runMain = DD.forceTerm a a (Term.var a v)
        in UF.typecheckedUnisonFile
             (UF.dataDeclarations' uf)
             (UF.effectDeclarations' uf)
             (UF.topLevelComponents' uf <> [[(v2, runMain, nullaryMain a)]])
             (UF.watchComponents uf)
      else Nothing
    _ -> addRunMain mainName Nothing

executePPE
  :: (Var v, Monad m)
  => TypecheckedUnisonFile v a
  -> Action' m v PPE.PrettyPrintEnv
executePPE unisonFile =
  -- voodoo
  prettyPrintEnv =<<
    makeShadowedPrintNamesFromLabeled
      (UF.termSignatureExternalLabeledDependencies unisonFile)
      (UF.typecheckedToNames0 unisonFile)

loadTypeOfTerm :: Referent -> Action m i v (Maybe (Type v Ann))
loadTypeOfTerm (Referent.Ref r) = eval $ LoadTypeOfTerm r
loadTypeOfTerm (Referent.Con (Reference.DerivedId r) cid _) = do
  decl <- eval $ LoadType r
  case decl of
    Just (either DD.toDataDecl id -> dd) -> pure $ DD.typeOfConstructor dd cid
    Nothing -> pure Nothing
loadTypeOfTerm (Referent.Con r cid _) = error $
  reportBug "924628772" "Attempt to load a type declaration which is a builtin!"
