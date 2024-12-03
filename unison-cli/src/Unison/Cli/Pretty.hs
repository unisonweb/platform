{-# LANGUAGE MagicHash #-}

-- | Small combinators that pretty-print small types in a canonical way for human consumption, such as hashes, file
-- paths, and project names.
module Unison.Cli.Pretty
  ( displayBranchHash,
    prettyAbsolute,
    prettyProjectPath,
    prettyBranchRelativePath,
    prettyBase32Hex#,
    prettyBase32Hex,
    prettyBranchId,
    prettyCausalHash,
    prettyDeclPair,
    prettyDeclTriple,
    prettyFilePath,
    prettyHash,
    prettyHash32,
    prettyHumanReadableTime,
    prettyLabeledDependencies,
    prettyPath,
    prettyPath',
    prettyMergeSource,
    prettyMergeSourceOrTarget,
    prettyProjectAndBranchName,
    prettyProjectBranchName,
    prettyProjectName,
    prettyProjectNameSlash,
    prettyNamespaceKey,
    prettyReadRemoteNamespace,
    prettyReadRemoteNamespaceWith,
    prettyRelative,
    prettyRemoteBranchInfo,
    prettyRepoInfo,
    prettySCH,
    prettySemver,
    prettySharePath,
    prettyShareURI,
    prettySlashProjectBranchName,
    prettyTerm,
    prettyTermName,
    prettyType,
    prettyTypeName,
    prettyTypeResultHeader',
    prettyTypeResultHeaderFull',
    prettyURI,
    prettyUnisonFile,
    prettyWhichBranchEmpty,
    prettyWriteRemoteNamespace,
    shareOrigin,
    unsafePrettyTermResultSigFull',
  )
where

import Control.Lens hiding (at)
import Control.Monad.Writer (Writer, runWriter)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Data.Time.Format.Human (HumanTimeLocale (..), defaultHumanTimeLocale, humanReadableTimeI18N')
import Network.URI (URI)
import Network.URI qualified as URI
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Util.Base32Hex (Base32Hex)
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Cli.MergeTypes (MergeSource (..), MergeSourceOrTarget (..))
import Unison.Cli.Share.Projects.Types qualified as Share
import Unison.Codebase.Editor.DisplayObject (DisplayObject (BuiltinObject, MissingObject, UserObject))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.RemoteRepo
  ( ReadRemoteNamespace (..),
  )
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath)
import Unison.Core.Project (ProjectBranchName)
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectName, Semver (..))
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Server.SearchResultPrime qualified as SR'
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Sync.Types qualified as Share
import Unison.Syntax.DeclPrinter (AccessorName)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.HashQualified qualified as HQ (unsafeFromVar)
import Unison.Syntax.Name qualified as Name (unsafeParseVar)
import Unison.Syntax.NamePrinter (SyntaxText, prettyHashQualified, styleHashQualified')
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as P
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK

type Pretty = P.Pretty P.ColorText

prettyURI :: URI -> Pretty
prettyURI = P.bold . P.blue . P.shown

prettyShareURI :: URI -> Pretty
prettyShareURI host
  | URI.uriToString id host "" == "https://api.unison-lang.org" = P.bold (P.blue "Unison Share")
  | otherwise = P.bold (P.blue (P.shown host))

prettyReadRemoteNamespace :: ReadRemoteNamespace Share.RemoteProjectBranch -> Pretty
prettyReadRemoteNamespace =
  prettyReadRemoteNamespaceWith \remoteProjectBranch ->
    into @Text (ProjectAndBranch (remoteProjectBranch ^. #projectName) (remoteProjectBranch ^. #branchName))

prettyReadRemoteNamespaceWith :: (a -> Text) -> ReadRemoteNamespace a -> Pretty
prettyReadRemoteNamespaceWith printProject =
  P.group . P.blue . P.text . RemoteRepo.printReadRemoteNamespace printProject

prettyWriteRemoteNamespace :: (ProjectAndBranch ProjectName ProjectBranchName) -> Pretty
prettyWriteRemoteNamespace =
  P.group . P.blue . P.text . RemoteRepo.printWriteRemoteNamespace

shareOrigin :: Text
shareOrigin = "https://share.unison-lang.org"

prettyRepoInfo :: Share.RepoInfo -> Pretty
prettyRepoInfo (Share.RepoInfo repoInfo) =
  P.blue (P.text repoInfo)

prettySharePath :: Share.Path -> Pretty
prettySharePath =
  prettyRelative
    . Path.Relative
    . Path.fromList
    . coerce @[Text] @[NameSegment]
    . toList
    . Share.pathSegments

prettyFilePath :: FilePath -> Pretty
prettyFilePath fp =
  P.blue (P.string fp)

prettyPath :: Path.Path -> Pretty
prettyPath path =
  if path == Path.empty
    then "the current namespace"
    else P.blue (P.shown path)

prettyPath' :: Path.Path' -> Pretty
prettyPath' p' =
  if Path.isCurrentPath p'
    then "the current namespace"
    else P.blue (P.shown p')

prettyNamespaceKey :: Either ProjectPath (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) -> Pretty
prettyNamespaceKey = \case
  Left path -> prettyProjectPath path
  Right (ProjectAndBranch project branch) ->
    prettyProjectAndBranchName (ProjectAndBranch (project ^. #name) (branch ^. #name))

prettyBranchId :: Input.AbsBranchId -> Pretty
prettyBranchId = \case
  Input.BranchAtSCH sch -> prettySCH sch
  Input.BranchAtPath absPath -> prettyAbsolute $ absPath
  Input.BranchAtProjectPath pp -> prettyProjectPath pp

prettyRelative :: Path.Relative -> Pretty
prettyRelative = P.blue . P.shown

prettyAbsolute :: Path.Absolute -> Pretty
prettyAbsolute = P.blue . P.shown

prettyProjectPath :: PP.ProjectPath -> Pretty
prettyProjectPath (PP.ProjectPath project branch path) =
  prettyProjectAndBranchName (ProjectAndBranch project.name branch.name)
    <>
    -- Only show the path if it's not the root
    Monoid.whenM (path /= Path.absoluteEmpty) (P.cyan (":" <> P.shown path))

prettySCH :: (IsString s) => ShortCausalHash -> P.Pretty s
prettySCH hash = P.group $ "#" <> P.text (SCH.toText hash)

prettyCausalHash :: (IsString s) => CausalHash -> P.Pretty s
prettyCausalHash hash = P.group $ "#" <> P.text (Hash.toBase32HexText . unCausalHash $ hash)

prettyBase32Hex :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32Hex = P.text . Base32Hex.toText

prettyBase32Hex# :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32Hex# b = P.group $ "#" <> prettyBase32Hex b

prettyHash :: (IsString s) => Hash.Hash -> P.Pretty s
prettyHash = prettyBase32Hex# . Hash.toBase32Hex

prettyHash32 :: (IsString s) => Hash32 -> P.Pretty s
prettyHash32 = prettyBase32Hex# . Hash32.toBase32Hex

prettyMergeSource :: MergeSource -> Pretty
prettyMergeSource = \case
  MergeSource'LocalProjectBranch branch -> prettyProjectAndBranchName branch
  MergeSource'RemoteProjectBranch branch -> "remote " <> prettyProjectAndBranchName branch
  MergeSource'RemoteLooseCode info -> prettyReadRemoteNamespace (ReadShare'LooseCode info)

prettyMergeSourceOrTarget :: MergeSourceOrTarget -> Pretty
prettyMergeSourceOrTarget = \case
  MergeSourceOrTarget'Target alice -> prettyProjectAndBranchName alice
  MergeSourceOrTarget'Source bob -> prettyMergeSource bob

prettyProjectName :: ProjectName -> Pretty
prettyProjectName =
  P.green . P.text . into @Text

-- | 'prettyProjectName' with a trailing slash.
prettyProjectNameSlash :: ProjectName -> Pretty
prettyProjectNameSlash project =
  P.group (prettyProjectName project <> P.hiBlack "/")

prettyProjectBranchName :: ProjectBranchName -> Pretty
prettyProjectBranchName =
  P.blue . P.text . into @Text

prettySemver :: Semver -> Pretty
prettySemver (Semver x y z) =
  P.group (P.num x <> "." <> P.num y <> "." <> P.num z)

-- | Like 'prettyProjectBranchName', but with a leading forward slash. This is used in some outputs to
-- encourage/advertise an unambiguous syntax for project branches, as there's an ambiguity with single-segment relative
-- paths.
--
-- Not all project branches are printed such: for example, when listing all branches of a project, we probably don't
-- need or want to prefix every one with a forward slash.
prettySlashProjectBranchName :: ProjectBranchName -> Pretty
prettySlashProjectBranchName branch =
  P.group (P.hiBlack "/" <> prettyProjectBranchName branch)

prettyProjectAndBranchName :: ProjectAndBranch ProjectName ProjectBranchName -> Pretty
prettyProjectAndBranchName (ProjectAndBranch project branch) =
  P.group (prettyProjectName project <> P.hiBlack "/" <> prettyProjectBranchName branch)

prettyBranchRelativePath :: BranchRelativePath -> Pretty
prettyBranchRelativePath = P.blue . P.text . into @Text

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms#0
-- Optional.None, Maybe.Nothing : Maybe a
unsafePrettyTermResultSigFull' ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  SR'.TermResult' v a ->
  Pretty
unsafePrettyTermResultSigFull' ppe = \case
  SR'.TermResult' hq (Just typ) r aliases ->
    P.lines
      [ P.hiBlack "-- " <> greyHash (HQ.fromReferent r),
        P.group $
          P.commas (fmap greyHash $ hq : map HQ'.toHQ (toList aliases))
            <> " : "
            <> P.syntaxToColor (TypePrinter.prettySyntax ppe typ),
        mempty
      ]
  _ -> error "Don't pass Nothing"
  where
    greyHash = styleHashQualified' id P.hiBlack

prettyTypeResultHeader' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeader' (SR'.TypeResult' name dt r _aliases) =
  prettyDeclTriple (name, r, dt)

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms
-- type Optional
-- type Maybe
prettyTypeResultHeaderFull' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeaderFull' (SR'.TypeResult' name dt r aliases) =
  P.lines stuff <> P.newline
  where
    stuff =
      (P.hiBlack "-- " <> greyHash (HQ.fromReference r))
        : fmap
          (\name -> prettyDeclTriple (name, r, dt))
          (name : map HQ'.toHQ (toList aliases))
      where
        greyHash = styleHashQualified' id P.hiBlack

prettyDeclTriple ::
  (Var v) =>
  (HQ.HashQualified Name, Reference.Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
  BuiltinObject _ -> P.hiBlack "builtin " <> P.hiBlue "type " <> P.blue (P.syntaxToColor $ prettyHashQualified name)
  MissingObject _ -> mempty -- these need to be handled elsewhere
  UserObject decl -> P.syntaxToColor $ DeclPrinter.prettyDeclHeader name decl

prettyDeclPair ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  (Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclPair ppe (r, dt) = prettyDeclTriple (PPE.typeName ppe r, r, dt)

prettyTermName :: PPE.PrettyPrintEnv -> Referent -> Pretty
prettyTermName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.termName ppe r)

prettyTypeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
prettyTypeName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.typeName ppe r)

-- | Pretty-print a 'WhichBranchEmpty'.
prettyWhichBranchEmpty :: WhichBranchEmpty -> Pretty
prettyWhichBranchEmpty = \case
  WhichBranchEmptyHash hash -> P.shown hash
  WhichBranchEmptyPath pp -> prettyProjectPath pp

-- | Displays a full, non-truncated Branch.CausalHash to a string, e.g. #abcdef
displayBranchHash :: CausalHash -> Text
displayBranchHash = ("#" <>) . Hash.toBase32HexText . unCausalHash

prettyHumanReadableTime :: UTCTime -> UTCTime -> Pretty
prettyHumanReadableTime now time =
  P.green . P.string $ humanReadableTimeI18N' terseTimeLocale now time
  where
    terseTimeLocale =
      defaultHumanTimeLocale
        { justNow = "now",
          secondsAgo = \f -> (++ " secs" ++ dir f),
          oneMinuteAgo = \f -> "a min" ++ dir f,
          minutesAgo = \f -> (++ " mins" ++ dir f),
          oneHourAgo = \f -> "an hour" ++ dir f,
          aboutHoursAgo = \f x -> "about " ++ x ++ " hours" ++ dir f,
          at = \_ t -> t,
          daysAgo = \f -> (++ " days" ++ dir f),
          weekAgo = \f -> (++ " week" ++ dir f),
          weeksAgo = \f -> (++ " weeks" ++ dir f),
          onYear = \dt -> dt,
          dayOfWeekFmt = "%A, %-l:%M%p",
          thisYearFmt = "%b %e",
          prevYearFmt = "%b %e, %Y"
        }

    dir True = " from now"
    dir False = " ago"

prettyRemoteBranchInfo :: (URI, ProjectName, ProjectBranchName) -> Pretty
prettyRemoteBranchInfo (host, remoteProject, remoteBranch) =
  -- Special-case Unison Share since we know its project branch URLs
  if URI.uriToString id host "" == "https://api.unison-lang.org"
    then
      P.group $
        "https://share.unison-lang.org/"
          <> prettyProjectName remoteProject
          <> "/code/"
          <> prettyProjectBranchName remoteBranch
    else
      prettyProjectAndBranchName (ProjectAndBranch remoteProject remoteBranch)
        <> " on "
        <> P.shown host

prettyLabeledDependencies :: PPE.PrettyPrintEnv -> Set LabeledDependency -> Pretty
prettyLabeledDependencies ppe lds =
  P.syntaxToColor (P.sep ", " (ld <$> toList lds))
  where
    ld = \case
      LD.TermReferent r -> prettyHashQualified (PPE.termNameOrHashOnly ppe r)
      LD.TypeReference r -> "type " <> prettyHashQualified (PPE.typeNameOrHashOnly ppe r)

prettyUnisonFile :: forall v a. (Var v, Ord a) => PPED.PrettyPrintEnvDecl -> UF.UnisonFile v a -> P.Pretty P.ColorText
prettyUnisonFile ppe uf@(UF.UnisonFileId datas effects terms watches) =
  P.sep "\n\n" (map snd . sortOn fst $ prettyEffects <> prettyDatas <> catMaybes prettyTerms <> prettyWatches)
  where
    prettyEffects = map prettyEffectDecl (Map.toList effects)
    (prettyDatas, accessorNames) = runWriter $ traverse prettyDataDecl (Map.toList datas)
    prettyTerms = Map.foldrWithKey (\k v -> (prettyTerm accessorNames k v :)) [] terms
    prettyWatches = Map.toList watches >>= \(wk, tms) -> map (prettyWatch . (wk,)) tms

    prettyEffectDecl :: (v, (Reference.Id, DD.EffectDeclaration v a)) -> (a, P.Pretty P.ColorText)
    prettyEffectDecl (n, (r, et)) =
      (DD.annotation . DD.toDataDecl $ et, st $ DeclPrinter.prettyDecl ppe' (rd r) (hqv n) (Left et))
    prettyDataDecl :: (v, (Reference.Id, DD.DataDeclaration v a)) -> Writer (Set AccessorName) (a, P.Pretty P.ColorText)
    prettyDataDecl (n, (r, dt)) =
      (DD.annotation dt,) . st <$> DeclPrinter.prettyDeclW ppe' (rd r) (hqv n) (Right dt)
    prettyTerm :: Set AccessorName -> v -> (a, Term v a) -> Maybe (a, P.Pretty P.ColorText)
    prettyTerm skip n (a, tm) =
      if traceMember isMember then Nothing else Just (a, pb hq tm)
      where
        traceMember =
          if Debug.shouldDebug Debug.Update
            then trace (show hq ++ " -> " ++ if isMember then "skip" else "print")
            else id
        isMember = Set.member (Name.unsafeParseVar n) skip
        hq = hqv n
    prettyWatch :: (String, (v, a, Term v a)) -> (a, P.Pretty P.ColorText)
    prettyWatch (wk, (n, a, tm)) = (a, go wk n tm)
      where
        go wk v tm = case wk of
          WK.RegularWatch
            | Var.UnnamedWatch _ _ <- Var.typeOf v ->
                "> " <> P.indentNAfterNewline 2 (TermPrinter.pretty sppe tm)
          WK.RegularWatch -> "> " <> pb (hqv v) tm
          WK.TestWatch -> "test> " <> st (TermPrinter.prettyBindingWithoutTypeSignature sppe (hqv v) tm)
          w -> P.string w <> "> " <> pb (hqv v) tm
    st = P.syntaxToColor
    sppe = PPED.suffixifiedPPE ppe'
    pb v tm = st $ TermPrinter.prettyBinding sppe v tm
    ppe' = PPED.PrettyPrintEnvDecl dppe dppe `PPED.addFallback` ppe
    dppe = PPE.makePPE (PPE.namer (UF.toNames uf)) PPE.dontSuffixify
    rd = Reference.DerivedId
    hqv v = HQ.unsafeFromVar v

prettyTerm ::
  PPED.PrettyPrintEnvDecl ->
  Bool {- whether we're printing to a source-file or not. -} ->
  Bool {- Whether the term is a test -} ->
  (HQ.HashQualified Name, Reference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  P.Pretty SyntaxText
prettyTerm pped isSourceFile isTest (n, r, dt) =
  case dt of
    MissingObject r -> missingDefinitionMsg n r
    BuiltinObject typ ->
      commentBuiltin $
        P.hang
          ("builtin " <> prettyHashQualified n <> " :")
          (TypePrinter.prettySyntax (ppeBody n r) typ)
    UserObject tm ->
      if isTest
        then WK.TestWatch <> "> " <> TermPrinter.prettyBindingWithoutTypeSignature (ppeBody n r) n tm
        else TermPrinter.prettyBinding (ppeBody n r) n tm
  where
    commentBuiltin txt =
      if isSourceFile
        then P.indent "-- " txt
        else txt
    ppeBody n r = PPE.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPE pped r

prettyType :: PPED.PrettyPrintEnvDecl -> (HQ.HashQualified Name, Reference, DisplayObject () (DD.Decl Symbol Ann)) -> P.Pretty SyntaxText
prettyType pped (n, r, dt) =
  case dt of
    MissingObject r -> missingDefinitionMsg n r
    BuiltinObject _ -> builtin n
    UserObject decl -> DeclPrinter.prettyDecl (PPED.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPEDecl pped r) r n decl
  where
    builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."

missingDefinitionMsg :: HQ.HashQualified Name -> ShortHash -> P.Pretty SyntaxText
missingDefinitionMsg n r =
  P.wrap
    ( "-- The name "
        <> prettyHashQualified n
        <> " is assigned to the "
        <> "reference "
        <> fromString (show r ++ ",")
        <> "which is missing from the codebase."
    )
    <> P.newline
    <> tip "You might need to repair the codebase manually."
  where
    tip :: P.Pretty SyntaxText -> P.Pretty SyntaxText
    tip s = P.column2 [("Tip:", P.wrap s)]
