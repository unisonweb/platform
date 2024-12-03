{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Unison.Runtime.IOSource where

import Control.Lens (_2)
import Control.Monad.Morph (hoist)
import Data.List (elemIndex, genericIndex)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Text.RawString.QQ (r)
import Unison.Builtin qualified as Builtin
import Unison.Codebase.CodeLookup (CodeLookup (..))
import Unison.Codebase.CodeLookup.Util qualified as CL
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.ConstructorId qualified as DD
import Unison.FileParsers (ShouldUseTndr (..), computeTypecheckingEnvironment, synthesizeFile)
import Unison.Parser.Ann (Ann (..))
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrintError qualified as PrintError
import Unison.Reference qualified as R
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty qualified as Pretty
import Unison.Var qualified as Var

debug :: Bool
debug = False

parsingEnv :: Parser.ParsingEnv Identity
parsingEnv =
  Parser.ParsingEnv
    { uniqueNames = mempty,
      uniqueTypeGuid = \_ -> pure Nothing,
      names = Builtin.names,
      maybeNamespace = Nothing,
      localNamespacePrefixedTypesAndConstructors = mempty
    }

typecheckingEnv :: Typechecker.Env Symbol Ann
typecheckingEnv =
  runIdentity do
    computeTypecheckingEnvironment
      (ShouldUseTndr'Yes parsingEnv)
      []
      (\_ -> pure (External <$ Builtin.typeLookup))
      parsedFile

parsedFile :: UF.UnisonFile Symbol Ann
parsedFile =
  case runIdentity (Parsers.parseFile "<IO.u builtin>" sourceString parsingEnv) of
    Left err -> error (Pretty.toAnsiUnbroken (PrintError.prettyParseError sourceString err))
    Right file -> file

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile =
  let x = typecheckedFile'
   in if debug then trace ("IOSource.typecheckedFile = " ++ show x) x else x

typecheckedFile' :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile' =
  case synthesizeFile typecheckingEnv parsedFile of
    Result.Result notes Nothing -> error (showNotes sourceString ppEnv notes)
    Result.Result _ (Just file) -> file

typecheckedFileTerms :: Map.Map Symbol R.Reference
typecheckedFileTerms = view _2 <$> UF.hashTerms typecheckedFile

termNamed :: String -> R.Reference
termNamed s =
  fromMaybe (error $ "No builtin term called: " <> s) $
    Map.lookup (Var.nameds s) typecheckedFileTerms

codeLookup :: CodeLookup Symbol Identity Ann
codeLookup = CL.fromTypecheckedUnisonFile typecheckedFile

codeLookupM :: (Applicative m) => CodeLookup Symbol m Ann
codeLookupM = hoist (pure . runIdentity) codeLookup

typeNamedId :: String -> R.Id
typeNamedId s =
  case Map.lookup (Var.nameds s) (UF.dataDeclarationsId' typecheckedFile) of
    Nothing -> error $ "No builtin type called: " <> s
    Just (r, _) -> r

typeNamed :: String -> R.Reference
typeNamed = R.DerivedId . typeNamedId

abilityNamedId :: String -> R.Id
abilityNamedId s =
  case Map.lookup (Var.nameds s) (UF.effectDeclarationsId' typecheckedFile) of
    Nothing -> error $ "No builtin ability called: " <> s
    Just (r, _) -> r

eitherReference,
  optionReference,
  isTestReference,
  isPropagatedReference ::
    R.Reference
eitherReference = typeNamed "Either"
optionReference = typeNamed "Optional"
isTestReference = typeNamed "IsTest"
isPropagatedReference = typeNamed "IsPropagated"

isTest :: (R.Reference, R.Reference)
isTest = (isTestReference, termNamed "metadata.isTest")

isIOTest :: (R.Reference, R.Reference)
isIOTest = (isTestReference, termNamed "metadata.isIOTest")

isPropagatedValue :: R.Reference
isPropagatedValue = termNamed "metadata.isPropagated"

eitherLeftId, eitherRightId, someId, noneId :: DD.ConstructorId
eitherLeftId = constructorNamed eitherReference "Either.Left"
eitherRightId = constructorNamed eitherReference "Either.Right"
someId = constructorNamed optionReference "Optional.Some"
noneId = constructorNamed optionReference "Optional.None"

authorRef, guidRef, copyrightHolderRef :: R.Reference
authorRef = typeNamed "Author"
guidRef = typeNamed "GUID"
copyrightHolderRef = typeNamed "CopyrightHolder"

doc2Ref :: R.Reference
doc2Ref = typeNamed "Doc2"

doc2SpecialFormRef = typeNamed "Doc2.SpecialForm"

doc2TermRef = typeNamed "Doc2.Term"

prettyRef = typeNamed "Pretty"

prettyAnnotatedRef = typeNamed "Pretty.Annotated"

ansiColorRef = typeNamed "ANSI.Color"

consoleTextRef = typeNamed "ConsoleText"

pattern Doc2Ref <- ((== doc2Ref) -> True)

doc2WordId = constructorNamed doc2Ref "Doc2.Word"

doc2CodeId = constructorNamed doc2Ref "Doc2.Code"

doc2CodeBlockId = constructorNamed doc2Ref "Doc2.CodeBlock"

doc2BoldId = constructorNamed doc2Ref "Doc2.Bold"

doc2ItalicId = constructorNamed doc2Ref "Doc2.Italic"

doc2StrikethroughId = constructorNamed doc2Ref "Doc2.Strikethrough"

doc2StyleId = constructorNamed doc2Ref "Doc2.Style"

doc2AnchorId = constructorNamed doc2Ref "Doc2.Anchor"

doc2BlockquoteId = constructorNamed doc2Ref "Doc2.Blockquote"

doc2BlanklineId = constructorNamed doc2Ref "Doc2.Blankline"

doc2LinebreakId = constructorNamed doc2Ref "Doc2.Linebreak"

doc2SectionBreakId = constructorNamed doc2Ref "Doc2.SectionBreak"

doc2TooltipId = constructorNamed doc2Ref "Doc2.Tooltip"

doc2AsideId = constructorNamed doc2Ref "Doc2.Aside"

doc2CalloutId = constructorNamed doc2Ref "Doc2.Callout"

doc2TableId = constructorNamed doc2Ref "Doc2.Table"

doc2FoldedId = constructorNamed doc2Ref "Doc2.Folded"

doc2ParagraphId = constructorNamed doc2Ref "Doc2.Paragraph"

doc2BulletedListId = constructorNamed doc2Ref "Doc2.BulletedList"

doc2NumberedListId = constructorNamed doc2Ref "Doc2.NumberedList"

doc2SectionId = constructorNamed doc2Ref "Doc2.Section"

doc2NamedLinkId = constructorNamed doc2Ref "Doc2.NamedLink"

doc2ImageId = constructorNamed doc2Ref "Doc2.Image"

doc2SpecialId = constructorNamed doc2Ref "Doc2.Special"

doc2JoinId = constructorNamed doc2Ref "Doc2.Join"

doc2UntitledSectionId = constructorNamed doc2Ref "Doc2.UntitledSection"

doc2ColumnId = constructorNamed doc2Ref "Doc2.Column"

doc2GroupId = constructorNamed doc2Ref "Doc2.Group"

doc2MediaSourceRef :: R.Reference
doc2MediaSourceRef = typeNamed "Doc2.MediaSource"

pattern Doc2MediaSourceRef <- ((== doc2MediaSourceRef) -> True)

doc2VideoRef :: R.Reference
doc2VideoRef = typeNamed "Doc2.Video"

pattern Doc2VideoRef <- ((== doc2VideoRef) -> True)

doc2FrontMatterRef :: R.Reference
doc2FrontMatterRef = typeNamed "Doc2.FrontMatter"

pattern Doc2FrontMatterRef <- ((== doc2FrontMatterRef) -> True)

doc2LaTeXInlineRef :: R.Reference
doc2LaTeXInlineRef = typeNamed "Doc2.LaTeXInline"

pattern Doc2LaTeXInlineRef <- ((== doc2LaTeXInlineRef) -> True)

doc2SvgRef :: R.Reference
doc2SvgRef = typeNamed "Doc2.Svg"

pattern Doc2SvgRef <- ((== doc2SvgRef) -> True)

pattern Doc2Word txt <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2WordId -> True))) (Term.Text' txt)

pattern Doc2Code d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2CodeId -> True))) d

pattern Doc2CodeBlock lang d <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2CodeBlockId -> True))) [Term.Text' lang, d]

pattern Doc2Bold d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2BoldId -> True))) d

pattern Doc2Italic d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2ItalicId -> True))) d

pattern Doc2Strikethrough d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2StrikethroughId -> True))) d

pattern Doc2Style s d <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2StyleId -> True))) [Term.Text' s, d]

pattern Doc2Anchor id d <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2AnchorId -> True))) [Term.Text' id, d]

pattern Doc2Blockquote d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2BlockquoteId -> True))) d

pattern Doc2Blankline <- Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2BlanklineId -> True))

pattern Doc2Linebreak <- Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2LinebreakId -> True))

pattern Doc2SectionBreak <- Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2SectionBreakId -> True))

pattern Doc2Tooltip d tip <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2TooltipId -> True))) [d, tip]

pattern Doc2Aside d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2AsideId -> True))) d

pattern Doc2Callout icon d <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2CalloutId -> True))) [icon, d]

pattern Doc2Table ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2TableId -> True))) (Term.List' (toList -> ds))

pattern Doc2Folded isFolded d d2 <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2FoldedId -> True))) [Term.Boolean' isFolded, d, d2]

pattern Doc2Paragraph ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2ParagraphId -> True))) (Term.List' (toList -> ds))

pattern Doc2BulletedList ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2BulletedListId -> True))) (Term.List' (toList -> ds))

pattern Doc2NumberedList n ds <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2NumberedListId -> True))) [Term.Nat' n, Term.List' (toList -> ds)]

pattern Doc2Section title ds <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2SectionId -> True))) [title, Term.List' (toList -> ds)]

pattern Doc2NamedLink name dest <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2NamedLinkId -> True))) [name, dest]

pattern Doc2Image alt link caption <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2ImageId -> True))) [alt, link, caption]

pattern Doc2Special sf <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2SpecialId -> True))) sf

pattern Doc2Join ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2JoinId -> True))) (Term.List' (toList -> ds))

pattern Doc2UntitledSection ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2UntitledSectionId -> True))) (Term.List' (toList -> ds))

pattern Doc2Column ds <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2ColumnId -> True))) (Term.List' (toList -> ds))

pattern Doc2Group d <- Term.App' (Term.Constructor' (ConstructorReference Doc2Ref ((==) doc2GroupId -> True))) d

pattern Doc2SpecialFormRef <- ((== doc2SpecialFormRef) -> True)

doc2SpecialFormSourceId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Source"

doc2SpecialFormFoldedSourceId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.FoldedSource"

doc2SpecialFormExampleId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Example"

doc2SpecialFormExampleBlockId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.ExampleBlock"

doc2SpecialFormLinkId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Link"

doc2SpecialFormSignatureId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Signature"

doc2SpecialFormSignatureInlineId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.SignatureInline"

doc2SpecialFormEvalId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Eval"

doc2SpecialFormEvalInlineId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.EvalInline"

doc2SpecialFormEmbedId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.Embed"

doc2SpecialFormEmbedInlineId = constructorNamed doc2SpecialFormRef "Doc2.SpecialForm.EmbedInline"

pattern Doc2SpecialFormSource tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormSourceId -> True))) tm

pattern Doc2SpecialFormFoldedSource tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormFoldedSourceId -> True))) tm

pattern Doc2SpecialFormExample n tm <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormExampleId -> True))) [Term.Nat' n, tm]

pattern Doc2SpecialFormExampleBlock n tm <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormExampleBlockId -> True))) [Term.Nat' n, tm]

pattern Doc2SpecialFormLink tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormLinkId -> True))) tm

pattern Doc2SpecialFormSignature tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormSignatureId -> True))) tm

pattern Doc2SpecialFormSignatureInline tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormSignatureInlineId -> True))) tm

pattern Doc2SpecialFormEval tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormEvalId -> True))) tm

pattern Doc2SpecialFormEvalInline tm <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormEvalInlineId -> True))) tm

pattern Doc2SpecialFormEmbed any <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormEmbedId -> True))) any

pattern Doc2SpecialFormEmbedInline any <- Term.App' (Term.Constructor' (ConstructorReference Doc2SpecialFormRef ((==) doc2SpecialFormEmbedInlineId -> True))) any

pattern Doc2MediaSource src mimeType <- Term.Apps' (Term.Constructor' (ConstructorReference Doc2MediaSourceRef _)) [src, mimeType]

pattern Doc2SpecialFormEmbedVideo sources config <- Doc2SpecialFormEmbed (Term.App' _ (Term.Apps' (Term.Constructor' (ConstructorReference Doc2VideoRef _)) [Term.List' (toList -> sources), Term.List' (toList -> config)]))

pattern Doc2SpecialFormEmbedFrontMatter frontMatter <- Doc2SpecialFormEmbed (Term.App' _ (Term.App' (Term.Constructor' (ConstructorReference Doc2FrontMatterRef _)) (Term.List' (toList -> frontMatter))))

pattern Doc2SpecialFormEmbedLaTeXInline latex <- Doc2SpecialFormEmbedInline (Term.App' _ (Term.App' (Term.Constructor' (ConstructorReference Doc2LaTeXInlineRef _)) (Term.Text' latex)))

pattern Doc2SpecialFormEmbedSvg svg <- Doc2SpecialFormEmbed (Term.App' _ (Term.App' (Term.Constructor' (ConstructorReference Doc2SvgRef _)) (Term.Text' svg)))

-- pulls out `vs body` in `Doc2.Term (Any '(vs -> body))`, where
-- vs can be any number of parameters
pattern Doc2Example vs body <- Term.App' _term (Term.App' _any (Term.LamNamed' _ (Term.LamsNamedOpt' vs body)))

-- pulls out `body` in `Doc2.Term (Any 'body)`
pattern Doc2Term body <- Term.App' _term (Term.App' _any (Term.LamNamed' _ body))

pattern Doc2TermRef <- ((== doc2TermRef) -> True)

pattern PrettyAnnotatedRef <- ((== prettyAnnotatedRef) -> True)

prettyEmptyId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Empty"

prettyGroupId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Group"

prettyLitId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Lit"

prettyWrapId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Wrap"

prettyOrElseId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.OrElse"

prettyIndentId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Indent"

prettyAppendId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Append"

prettyTableId = constructorNamed prettyAnnotatedRef "Pretty.Annotated.Table"

pattern PrettyEmpty <- Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyEmptyId -> True))

pattern PrettyGroup ann tm <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyGroupId -> True))) [ann, tm]

pattern PrettyLit ann tm <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyLitId -> True))) [ann, tm]

pattern PrettyWrap ann tm <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyWrapId -> True))) [ann, tm]

pattern PrettyIndent ann i0 i1 tm <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyIndentId -> True))) [ann, i0, i1, tm]

pattern PrettyOrElse ann p1 p2 <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyOrElseId -> True))) [ann, p1, p2]

pattern PrettyTable ann rows <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyTableId -> True))) [ann, Term.List' rows]

pattern PrettyAppend ann tms <- Term.Apps' (Term.Constructor' (ConstructorReference PrettyAnnotatedRef ((==) prettyAppendId -> True))) [ann, Term.List' tms]

pattern PrettyRef <- ((== prettyRef) -> True)

prettyGetRef = termNamed "Pretty.get"

doc2FormatConsoleRef = termNamed "syntax.docFormatConsole"

pattern AnsiColorRef <- ((== ansiColorRef) -> True)

( ansiColorBlackId,
  ansiColorRedId,
  ansiColorGreenId,
  ansiColorYellowId,
  ansiColorBlueId,
  ansiColorMagentaId,
  ansiColorCyanId,
  ansiColorWhiteId,
  ansiColorBrightBlackId,
  ansiColorBrightRedId,
  ansiColorBrightGreenId,
  ansiColorBrightYellowId,
  ansiColorBrightBlueId,
  ansiColorBrightMagentaId,
  ansiColorBrightCyanId,
  ansiColorBrightWhiteId
  ) =
    ( ct "Black",
      ct "Red",
      ct "Green",
      ct "Yellow",
      ct "Blue",
      ct "Magenta",
      ct "Cyan",
      ct "White",
      ct "BrightBlack",
      ct "BrightRed",
      ct "BrightGreen",
      ct "BrightYellow",
      ct "BrightBlue",
      ct "BrightMagenta",
      ct "BrightCyan",
      ct "BrightWhite"
    )
    where
      ct :: Text -> DD.ConstructorId
      ct n = constructorNamed ansiColorRef ("ANSI.Color." <> n)

pattern AnsiColorBlack <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBlackId -> True))

pattern AnsiColorRed <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorRedId -> True))

pattern AnsiColorGreen <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorGreenId -> True))

pattern AnsiColorYellow <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorYellowId -> True))

pattern AnsiColorBlue <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBlueId -> True))

pattern AnsiColorMagenta <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorMagentaId -> True))

pattern AnsiColorCyan <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorCyanId -> True))

pattern AnsiColorWhite <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorWhiteId -> True))

pattern AnsiColorBrightBlack <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightBlackId -> True))

pattern AnsiColorBrightRed <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightRedId -> True))

pattern AnsiColorBrightGreen <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightGreenId -> True))

pattern AnsiColorBrightYellow <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightYellowId -> True))

pattern AnsiColorBrightBlue <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightBlueId -> True))

pattern AnsiColorBrightMagenta <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightMagentaId -> True))

pattern AnsiColorBrightCyan <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightCyanId -> True))

pattern AnsiColorBrightWhite <- Term.Constructor' (ConstructorReference AnsiColorRef ((==) ansiColorBrightWhiteId -> True))

pattern ConsoleTextRef <- ((== consoleTextRef) -> True)

consoleTextPlainId = constructorNamed consoleTextRef "ConsoleText.Plain"

consoleTextForegroundId = constructorNamed consoleTextRef "ConsoleText.Foreground"

consoleTextBackgroundId = constructorNamed consoleTextRef "ConsoleText.Background"

consoleTextBoldId = constructorNamed consoleTextRef "ConsoleText.Bold"

consoleTextUnderlineId = constructorNamed consoleTextRef "ConsoleText.Underline"

consoleTextInvertId = constructorNamed consoleTextRef "ConsoleText.Invert"

pattern ConsoleTextPlain txt <- Term.App' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextPlainId -> True))) txt

pattern ConsoleTextForeground color ct <- Term.Apps' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextForegroundId -> True))) [color, ct]

pattern ConsoleTextBackground color ct <- Term.Apps' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextBackgroundId -> True))) [color, ct]

pattern ConsoleTextBold ct <- Term.App' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextBoldId -> True))) ct

pattern ConsoleTextUnderline ct <- Term.App' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextUnderlineId -> True))) ct

pattern ConsoleTextInvert ct <- Term.App' (Term.Constructor' (ConstructorReference ConsoleTextRef ((==) consoleTextInvertId -> True))) ct

iarrayFromListRef :: R.Reference
iarrayFromListRef = termNamed "ImmutableArray.fromList"

ibarrayFromBytesRef :: R.Reference
ibarrayFromBytesRef = termNamed "ImmutableByteArray.fromBytes"

constructorNamed :: R.Reference -> Text -> DD.ConstructorId
constructorNamed ref name =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error $
        "There's a bug in the Unison runtime. Couldn't find type "
          <> show ref
    Just decl ->
      fromIntegral
        . fromMaybe
          ( error $
              "Unison runtime bug. The type "
                <> show ref
                <> " has no constructor named "
                <> show name
          )
        . elemIndex name
        . DD.constructorNames
        $ DD.asDataDecl decl

constructorName :: R.Reference -> DD.ConstructorId -> Text
constructorName ref cid =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error $
        "There's a bug in the Unison runtime. Couldn't find type "
          <> show ref
    Just decl -> genericIndex (DD.constructorNames $ DD.asDataDecl decl) cid

-- .. todo - fill in the rest of these

sourceString :: String
sourceString =
  Text.unpack source

source :: Text
source =
  fromString
    [r|

structural type Either a b = Left a | Right b

structural type Optional a = None | Some a

unique[b28d929d0a73d2c18eac86341a3bb9399f8550c11b5f35eabb2751e6803ccc20] type
  IsPropagated = IsPropagated

d1 Doc.++ d2 =
  use Doc2
  match (d1,d2) with
    (Join ds, Join ds2) -> Join (ds List.++ ds2)
    (Join ds, _) -> Join (List.snoc ds d2)
    (_, Join ds) -> Join (List.cons d1 ds)
    _ -> Join [d1,d2]

unique[q1905679b27a97a4098bc965574da880c1074183a2c55ff1d481619c7fb8a1e1] type
  Author = { guid : GUID, name : Text }

unique[ee1c051034fa0671ea66e7c708ba552003bd3cf657bd28bf0051f1f8cdfcba53] type
  CopyrightHolder = { guid : GUID, name : Text}

unique[bed6724af0d5f47f80cdea1b6023d35f120137ee0556e57154a9fc8b62fe5fed] type
  License = { copyrightHolders : [CopyrightHolder]
            , years : [Year]
            , licenseType : LicenseType }

-- Use `Doc` here to get nice text-wrapping when viewing
-- and to avoid needing to stick hard line breaks in the license
unique[d875fa1ea7ef3adf8e29417c6c8b01a1830c4c6bd10dcca9d4196388462e0b7a] type LicenseType = LicenseType Doc

unique[cb8469a1b41a63655062226556eaccf06129a2641af61fe7edef9c485c94a870] type GUID = GUID Bytes

-- Common era years
unique[u9ae6694152966cf1b0c1f4ad901a77e1acd7bbe16595fd27b07435ac45dab05] type Year = Year Nat

-- This is linked to definitions that are considered tests
unique[e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f] type
  IsTest = IsTest

-- Create references for these that can be used as metadata.
-- (Reminder: Metadata is references, not values.)
metadata.isTest = IsTest.IsTest
metadata.isPropagated = IsPropagated.IsPropagated

-- Built-ins

-- A newtype used when embedding term references in a Doc2
unique[fb488e55e66e2492c2946388e4e846450701db04] type Doc2.Term = Term Any

-- Media types for Doc2.Embed.
-- Somewhat modelled after:
--   https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source and
--   https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video

unique[ab9344724264495159ec7122d276a6358630403b6a5529e1e5d76bcf] type Doc2.MediaSource
  = { sourceUrl: Text, mimeType: Optional Text }

-- Used with MediaSource to embed videos in a Doc. The `config` field is
-- intended to be used to add attributes etc, like `poster` or `autoplay` for
-- the HTML <video> element, if rendered as such.
unique[b2ada5dfd4112ca3a7ba0a6483ce3d82811400c56eff8e6eca1b3fbf] type Doc2.Video
  = { sources: [Doc2.MediaSource]
    , config: [(Text, Text)]
    }

-- Useful for embedded data into a Doc, like title, date, tags etc:
unique[ea60b6205a6b25449a8784de87c113833bacbcdfe32829c7a76985d5] type Doc2.FrontMatter
  = FrontMatter [(Text, Text)]

-- Similar to using triple backticks with a latex pragma (```latex), but for
-- when you'd want to render LaTeX inline
unique[d1dc0515a2379df8a4c91571fe2f9bf9322adaf97677c87b806e49572447c688] type Doc2.LaTeXInline
  = LaTeXInline Text

-- Used for embedding SVGs
unique[ae4e05d8bede04825145db1a6a2222fdf2d890b3044d86fd4368f53b265de7f9] type Doc2.Svg
  = Svg Text

-- ex: Doc2.term 'List.map
Doc2.term : ∀ a g1 g. '{g} a ->{g1} Doc2.Term
Doc2.term a = Doc2.Term.Term (Any a)

unique[da70bff6431da17fa515f3d18ded11852b6a745f] type Doc2.SpecialForm
  -- @source{type Optional, List.map @ note1 note2} OR
  -- The notes are ignored currently, but will later be used to produce
  -- rich annotated source code with tooltips, highlights and whatnot.
  = Source [(Either Link.Type Doc2.Term, [Doc2.Term])]
  -- like Source, but the code starts out folded
  | FoldedSource [(Either Link.Type Doc2.Term, [Doc2.Term])]
  -- In `Example n expr`, `n` is the number of lambda parameters
  -- that should be elided during display.
  -- Ex: `Example 2 '(x y -> foo x y)` should render as `foo x y`.
  -- Ex: `Example 0 '(1 + 1)` should render as `42`.
  | Example Nat Doc2.Term
  -- Same as `Example`, but as a block rather than inline element
  | ExampleBlock Nat Doc2.Term
  -- {type Optional} or {List.map}
  | Link (Either Link.Type Doc2.Term)
  -- @signatures{List.map, List.filter, List.foldLeft}
  | Signature [Doc2.Term]
  -- @signature{List.map}
  | SignatureInline Doc2.Term
  -- ```
  -- id x = x
  -- id 42 + 1
  -- ```
  | Eval Doc2.Term
  -- @eval{1 + 1}
  | EvalInline Doc2.Term
  -- For extensions like a `Diagram` or `Animation` type.
  -- Renderers will be best effort for these; not all
  -- renderers will support all extensions
  | Embed Any
  | EmbedInline Any

unique[b7a4fb87e34569319591130bf3ec6e24c9955b6a] type Doc2
  -- Just raw text embedded in a doc. Will be unbroken.
  = Word Text
  -- Inline monospace, as in ''some monospace code''.
  | Code Doc2
  -- Block monospace with syntax highlighting.
  -- ''' blocks are parsed as ``` raw
  | CodeBlock Text Doc2
  | Bold Doc2
  | Italic Doc2
  | Strikethrough Doc2
  -- Can be used to affect HTML rendering
  | Style Text Doc2
  -- Create a named anchor point which can be used in links
  -- as in HTML: <h2><a id="section1">Section 1 Title</a></h2>
  | Anchor Text Doc2
  | Blockquote Doc2
  | Blankline
  | Linebreak
  -- For longer sections, this inserts a doodad or thingamajig
  | SectionBreak
  -- Tooltip inner tooltipContent
  | Tooltip Doc2 Doc2
  -- Aside asideContent
  | Aside Doc2
  -- Callout icon content
  | Callout (Optional Doc2) Doc2
  -- Table rows
  | Table [[Doc2]]
  -- Folded isFolded summary details
  -- If folded, only summary is shown, otherwise
  -- summary is followed by details. Some renderers
  -- will present this as a toggle or clickable elipses
  | Folded Boolean Doc2 Doc2
  -- Documents separated by spaces and wrapped to available width
  | Paragraph [Doc2]
  | BulletedList [Doc2]
  -- NumberedList startingNumber listElements
  | NumberedList Nat [Doc2]
  -- Section title subelements
  | Section Doc2 [Doc2]
  -- [our website](https://www.unison-lang.org/) or [blah]({type MyType})
  | NamedLink Doc2 Doc2
  -- image alt-text link caption
  | Image Doc2 Doc2 (Optional Doc2)
  | Special Doc2.SpecialForm
  -- Concatenation of docs
  | Join [Doc2]
  -- A section with no title but otherwise laid out the same
  | UntitledSection [Doc2]
  -- A list of documents that should start on separate lines;
  -- this is used for nested lists, for instance
  -- * A
  --   * A.1
  --   * A.2
  -- * B
  --   * B.1
  --   * B.2
  -- Is modeled as:
  --   BulletedList [ Column [A, BulletedList [A.1, A.2]]
  --                , Column [B, BulletedList [B.1, B.2]]
  | Column [Doc2]
  -- Sometimes useful in paragraph text to avoid line breaks in
  -- awkward places
  | Group Doc2

unique[d7b2ced8c08b2c6e54050d1f5acedef3395f293d] type Pretty.Annotated w txt
  -- See more detailed comments below on Pretty smart constructors, like
  -- Pretty.orElse, Pretty.group, etc
  = Empty
  | Group w (Pretty.Annotated w txt)
  | Lit w txt
  | Wrap w (Pretty.Annotated w txt)
  | OrElse w (Pretty.Annotated w txt) (Pretty.Annotated w txt)
  -- table rows
  | Table w [[Pretty.Annotated w txt]]
  -- Indent _ initialIndent indentAfterNewline p prefixes the first
  -- line of `p` with `initialIndent`, and subsequent lines by `indentAfterNewline`.
  | Indent w (Pretty.Annotated w txt) (Pretty.Annotated w txt) (Pretty.Annotated w txt)
  | Append w [Pretty.Annotated w txt]

structural type Pretty txt = Pretty (Pretty.Annotated () txt)

Pretty.get = cases Pretty p -> p

Pretty.map : (txt ->{g} txt2) ->{} Pretty txt ->{g} Pretty txt2
Pretty.map f p =
  use Pretty.Annotated
  go = cases
    Empty -> Empty
    Group _ p -> Group () (go p)
    Lit _ t -> Lit () (f t)
    Wrap _ p -> Wrap () (go p)
    OrElse _ p1 p2 -> OrElse () (go p1) (go p2)
    Table _ xs -> Table () (List.map (List.map go) xs)
    Indent _ i0 iN p -> Indent () (go i0) (go iN) (go p)
    Annotated.Append _ ps -> Annotated.Append () (List.map go ps)
  Pretty (go (Pretty.get p))

Pretty.empty : Pretty txt
Pretty.empty = Pretty Empty

{- A group adds a level of breaking. Layout tries not to break a group
   unless needed to fit in available width. Breaking is done "outside in".

   (a | b) <> (c | d) will try (a <> c)
                          then (b <> d)

   (a | b) <> group (c | d) will try (a <> c)
                                then (b <> c)
                                then (b <> d)
-}
Pretty.group : Pretty txt -> Pretty txt
Pretty.group p = Pretty (Group () (Pretty.get p))

-- Create a leaf-level `Pretty` that cannot be broken.
Pretty.lit : txt -> Pretty txt
Pretty.lit txt = Pretty (Lit () txt)

-- Turn on wrapping for `p`, which means that it inserts
-- softbreaks (either a space or a newline) between each
-- subgroup or leaf.
-- wrap (lit a <> lit b <> group c) ==
-- wrap (lit a <> group (orElse (lit " ") (lit "\n")
Pretty.wrap : Pretty txt -> Pretty txt
Pretty.wrap p = Pretty (Wrap () (Pretty.get p))

-- If `p1` fits on the current line at its preferred width,
-- it will be chosen, otherwise `p2` is chosen.
Pretty.orElse : Pretty txt -> Pretty txt -> Pretty txt
Pretty.orElse p1 p2 = Pretty (OrElse () (Pretty.get p1) (Pretty.get p2))

-- Prefixes all lines of `p` by `by`.
Pretty.indent : Pretty txt -> Pretty txt -> Pretty txt
Pretty.indent by p = Pretty (Indent () (Pretty.get by) (Pretty.get by) (Pretty.get p))

-- Prefixes the first line of `p` with `initialIndent`, and
-- subsequent lines by `indentAfterNewline`.
Pretty.indent' : Pretty txt -> Pretty txt -> Pretty txt -> Pretty txt
Pretty.indent' initialIndent indentAfterNewline p =
  Pretty (Indent () (Pretty.get initialIndent)
                    (Pretty.get indentAfterNewline)
                    (Pretty.get p))

Pretty.table : [[Pretty txt]] -> Pretty txt
Pretty.table rows = Pretty (Annotated.Table () (List.map (List.map Pretty.get) rows))

Pretty.append : Pretty txt -> Pretty txt -> Pretty txt
Pretty.append p1 p2 =
  use Pretty.Annotated Empty Append
  match (Pretty.get p1, Pretty.get p2) with
    (_, Empty) -> p1
    (Empty, _) -> p2
    (Append _ ps1, Append _ ps2) -> Pretty (Append () (ps1 List.++ ps2))
    (Append _ ps1, p2) -> Pretty (Append () (ps1 :+ p2))
    (p1, Append _ ps2) -> Pretty (Append () (p1 +: ps2))
    (p1,p2) -> Pretty (Append () [p1,p2])

Pretty.join : [Pretty txt] -> Pretty txt
Pretty.join =
  go acc = cases [] -> acc
                 h +: t -> go (append acc h) t
  go Pretty.empty

Pretty.sepBy : Pretty txt -> [Pretty txt] -> Pretty txt
Pretty.sepBy sep ps =
  go acc insertSep = cases
    [] -> acc
    ps | insertSep -> go (append acc sep) false ps
    h +: t -> go (append acc h) true t
  go Pretty.empty false ps

syntax.docJoin = cases [d] -> d
                       ds  -> Doc2.Join ds
syntax.docUntitledSection = cases
  [d] -> d
  ds -> UntitledSection ds
syntax.docColumn = cases
  [d] -> d
  ds -> Doc2.Column ds
syntax.docGroup = Doc2.Group
syntax.docWord = Word
syntax.docBold = Doc2.Bold
syntax.docItalic = Italic
syntax.docStrikethrough = Strikethrough
syntax.docParagraph = Paragraph
syntax.docEmbedTermLink tm =
  guid = "9d3927033a9589dda2d10406840af7ef3b4bf21e"
  Right (Doc2.term tm)
syntax.docEmbedTypeLink typ =
  guid = "f9e80035f8c21ac80c98b6c2cc06fe004ae2eb2c"
  Left typ
syntax.docSource t = Special (Source t)
syntax.docFoldedSource t = Special (FoldedSource t)
syntax.docSignature ts = Special (Signature ts)
syntax.docSignatureInline t = Special (SignatureInline t)
syntax.docSourceElement link annotations =
  guid = "e56ece7785c34c1cc9a441b11da81cfa98d05985"
  (link, annotations)
syntax.docEmbedAnnotations tms =
  guid = "11f21dc3bcb37652d8058d655e757560ac38f7b3"
  tms
syntax.docEmbedAnnotation tm =
  guid = "8546106e53c88996c8d3eb785a2fca80df9c7b3b"
  Doc2.Term.Term (Any tm)
syntax.docEmbedSignatureLink tm =
  guid = "d9a4fb87e34569319591130bf3ec6e24"
  Doc2.term tm
syntax.docCode c = Code c
syntax.docCodeBlock typ c = CodeBlock typ (docWord c)
syntax.docVerbatim c = CodeBlock "raw" c
syntax.docEval : '{} a -> Doc2
syntax.docEval d = Special (Eval (Doc2.term d))
syntax.docEvalInline : '{} a -> Doc2
syntax.docEvalInline a = Special (EvalInline (Doc2.term a))
syntax.docExample n a = Special (Example n (Doc2.term a))
syntax.docExampleBlock n a = Special (ExampleBlock n (Doc2.term a))
syntax.docLink t = Special (Link t)
syntax.docTransclude d =
  guid = "b7a4fb87e34569319591130bf3ec6e24"
  d
syntax.docNamedLink = NamedLink
syntax.docBulletedList = BulletedList
syntax.docNumberedList = NumberedList
syntax.docSection = Section
syntax.docTable = Doc2.Table
syntax.docBlockquote = Blockquote
syntax.docCallout = Callout
syntax.docAside = Aside
syntax.docTooltip = Tooltip

unique[e25bc44d251ae0301517ad0bd02cbd294161dc89] type ConsoleText
  = Plain Text
  | Foreground ANSI.Color ConsoleText
  | Background ANSI.Color ConsoleText
  | Bold ConsoleText
  | Underline ConsoleText
  | Invert ConsoleText

unique[de2e0ee924578939213c950dfd8e0ba1047703ae] type ANSI.Color
  = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue
  | BrightMagenta | BrightCyan | BrightWhite

List.map : (a ->{e} b) -> [a] ->{e} [b]
List.map f a =
  go i as acc =
    match List.at i as with
      None   -> acc
      Some a ->
        use Nat +
        go (i + 1) as (acc :+ f a)
  go 0 a []

Text.alignRightWith : Nat -> Char -> Text -> Text
Text.alignRightWith w padChar txt =
  rem = drop w (Text.size txt)
  if rem == 0 then txt
  else Text.repeat rem (Char.toText padChar) Text.++ txt

Text.alignLeftWith : Nat -> Char -> Text -> Text
Text.alignLeftWith w padChar txt =
  rem = drop w (Text.size txt)
  if rem == 0 then txt
  else txt Text.++ Text.repeat rem (Char.toText padChar)

Either.mapRight : (a -> b) -> Either e a -> Either e b
Either.mapRight f = cases
  Left e -> Left e
  Right a -> Right (f a)

syntax.docFormatConsole : Doc2 -> Pretty (Either SpecialForm ConsoleText)
syntax.docFormatConsole d =
  use Doc2
  lit t = Pretty.lit (Right (Plain t))
  p1 <> p2 = Pretty.append p1 p2
  nl = lit "\n"
  map f p = Pretty.map (mapRight f) p
  go = cases
    Word t -> lit t
    Code d -> Pretty.group (lit "`" <> go d <> lit "`")
    CodeBlock typ d -> Pretty.group (
      lit "``` " <> Pretty.group (lit typ) <> nl <>
      go d <> nl <>
      lit "```")
    Italic (Paragraph ([l] ++ mid ++ [r])) ->
      Pretty.group (lit "*" <> go l) <>
      Pretty.join (List.map go mid) <>
      Pretty.group (go r <> lit "*")
    Italic d -> Pretty.group (lit "*" <> go d <> lit "*")
    Strikethrough (Paragraph ([l] ++ mid ++ [r])) ->
      Pretty.group (lit "~~" <> go l) <>
      Pretty.join (List.map go mid) <>
      Pretty.group (go r <> lit "~~")
    Strikethrough d -> Pretty.group (lit "~~" <> go d <> lit "~~")
    Doc2.Bold d -> map ConsoleText.Bold (go d)
    Style _ d -> go d
    Anchor _ d -> go d
    Blockquote d -> Pretty.group (Pretty.indent (lit "> ") (go d))
    Blankline -> Pretty.group (lit "\n\n")
    Linebreak -> Pretty.group (lit "\n")
    SectionBreak -> lit "܍"
    Tooltip inner _ -> go inner
    Aside d -> map (Foreground BrightBlack) (lit "(" <> go d <> lit ")")
    Callout None d -> Pretty.group (Pretty.indent (lit "  | ") (go d))
    Callout (Some icon) d ->
      Pretty.group (Pretty.indent (lit "  | ") (
        Pretty.sepBy nl [
          map ConsoleText.Bold (go icon),
          lit "",
          go d
        ]))
    Table rows -> Pretty.table (List.map (List.map go) rows)
    Folded _ summary details -> go summary <> go details
    Paragraph ds -> Pretty.wrap (Pretty.join (List.map go ds))
    BulletedList ds ->
      item d = Pretty.indent' (lit "* ") (lit "  ") (go d)
      items = List.map item ds
      Pretty.group (Pretty.sepBy nl items)
    NumberedList n ds ->
      dot = ". "
      w = Text.size (Nat.toText (n + List.size ds)) + size dot
      num n = lit (Text.alignRightWith w ?\s (Nat.toText n Text.++ dot))
      indent = lit (Text.repeat w " ")
      item : Nat -> Doc2 -> Pretty (Either SpecialForm ConsoleText)
      item n d = Pretty.indent' (num n) indent (go d)
      items n acc = cases [] -> acc
                          d +: ds -> items (n+1) (acc :+ item n d) ds
      Pretty.group (Pretty.sepBy nl (items n [] ds))
    Section title ds ->
      ggo d = Pretty.group (go d)
      t = Pretty.indent' (lit "# ") (lit "  ") (ggo (Doc2.Bold title))
      subs = List.map (d -> Pretty.indent (lit "  ") (ggo d)) ds
      Pretty.group (Pretty.sepBy (nl <> nl) (t +: subs))
    UntitledSection ds ->
      ggo d = Pretty.group (go d)
      Pretty.group (Pretty.sepBy (nl <> nl) (List.map ggo ds))
    Join ds -> Pretty.join (List.map go ds)
    Column ds -> Pretty.sepBy nl (List.map go ds)
    Group d -> Pretty.group (go d)
    NamedLink name _target -> map ConsoleText.Underline (go name)
    Image alt _link (Some caption) -> Pretty.sepBy nl [go alt, go (Italic caption)]
    Image alt _link None -> go alt
    Special sf -> Pretty.lit (Left sf)
  go d

ImmutableArray.fromList l = Scope.run do
  sz = List.size l
  dst = Scope.array sz
  go i = cases
    [] -> ()
    x +: xs ->
      MutableArray.write dst i x
      go (i+1) xs
  handle go 0 l with cases
    { r } -> ()
    { raise _ -> _ } -> ()
  MutableArray.freeze! dst

ImmutableByteArray.fromBytes : Bytes -> ImmutableByteArray
ImmutableByteArray.fromBytes bs = Scope.run do
  sz = Bytes.size bs
  arr = Scope.bytearray sz
  fill i =
    match Bytes.at i bs with
      Some b ->
        MutableByteArray.write8 arr i b
        fill (i + 1)
      None -> ()
  handle fill 0
  with cases
    { _ }                      -> ()
    { raise _ -> _ } -> ()
  MutableByteArray.freeze! arr
|]

type Note = Result.Note Symbol Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann

type SynthResult =
  Result.Result
    (Seq Note)
    (Either (UF.UnisonFile Symbol Ann) TFile)

type EitherResult = Either String TFile

showNotes :: (Foldable f) => String -> PrintError.Env -> f Note -> String
showNotes source env =
  intercalateMap "\n\n" $ PrintError.renderNoteAsANSI 60 env source

ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.makePPE (PPE.hqNamer 10 Builtin.names) PPE.dontSuffixify
