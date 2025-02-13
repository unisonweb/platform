module Unison.Syntax.DeclPrinter
  ( prettyDecl,
    prettyDeclW,
    prettyDeclHeader,
    prettyDeclOrBuiltinHeader,
    getFieldAndAccessorNames,
    AccessorName,
  )
where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List.NonEmpty (pattern (:|))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration, toDataDecl)
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Dependencies qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference, TypeReference)
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NamePrinter (prettyName, styleHashQualified'')
import Unison.Syntax.TypePrinter (runPretty)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Syntax.Var qualified as Var (namespaced)
import Unison.Type qualified as Type
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as P
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)
import Unison.Var qualified as Var (freshenId, name, named)

type SyntaxText = S.SyntaxText' Reference

type AccessorName = Name

prettyDeclW ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  TypeReference ->
  HQ.HashQualified Name ->
  DD.Decl v a ->
  Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW ppe r hq = \case
  Left e -> pure $ prettyEffectDecl ppe r hq e
  Right dd -> prettyDataDecl ppe r hq dd

prettyDecl ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  TypeReference ->
  HQ.HashQualified Name ->
  DD.Decl v a ->
  Pretty SyntaxText
prettyDecl ppe r hq d = fst . runWriter $ prettyDeclW ppe r hq d

prettyEffectDecl ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  TypeReference ->
  HQ.HashQualified Name ->
  EffectDeclaration v a ->
  Pretty SyntaxText
prettyEffectDecl ppe r name = prettyGADT ppe CT.Effect r name . toDataDecl

prettyGADT ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  CT.ConstructorType ->
  TypeReference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Pretty SyntaxText
prettyGADT env ctorType r name dd =
  header <> P.newline <> P.indentN 2 constructors
  where
    constructors = P.lines (constructor <$> zip [0 ..] (DD.constructors' dd))
    constructor (n, (_, _, t)) =
      prettyPattern (PPED.unsuffixifiedPPE env) ctorType name (ConstructorReference r n)
        <> fmt S.TypeAscriptionColon " :"
          `P.hang` TypePrinter.prettySyntax (PPED.suffixifiedPPE env) t
    header = prettyEffectHeader name (DD.EffectDeclaration dd) <> fmt S.ControlKeyword " where"

prettyPattern ::
  PrettyPrintEnv ->
  CT.ConstructorType ->
  HQ.HashQualified Name ->
  ConstructorReference ->
  Pretty SyntaxText
prettyPattern env ctorType namespace ref =
  styleHashQualified''
    (fmt (S.TermReference conRef))
    ( let strip =
            case HQ.toName namespace of
              Nothing -> id
              Just name -> HQ.stripNamespace name
       in strip (PPE.termName env conRef)
    )
  where
    conRef = Referent.Con ref ctorType

prettyDataDecl ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  TypeReference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Writer (Set AccessorName) (Pretty SyntaxText)
prettyDataDecl (PrettyPrintEnvDecl unsuffixifiedPPE suffixifiedPPE) r name dd =
  (header <>) . P.sep (fmt S.DelimiterChar (" | " `P.orElse` "\n  | "))
    <$> constructor `traverse` zip [0 ..] (DD.constructors' dd)
  where
    constructor (n, (_, _, Type.ForallsNamed' _ t)) = constructor' n t
    constructor (n, (_, _, t)) = constructor' n t
    constructor' n t = case Type.unArrows t of
      Nothing -> pure $ prettyPattern unsuffixifiedPPE CT.Data name (ConstructorReference r n)
      Just ts -> case getFieldAndAccessorNames unsuffixifiedPPE r name dd of
        Nothing ->
          pure
            . P.group
            . P.hang' (prettyPattern unsuffixifiedPPE CT.Data name (ConstructorReference r n)) "      "
            $ P.spaced (runPretty suffixifiedPPE (traverse (TypePrinter.prettyRaw Map.empty 10) (init ts)))
        Just (fieldNames, _) -> do
          tell $
            Set.fromList $
              [ case accessor of
                  Nothing -> declName `Name.joinDot` fieldName
                  Just accessor -> declName `Name.joinDot` fieldName `Name.joinDot` accessor
                | HQ.NameOnly declName <- [name],
                  fieldName <- fieldNames,
                  accessor <-
                    [ Nothing,
                      Just (Name.fromSegment NameSegment.setSegment),
                      Just (Name.fromSegment NameSegment.modifySegment)
                    ]
              ]
          pure . P.group $
            fmt S.DelimiterChar "{ "
              <> P.sep
                (fmt S.DelimiterChar "," <> " " `P.orElse` "\n      ")
                (field <$> zip fieldNames (init ts))
              <> fmt S.DelimiterChar " }"
    field (fname, typ) =
      P.group $
        fmt (S.TypeReference r) (prettyName fname)
          <> fmt S.TypeAscriptionColon " :"
            `P.hang` runPretty suffixifiedPPE (TypePrinter.prettyRaw Map.empty (-1) typ)
    header = prettyDataHeader name dd <> fmt S.DelimiterChar (" = " `P.orElse` "\n  = ")

-- This function determines if a data declaration "looks like a record", and if so, returns both its auto-generated
-- accessor names (such as "Pt.x.set") and field names (such as "x"). Because we generate three accessors per field,
-- there will always be three times as many accessors as there are fields.
--
-- It works by works by generating the record accessor terms for the data type, hashing these terms, and then checking
-- the `PrettyPrintEnv` for the names of those hashes.
--
-- For example, for a type named "Pt", if the names of its accessors are
--
--   `Pt.x`, `Pt.x.set`, `Pt.x.modify`, `Pt.y`, `Pt.y.set`, `Pt.y.modify`
--
-- then we will return those accessors along with the field names
--
--   `x`, `y`
--
-- This function returns `Nothing` if the given data declaration does not "look like a record".
getFieldAndAccessorNames ::
  forall v a.
  (Var v) =>
  PrettyPrintEnv ->
  TypeReference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Maybe ([Name], [Name]) -- field names, accessor names
getFieldAndAccessorNames env r hqTypename dd = do
  -- If we only have a hash for the decl, then we can't know where in the namespace to look for the generated accessors,
  -- so we just give up trying to infer whether this was a record (even if it was one).
  typename <- HQ.toName hqTypename

  -- Records have exactly one constructor
  [(_, typ)] <- Just (DD.constructors dd)

  -- [ "_0", "_1"-1 ]
  let vars :: [v]
      -- We add `n` to the end of the variable name as a quick fix to #4752, but we suspect there's a more
      -- fundamental fix to be made somewhere in the term printer to automatically suffix a var name with its
      -- freshened id if it would be ambiguous otherwise.
      vars = [Var.freshenId (fromIntegral n) (Var.named ("_" <> Text.pack (show n))) | n <- [0 .. Type.arity typ - 1]]

  -- {
  --   "Pt._0"         => ( #getx    ,   pt -> match pt with Pt x _ -> x          , Pt -> Int                )
  --   "Pt._0.set"     => ( #setx    , x pt -> match pt with Pt _ y -> Pt x y     , Int -> Pt -> Pt          )
  --   "Pt._0.modify"  => ( #modifyx , f pt -> match pt with Pt x y -> Pt (f x) y , (Int -> Int) -> Pt -> Pt )
  --   "Pt._11"        => ( #gety    ,   pt -> match pt with Pt _ y -> y          , Pt -> Int                )
  --   "Pt._11.set"    => ( #sety    , y pt -> match pt with Pt x _ -> Pt x y     , Int -> Pt -> Pt          )
  --   "Pt._11.modify" => ( #modifyy , f pt -> match pt with Pt x y -> Pt x (f y) , (Int -> Int) -> Pt -> Pt )
  -- }
  hashes <- DD.hashFieldAccessors env (Name.toVar typename) vars r dd

  -- [
  --   ( #getx    , "Pt.x"        )
  --   ( #setx    , "Pt.x.set"    )
  --   ( #modifyx , "Pt.x.modify" )
  --   ( #gety    , "Pt.y"        )
  --   ( #sety    , "Pt.y.set"    )
  --   ( #modifyy , "Pt.y.modify" )
  -- ]
  let accessorNamesByHash =
        hashes
          & Map.elems
          & map \(refId, _term, _typ) ->
            (refId, HQ.toText (PPE.termName env (Referent.fromTermReferenceId refId)))

  -- {
  --   #getx    => "x"
  --   #setx    => "x"
  --   #modifyx => "x"
  --   #gety    => "y"
  --   #sety    => "y"
  --   #modifyy => "y"
  -- }
  let fieldNamesByHash =
        Map.fromList
          [ (r, f)
            | (r, n) <- accessorNamesByHash,
              let typenameText = Name.toText typename,
              typenameText `Text.isPrefixOf` n,
              let rest = Text.drop (Text.length typenameText + 1) n,
              (f, rest) <- pure $ Text.span (/= '.') rest,
              rest `elem` ["", ".set", ".modify"]
          ]

  if Map.size fieldNamesByHash == length accessorNamesByHash
    then
      Just
        ( [ Name.unsafeParseText name
            | -- "_0"
              v <- vars,
              -- #getx
              Just (ref, _, _) <- [Map.lookup (Var.namespaced (Name.toVar typename :| [v])) hashes],
              -- "x"
              Just name <- [Map.lookup ref fieldNamesByHash]
          ],
          map (Name.unsafeParseText . snd) accessorNamesByHash
        )
    else Nothing

prettyModifier :: DD.Modifier -> Pretty SyntaxText
prettyModifier DD.Structural = fmt S.DataTypeModifier "structural"
prettyModifier (DD.Unique _uid) = mempty -- don't print anything since 'unique' is the default
-- leaving this comment for the historical record so the syntax for uid is not forgotten
-- fmt S.DataTypeModifier "unique" -- <> ("[" <> P.text uid <> "] ")

prettyDataHeader ::
  (Var v) => HQ.HashQualified Name -> DD.DataDeclaration v a -> Pretty SyntaxText
prettyDataHeader name dd =
  P.sepNonEmpty
    " "
    [ prettyModifier (DD.modifier dd),
      fmt S.DataTypeKeyword "type",
      styleHashQualified'' (fmt $ S.HashQualifier name) name,
      P.sep " " (fmt S.DataTypeParams . P.text . Var.name <$> DD.bound dd)
    ]

prettyEffectHeader ::
  (Var v) =>
  HQ.HashQualified Name ->
  DD.EffectDeclaration v a ->
  Pretty SyntaxText
prettyEffectHeader name ed =
  P.sepNonEmpty
    " "
    [ prettyModifier (DD.modifier (DD.toDataDecl ed)),
      fmt S.DataTypeKeyword "ability",
      styleHashQualified'' (fmt $ S.HashQualifier name) name,
      P.sep
        " "
        (fmt S.DataTypeParams . P.text . Var.name <$> DD.bound (DD.toDataDecl ed))
    ]

prettyDeclHeader ::
  (Var v) =>
  HQ.HashQualified Name ->
  Either (DD.EffectDeclaration v a) (DD.DataDeclaration v a) ->
  Pretty SyntaxText
prettyDeclHeader name (Left e) = prettyEffectHeader name e
prettyDeclHeader name (Right d) = prettyDataHeader name d

prettyDeclOrBuiltinHeader ::
  (Var v) =>
  HQ.HashQualified Name ->
  DD.DeclOrBuiltin v a ->
  Pretty SyntaxText
prettyDeclOrBuiltinHeader name (DD.Builtin ctype) = case ctype of
  CT.Data -> fmt S.DataTypeKeyword "builtin type " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
  CT.Effect -> fmt S.DataTypeKeyword "builtin ability " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
prettyDeclOrBuiltinHeader name (DD.Decl e) = prettyDeclHeader name e

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = P.withSyntax
