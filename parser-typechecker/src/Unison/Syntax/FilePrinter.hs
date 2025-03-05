module Unison.Syntax.FilePrinter
  ( renderDefnsForUnisonFile,
  )
where

import Control.Lens (mapped, _1)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer qualified as Writer
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Builtin.Decls qualified as Builtin.Decls
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DeclNameLookup (DeclNameLookup, expectConstructorNames)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.Reference (TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Syntax.DeclPrinter (AccessorName)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Var (Var)

-- | Render definitions destined for a Unison file.
--
-- This first renders the types (discovering which record accessors will be generated upon parsing), then renders the
-- terms (being careful not to render any record accessors, since those would cause duplicate binding errors upon
-- parsing).
renderDefnsForUnisonFile ::
  forall a v.
  (Var v, Monoid a) =>
  DeclNameLookup ->
  PrettyPrintEnvDecl ->
  DeclPrinter.RenderUniqueTypeGuids ->
  DefnsF (Map Name) (Term v a, Type v a) (TypeReferenceId, Decl v a) ->
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
renderDefnsForUnisonFile declNameLookup ppe guid defns =
  let (types, accessorNames) = Writer.runWriter (Map.traverseWithKey renderType defns.types)
   in Defns
        { terms = Map.mapMaybeWithKey (renderTerm accessorNames) defns.terms,
          types
        }
  where
    renderType :: Name -> (TypeReferenceId, Decl v a) -> Writer (Set AccessorName) (Pretty ColorText)
    renderType name (ref, typ) =
      fmap Pretty.syntaxToColor $
        DeclPrinter.prettyDeclW
          -- Sort of a hack; since the decl printer looks in the PPE for names of constructors,
          -- we just delete all term names out and add back the constructors...
          -- probably no need to wipe out the suffixified side but we do it anyway
          (setPpedToConstructorNames declNameLookup name ref ppe)
          guid
          (Reference.fromId ref)
          (HQ.NameOnly name)
          typ

    renderTerm :: Set Name -> Name -> (Term v a, Type v a) -> Maybe (Pretty ColorText)
    renderTerm accessorNames name (term, typ) = do
      guard (not (Set.member name accessorNames))
      let hqName = HQ.NameOnly name
      let rendered
            | Typechecker.isEqual (Builtin.Decls.testResultListType mempty) typ =
                "test> " <> TermPrinter.prettyBindingWithoutTypeSignature ppe.suffixifiedPPE hqName term
            | otherwise = TermPrinter.prettyBinding ppe.suffixifiedPPE hqName term
      Just (Pretty.syntaxToColor rendered)

setPpedToConstructorNames :: DeclNameLookup -> Name -> TypeReferenceId -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
setPpedToConstructorNames declNameLookup name ref =
  set (#unsuffixifiedPPE . #termNames) referentNames
    . set (#suffixifiedPPE . #termNames) referentNames
  where
    constructorNameMap :: Map ConstructorReference Name
    constructorNameMap =
      Map.fromList
        ( name
            & expectConstructorNames declNameLookup
            & List.zip [0 ..]
            & over (mapped . _1) (ConstructorReference (Reference.fromId ref))
        )

    referentNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    referentNames = \case
      Referent.Con conRef _ ->
        case Map.lookup conRef constructorNameMap of
          Nothing -> []
          Just conName -> let hqConName = HQ'.NameOnly conName in [(hqConName, hqConName)]
      Referent.Ref _ -> []
