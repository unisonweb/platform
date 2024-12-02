module Unison.Codebase.Editor.HandleInput.EditNamespace
  ( handleEditNamespace,
    getNamesForEdit,
  )
where

import Control.Monad.Reader
import Data.Foldable qualified as Foldable
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Reference (Reference' (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.DisplayObject qualified as DisplayObject
import Unison.Codebase.Editor.HandleInput.ShowDefinition (showDefinitions)
import Unison.Codebase.Editor.Input (OutputLocation (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Set qualified as Set

handleEditNamespace :: OutputLocation -> [Path] -> Cli ()
handleEditNamespace outputLoc paths0 = do
  Cli.Env {codebase} <- ask
  currentBranch <- Cli.getCurrentBranch0
  let currentNames = Branch.toNames currentBranch
  let ppe = PPED.makePPED (PPE.hqNamer 10 currentNames) (PPE.suffixifyByHashName currentNames)

  -- Adjust the requested list of paths slightly: if it's missing (i.e. `edit.namespace` without arguments), then behave
  -- as if the empty path (which there is no syntax for, heh) was supplied.
  let paths =
        if null paths0
          then [Path.empty]
          else paths0

  -- Make a names object that contains the union of all names in the supplied paths (each prefixed with the associated
  -- path of course). Special case: if the path is the empty path, then ignore `lib`.
  let allNamesToEdit =
        List.nubOrd paths & foldMap \path ->
          let branch = (if path == Path.empty then Branch.withoutLib else id) (Branch.getAt0 path currentBranch)
              names = Branch.toNames branch
           in case Path.toName path of
                Nothing -> names
                Just pathPrefix -> Names.prefix0 pathPrefix names

  (types, terms) <- Cli.runTransaction (getNamesForEdit codebase ppe allNamesToEdit)
  let misses = []
  showDefinitions outputLoc ppe terms types misses

-- | Get names "for edit": gets types and terms out the codebase as display objects, but is careful not to get an
-- auto-generated record accessor term like `Foo.bar.set` if it's also getting the corresponding type `Foo`. This is
-- because these name are "for edit", i.e. going into a scratch file, where parsing the record type will generate
-- its accessors.
getNamesForEdit ::
  Codebase m Symbol Ann ->
  PrettyPrintEnvDecl ->
  Names ->
  Sqlite.Transaction
    ( Map TypeReference (DisplayObject () (Decl Symbol Ann)),
      Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))
    )
getNamesForEdit codebase ppe allNamesToEdit = do
  let termRefs = Names.termReferences allNamesToEdit
  let typeRefs = Names.typeReferences allNamesToEdit

  (types, accessorNames) <-
    Foldable.foldlM
      ( \(types, accessorNames) ref ->
          case ref of
            ReferenceBuiltin _ -> do
              let !types1 = Map.insert ref (DisplayObject.BuiltinObject ()) types
              pure (types1, accessorNames)
            ReferenceDerived refId -> do
              decl <- Codebase.unsafeGetTypeDeclaration codebase refId
              let !types1 = Map.insert ref (DisplayObject.UserObject decl) types
              let !accessorNames1 =
                    accessorNames <> case decl of
                      Left _effectDecl -> Set.empty
                      Right dataDecl ->
                        let declAccessorNames :: Name -> Set Name
                            declAccessorNames declName =
                              case DeclPrinter.getFieldAndAccessorNames
                                ppe.unsuffixifiedPPE
                                ref
                                (HQ.fromName declName)
                                dataDecl of
                                Nothing -> Set.empty
                                Just (_fieldNames, theAccessorNames) -> Set.fromList theAccessorNames
                         in foldMap declAccessorNames (Names.namesForReference allNamesToEdit ref)
              pure (types1, accessorNames1)
      )
      (Map.empty, Set.empty)
      typeRefs

  terms <-
    termRefs & foldMapM \ref ->
      let isRecordAccessor =
            Set.intersects
              (Names.namesForReferent allNamesToEdit (Referent.fromTermReference ref))
              accessorNames
       in if isRecordAccessor
            then pure Map.empty
            else Map.singleton ref <$> Backend.displayTerm codebase ref

  pure (types, terms)
