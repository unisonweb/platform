module Unison.Codebase.Editor.HandleInput.Dependencies
  ( handleDependencies,
  )
where

import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQToLabeledDependencies)
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.LabeledDependency qualified as LabeledDependency
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Util.List (nubOrdOn)

handleDependencies :: HQ.HashQualified Name -> Cli ()
handleDependencies hq = do
  Cli.Env {codebase} <- ask
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = pped.suffixifiedPPE
  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)
  results <- for (toList lds) \ld -> do
    dependencies :: Set LabeledDependency <-
      Cli.runTransaction do
        let tp r@(Reference.DerivedId i) =
              Codebase.getTypeDeclaration codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl ->
                  Set.map LabeledDependency.TypeReference . Set.delete r . DD.typeDependencies $
                    DD.asDataDecl decl
            tp _ = pure mempty
            tm r@(Referent.Ref (Reference.DerivedId i)) =
              Codebase.getTerm codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just tm -> Set.delete (LabeledDependency.TermReferent r) (Term.labeledDependencies tm)
            tm con@(Referent.Con (ConstructorReference (Reference.DerivedId i) cid) _ct) =
              Codebase.getTypeDeclaration codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl -> case DD.typeOfConstructor (DD.asDataDecl decl) cid of
                  Nothing -> error $ "What happened to " ++ show con ++ "?"
                  Just tp -> Type.labeledDependencies tp
            tm _ = pure mempty
         in LD.fold tp tm ld
    let types = [(PPE.typeName suffixifiedPPE r, r) | LabeledDependency.TypeReference r <- toList dependencies]
    let terms = [(PPE.termName suffixifiedPPE r, r) | LabeledDependency.TermReferent r <- toList dependencies]
    pure (types, terms)
  let types = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst) . join $ fst <$> results
  let terms = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst) . join $ snd <$> results
  Cli.setNumberedArgs . map SA.HashQualified $ types <> terms
  Cli.respond $ ListDependencies suffixifiedPPE lds types terms
