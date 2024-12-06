module Unison.Codebase.Editor.HandleInput.Dependents
  ( handleDependents,
  )
where

import Data.Set qualified as Set
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQToLabeledDependencies)
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo, empty)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.List (nubOrdOn)

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq
  -- Use an unsuffixified PPE here, so we display full names (relative to the current path),
  -- rather than the shortest possible unambiguous name.
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let fqppe = PPE.unsuffixifiedPPE pped
  let ppe = PPE.suffixifiedPPE pped
  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  results <- for (toList lds) \ld -> do
    -- The full set of dependent references, any number of which may not have names in the current namespace.
    dependents <-
      let tp = Codebase.dependents Queries.ExcludeOwnComponent
          tm = \case
            Referent.Ref r -> Codebase.dependents Queries.ExcludeOwnComponent r
            Referent.Con (ConstructorReference r _cid) _ct ->
              Codebase.dependents Queries.ExcludeOwnComponent r
       in Cli.runTransaction (LD.fold tp tm ld)
    let -- True is term names, False is type names
        results :: [(Bool, HQ.HashQualified Name, Reference)]
        results = do
          r <- Set.toList dependents
          Just (isTerm, hq) <- [(True,) <$> PPE.terms fqppe (Referent.Ref r), (False,) <$> PPE.types fqppe r]
          fullName <- [HQ'.toName hq]
          guard (not (Name.beginsWithSegment fullName NameSegment.libSegment))
          Just shortName <- pure $ PPE.terms ppe (Referent.Ref r) <|> PPE.types ppe r
          pure (isTerm, HQ'.toHQ shortName, r)
    pure results
  let sort = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst)
  let types = sort [(n, r) | (False, n, r) <- join results]
  let terms = sort [(n, r) | (True, n, r) <- join results]
  Cli.setNumberedArgs . map SA.HashQualified $ types <> terms
  Cli.respond (ListDependents ppe lds types terms)
