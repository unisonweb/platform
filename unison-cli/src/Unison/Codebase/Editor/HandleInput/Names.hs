module Unison.Codebase.Editor.HandleInput.Names (handleNames) where

import Control.Monad (when)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Global qualified as Global
import Unison.Codebase.Editor.Input (ErrorMessageOrName, RawQuery)
import Unison.Codebase.Editor.Output (Output (..))
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as Names
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Util.Pretty qualified as P

-- | Handles a single @NamesI@ input query returning terms that match a given name.
--
-- Parameters:
--
-- * @global :: Bool@
-- ** If @True@, search all projects and branches.
-- ** If @False@, search only the current branch.
--
-- * @query :: (RawQuery, ErrorMessageOrName)@
-- ** The first member is the raw @nameQuery@ being handled.
-- ** The second member is the parsed @nameQuery@ that is either an error message
--    to be printed or a name that can be looked up in the codebase.
handleNames ::
  Bool ->
  (RawQuery, ErrorMessageOrName) ->
  Cli ()
handleNames _ (nameQuery, Left errMsg) = do
  Cli.respond $
    PrintMessage $
      P.lines [prettyNameQuery, errMsg]
  where
    prettyNameQuery =
      P.red (P.bold $ P.string nameQuery) <> ":"
handleNames global (nameQuery, Right query) = do
  hqLength <- Cli.runTransaction Codebase.hashLength
  let searchNames names = do
        let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
            unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
            terms = Names.lookupHQTerm Names.IncludeSuffixes query names
            types = Names.lookupHQType Names.IncludeSuffixes query names
            terms' :: [(Referent, [HQ'.HashQualified Name])]
            terms' = map (\r -> (r, PPE.allTermNames unsuffixifiedPPE r)) (Set.toList terms)
            types' :: [(Reference, [HQ'.HashQualified Name])]
            types' = map (\r -> (r, PPE.allTypeNames unsuffixifiedPPE r)) (Set.toList types)
        pure (terms', types')
  if global
    then do
      Global.forAllProjectBranches \(projBranchNames, _ids) branch -> do
        let names = Branch.toNames . Branch.head $ branch
        (terms, types) <- searchNames names
        when (not (null terms) || not (null types)) do
          Cli.respond $ GlobalListNames nameQuery projBranchNames hqLength types terms
    else do
      names <- Cli.currentNames
      (terms, types) <- searchNames names
      Cli.respond $ ListNames nameQuery hqLength types terms
