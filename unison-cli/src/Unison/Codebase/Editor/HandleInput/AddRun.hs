module Unison.Codebase.Editor.HandleInput.AddRun
  ( handleAddRun,
  )
where

import Control.Lens (use)
import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.Update (doSlurpAdds)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (Output (NoLastRunResult, SaveTermNameConflict, SlurpOutput))
import Unison.Codebase.Editor.Slurp qualified as Slurp
import Unison.Codebase.Editor.SlurpResult qualified as SlurpResult
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.Name (Name)
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF

handleAddRun :: Input -> Name -> Cli ()
handleAddRun input resultName = do
  let resultVar = Name.toVar resultName
  uf <- addSavedTermToUnisonFile resultName
  Cli.Env {codebase} <- ask
  currentNames <- Cli.currentNames
  let sr = Slurp.slurpFile uf (Set.singleton resultVar) Slurp.AddOp currentNames
  let adds = SlurpResult.adds sr
  Cli.runTransaction . Codebase.addDefsToCodebase codebase . SlurpResult.filterUnisonFile sr $ uf
  let description = (Text.pack (InputPattern.patternName InputPatterns.saveExecuteResult) <> " " <> Name.toText resultName)
  pp <- Cli.getCurrentProjectPath
  Cli.stepAt description (pp, doSlurpAdds adds uf)
  let namesWithDefinitionsFromFile = UF.addNamesFromTypeCheckedUnisonFile uf currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 namesWithDefinitionsFromFile) (PPE.suffixifyByHash namesWithDefinitionsFromFile)
  let suffixifiedPPE = PPE.suffixifiedPPE pped
  Cli.respond $ SlurpOutput input suffixifiedPPE sr

addSavedTermToUnisonFile :: Name -> Cli (TypecheckedUnisonFile Symbol Ann)
addSavedTermToUnisonFile resultName = do
  let resultSymbol = Name.toVar resultName
  (trm, typ, uf) <-
    use #lastRunResult & onNothingM do
      Cli.returnEarly NoLastRunResult
  whenJust (Map.lookup resultSymbol (UF.hashTermsId uf)) \_ -> do
    Cli.returnEarly (SaveTermNameConflict resultName)
  pure $
    UF.typecheckedUnisonFile
      (UF.dataDeclarationsId' uf)
      (UF.effectDeclarationsId' uf)
      ([(resultSymbol, External, trm, typ)] : UF.topLevelComponents' uf)
      (UF.watchComponents uf)
