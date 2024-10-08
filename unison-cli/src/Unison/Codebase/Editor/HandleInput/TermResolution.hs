module Unison.Codebase.Editor.HandleInput.TermResolution
  ( lookupTermRefs,
    lookupTermRefWithType,
    resolveMainRef,
  )
where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Data.Maybe (catMaybes)
import Data.Set (toList)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Runtime qualified as Runtime
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent, pattern Ref)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker

lookupTerm :: HQ.HashQualified Name -> Names -> [Referent]
lookupTerm hq parseNames = toList (Names.lookupHQTerm Names.IncludeSuffixes hq parseNames)

lookupTermRefs ::
  HQ.HashQualified Name -> Names -> ([Reference], [Referent])
lookupTermRefs hq parseNames =
  unzip . catMaybes . fmap extract $ lookupTerm hq parseNames
  where
    extract rt@(Ref rf) = Just (rf, rt)
    extract _ = Nothing

lookupTermRefWithType ::
  Codebase.Codebase IO Symbol Ann ->
  HQ.HashQualified Name ->
  Cli [(Reference, Type Symbol Ann)]
lookupTermRefWithType codebase name = do
  names <- Cli.currentNames
  liftIO
    . Codebase.runTransaction codebase
    . fmap catMaybes
    . traverse annot
    . fst
    $ lookupTermRefs name names
  where
    annot tm =
      fmap ((,) tm) <$> Codebase.getTypeOfTerm codebase tm

resolveMainRef :: HQ.HashQualified Name -> Cli (Reference, PrettyPrintEnv)
resolveMainRef main = do
  Cli.Env {codebase, runtime} <- ask
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  let mainType = Runtime.mainType runtime
  lookupTermRefWithType codebase main >>= \case
    [(rf, ty)]
      | Typechecker.fitsScheme ty mainType -> pure (rf, suffixifiedPPE)
      | otherwise -> Cli.returnEarly (BadMainFunction "main" main ty suffixifiedPPE [mainType])
    _ -> Cli.returnEarly (NoMainFunction main suffixifiedPPE [mainType])
