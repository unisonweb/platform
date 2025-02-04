module Unison.Test.UnisonSources where

import Control.Exception (throwIO)
import Control.Lens.Tuple (_5)
import Data.Map qualified as Map
import Data.Text (unpack)
import EasyTest
import System.Directory (doesFileExist)
import System.FilePath (joinPath, replaceExtension, splitPath)
import System.FilePath.Find (always, extension, find, (==?))
import Unison.Builtin qualified as Builtin
import Unison.Codebase.Runtime (Runtime, evaluateWatches)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrintError qualified as PrintError
import Unison.Result (Result, pattern Result)
import Unison.Result qualified as Result
import Unison.Runtime.Interface qualified as RTI
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Test.Common (parseAndSynthesizeAsFile, parsingEnv)
import Unison.Test.Common qualified as Common
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty (toPlain)

type Note = Result.Note Symbol Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann

type SynthResult =
  Result
    (Seq Note)
    (Either (UF.UnisonFile Symbol Ann) TFile)

type EitherResult = Either String TFile

ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.makePPE (PPE.hqNamer Common.hqLength Builtin.names) PPE.dontSuffixify

good :: EitherResult -> Test TFile
good = expectRight'

bad :: EitherResult -> Test TFile
bad r = EasyTest.expectLeft r >> done

test :: Test ()
test = do
  using (RTI.startRuntime False RTI.OneOff "") RTI.terminate \rt -> do
    scope "unison-src"
      . tests
      $ [ go rt shouldPassNow good,
          go rt shouldFailNow bad,
          go rt shouldPassLater (pending . bad),
          go rt shouldFailLater (pending . good)
        ]

shouldPassPath, shouldFailPath :: String
shouldPassPath = "../unison-src/tests"
shouldFailPath = "../unison-src/errors"

shouldPassNow :: IO [FilePath]
shouldPassNow = find always (extension ==? ".u") shouldPassPath

shouldFailNow :: IO [FilePath]
shouldFailNow = find always (extension ==? ".u") shouldFailPath

shouldPassLater :: IO [FilePath]
shouldPassLater = find always (extension ==? ".uu") shouldPassPath

shouldFailLater :: IO [FilePath]
shouldFailLater = find always (extension ==? ".uu") shouldFailPath

go :: Runtime Symbol -> IO [FilePath] -> (EitherResult -> Test TFile) -> Test ()
go rt files how = do
  files' <- liftIO files
  tests (makePassingTest rt how <$> files')

showNotes :: (Foldable f) => String -> PrintError.Env -> f Note -> String
showNotes source env =
  intercalateMap "\n\n" $ PrintError.renderNoteAsANSI 60 env source

decodeResult ::
  String -> SynthResult -> EitherResult --  String (UF.TypecheckedUnisonFile Symbol Ann)
decodeResult source (Result notes Nothing) =
  Left $ showNotes source ppEnv notes
decodeResult source (Result notes (Just (Left uf))) =
  let errNames = UF.toNames uf
   in Left $
        showNotes
          source
          ( PPE.makePPE
              (PPE.hqNamer Common.hqLength (Names.shadowing errNames Builtin.names))
              PPE.dontSuffixify
          )
          notes
decodeResult _source (Result _notes (Just (Right uf))) =
  Right uf

makePassingTest ::
  Runtime Symbol -> (EitherResult -> Test TFile) -> FilePath -> Test ()
makePassingTest rt how filepath = scope (shortName filepath) $ do
  uf <- typecheckingTest how filepath
  resultTest rt uf filepath

shortName :: FilePath -> FilePath
shortName = joinPath . drop 1 . splitPath

typecheckingTest :: (EitherResult -> Test TFile) -> FilePath -> Test TFile
typecheckingTest how filepath = scope "typecheck" $ do
  source <- io $ unpack <$> readUtf8 filepath
  how . decodeResult source $ parseAndSynthesizeAsFile [] (shortName filepath) source

resultTest ::
  Runtime Symbol -> TFile -> FilePath -> Test ()
resultTest rt uf filepath = do
  let valueFile = replaceExtension filepath "ur"
  rFileExists <- io $ doesFileExist valueFile
  if rFileExists
    then scope "result" $ do
      values <- io $ unpack <$> readUtf8 valueFile
      let term = runIdentity (Parsers.parseTerm values parsingEnv)
      let report e = throwIO (userError $ toPlain 10000 e)
      (bindings, _, watches) <-
        io $
          either report pure
            =<< evaluateWatches
              Builtin.codeLookup
              PPE.empty
              (const $ pure Nothing)
              rt
              uf
      case term of
        Right tm -> do
          -- compare the watch expression from the .u with the expr in .ur
          let watchResult = head (view _5 <$> Map.elems watches)
              tm' = Term.letRec' False (bindings <&> \(sym, tm) -> (sym, (), tm)) watchResult
          -- note . show $ tm'
          -- note . show $ Term.amap (const ()) tm
          expectEqual tm' (Term.amap (const ()) tm)
        Left e -> crash $ PrintError.renderParseErrorAsANSI 80 values e
    else pure ()
