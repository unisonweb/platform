{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unison.Test.Ucm
  ( initCodebase,
    deleteCodebase,
    runTranscript,
    lowLevel,
    CodebaseFormat (..),
    Transcript,
    unTranscript,
    Codebase (..),
  )
where

import Control.Monad (when)
import Data.Text qualified as Text
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp qualified as Temp
import U.Util.Text (stripMargin)
import Unison.Codebase (CodebasePath)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Init qualified as Codebase.Init
import Unison.Codebase.Init.CreateCodebaseError (CreateCodebaseError (..))
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Transcript.Runner qualified as Transcript
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.Parser.Ann (Ann)
import Unison.Prelude (toList, traceM)
import Unison.PrettyTerminal qualified as PT
import Unison.Symbol (Symbol)
import Unison.Util.Pretty qualified as P

data CodebaseFormat = CodebaseFormat2 deriving (Show, Enum, Bounded)

data Codebase = Codebase CodebasePath CodebaseFormat deriving (Show)

-- newtype Transcript = Transcript {unTranscript :: Text}
--   deriving (IsString, Show, Semigroup) via Text
type Transcript = String

unTranscript :: a -> a
unTranscript = id

type TranscriptOutput = String

debugTranscriptOutput :: Bool
debugTranscriptOutput = False

initCodebase :: CodebaseFormat -> IO Codebase
initCodebase fmt = do
  let cbInit = case fmt of CodebaseFormat2 -> SC.init
  tmp <-
    Temp.getCanonicalTemporaryDirectory
      >>= flip Temp.createTempDirectory "ucm-test"
  result <- Codebase.Init.withCreatedCodebase cbInit "ucm-test" tmp SC.DoLock (const $ pure ())
  case result of
    Left CreateCodebaseAlreadyExists -> fail $ P.toANSI 80 "Codebase already exists"
    Right _ -> pure $ Codebase tmp fmt

deleteCodebase :: Codebase -> IO ()
deleteCodebase (Codebase path _) = removeDirectoryRecursive path

runTranscript :: Codebase -> Transcript -> IO TranscriptOutput
runTranscript (Codebase codebasePath fmt) transcript = do
  let err e = fail $ "Parse error: \n" <> show e
      cbInit = case fmt of CodebaseFormat2 -> SC.init
      isTest = True
  Transcript.withRunner isTest Verbosity.Silent "Unison.Test.Ucm.runTranscript Invalid Version String" rtp $
    \runner -> do
      result <- Codebase.Init.withOpenCodebase cbInit "transcript" codebasePath SC.DoLock SC.DontMigrate \codebase -> do
        Codebase.runTransaction codebase (Codebase.installUcmDependencies codebase)
        let transcriptSrc = stripMargin . Text.pack $ unTranscript transcript
        output <-
          either err (Text.unpack . Transcript.formatStanzas . toList)
            <$> runner "transcript" transcriptSrc (codebasePath, codebase)
        when debugTranscriptOutput $ traceM output
        pure output
      either (fail . P.toANSI 80 . P.shown) pure result
  where
    -- Note: this needs to be properly configured if these tests ever
    -- need to do native compiles. But I suspect they won't.
    rtp = "native-compiler/bin"

lowLevel :: Codebase -> (Codebase.Codebase IO Symbol Ann -> IO a) -> IO a
lowLevel (Codebase root fmt) action = do
  let cbInit = case fmt of CodebaseFormat2 -> SC.init
  result <- Codebase.Init.withOpenCodebase cbInit "lowLevel" root SC.DoLock SC.DontMigrate action
  case result of
    Left e -> PT.putPrettyLn (P.shown e) *> pure (error "This really should have loaded")
    Right a -> pure a
