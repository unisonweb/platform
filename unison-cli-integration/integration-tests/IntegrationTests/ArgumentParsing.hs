{-# LANGUAGE OverloadedStrings #-}

module IntegrationTests.ArgumentParsing where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Time (diffUTCTime, getCurrentTime)
import EasyTest
import System.Directory qualified
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.Printf

integrationTestsDir :: FilePath
integrationTestsDir = "unison-cli-integration" </> "integration-tests" </> "IntegrationTests"

uFile :: FilePath
uFile = integrationTestsDir </> "print.u"

ucFile :: FilePath
ucFile = integrationTestsDir </> "main.uc"

transcriptFile :: FilePath
transcriptFile = integrationTestsDir </> "transcript.md"

unisonCmdString :: String
unisonCmdString =
  unlines
    [ "print : '{IO, Exception} ()",
      "print _ = base.io.printLine \"ok\""
    ]

tempCodebase :: FilePath
tempCodebase = "tempcodebase"

test :: Test ()
test = do
  let ucm = "unison"
  EasyTest.using (pure ()) clearTempCodebase \_ ->
    scope "argument-parsing" . tests $
      [ expectExitCode ExitSuccess ucm ["--help"] "",
        expectExitCode ExitSuccess ucm ["-h"] "",
        expectExitCode ExitSuccess ucm ["version", "--help"] "",
        expectExitCode ExitSuccess ucm ["init", "--help"] "",
        expectExitCode ExitSuccess ucm ["run", "--help"] "",
        expectExitCode ExitSuccess ucm ["run.file", "--help"] "",
        expectExitCode ExitSuccess ucm ["run.pipe", "--help"] "",
        expectExitCode ExitSuccess ucm ["transcript", "--help"] "",
        expectExitCode ExitSuccess ucm ["transcript.fork", "--help"] "",
        expectExitCode ExitSuccess ucm ["headless", "--help"] "",
        expectExitCode ExitSuccess ucm ["version"] "",
        -- , expectExitCode ExitSuccess ucm ["run"] "" -- how?
        expectExitCode ExitSuccess ucm ["run.file", uFile, "print", "--codebase-create", tempCodebase] "",
        expectExitCode ExitSuccess ucm ["run.pipe", "print", "--codebase-create", tempCodebase] unisonCmdString,
        expectExitCode ExitSuccess ucm ["transcript", transcriptFile, "--codebase-create", tempCodebase] "",
        expectExitCode ExitSuccess ucm ["transcript.fork", transcriptFile, "--codebase-create", tempCodebase] "",
        -- , expectExitCode ExitSuccess ucm ["headless"] "" -- ?
        -- options
        expectExitCode ExitSuccess ucm ["--port", "8000", "--codebase-create", tempCodebase] "exit",
        expectExitCode ExitSuccess ucm ["--host", "localhost", "--codebase-create", tempCodebase] "exit",
        expectExitCode ExitSuccess ucm ["--token", "MY_TOKEN", "--codebase-create", tempCodebase] "exit", -- ?
        expectExitCode ExitSuccess ucm ["--codebase-create", tempCodebase] "exit",
        expectExitCode ExitSuccess ucm ["--ui", tempCodebase, "--codebase-create", tempCodebase] "exit",
        scope "can compile, then run compiled artifact" $
          tests
            [ expectExitCode ExitSuccess ucm ["transcript", transcriptFile] "",
              expectExitCode ExitSuccess ucm ["run.compiled", ucFile] ""
            ]
      ]

expectExitCode :: ExitCode -> FilePath -> [String] -> String -> Test ()
expectExitCode expected cmd args stdin = scope (intercalate " " (cmd : args)) do
  start <- io $ getCurrentTime
  (code, _, stdErr) <- io $ readProcessWithExitCode cmd args stdin
  end <- io $ getCurrentTime
  let diff = diffUTCTime end start
  note $ printf "\n[Time: %s sec]" $ show diff
  when (code /= expected) do
    note ("stderr:\n" <> stdErr)
  expectEqual code expected

defaultArgs :: [String]
defaultArgs = ["--codebase-create", tempCodebase]

clearTempCodebase :: () -> IO ()
clearTempCodebase _ =
  System.Directory.removePathForcibly tempCodebase
