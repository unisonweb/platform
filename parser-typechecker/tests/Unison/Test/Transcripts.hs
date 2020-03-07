{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Unison.Prelude
import           EasyTest
import           Shellmet                       ( )
import           System.Directory
import           System.FilePath                ( (</>)
                                                , takeExtensions
                                                , takeBaseName
                                                )
import           System.Process                 ( readProcessWithExitCode )

import           Data.Text                      ( pack 
                                                , unpack
                                                )
import           Data.List

type TestBuilder = FilePath -> String -> Test ()

testBuilder :: FilePath -> String -> Test ()
testBuilder dir transcript = scope transcript $ do
  io $ "stack" ["exec", "unison", "--", "transcript", pack (dir </> transcript)]
  ok

testBuilder' :: FilePath -> String -> Test ()
testBuilder' dir transcript = scope transcript $ do
  let input = pack (dir </> transcript)
  let output = dir </> takeBaseName transcript <> ".output.md"
  io $ runAndCaptureError "stack" ["exec", "unison", "--", "transcript", input] output
  ok
  where 
    -- Given a command and arguments, run it and capture the standard error to a file
    -- regardless of success or failure.
    runAndCaptureError :: FilePath -> [Text] -> FilePath -> IO ()  
    runAndCaptureError cmd args outfile = do
      t <- readProcessWithExitCode cmd (map unpack args) ""
      let output = (\(_, _, stderr) -> stderr) t
      writeUtf8 outfile $ (pack . dropRunMessage) output

    -- Given the standard error, drops the part in the end that changes each run
    dropRunMessage :: String -> String  
    dropRunMessage = unlines . reverse . drop 3 . reverse . lines


buildTests :: TestBuilder -> FilePath -> Test ()
buildTests testBuilder dir = do 
  files <- io $ listDirectory dir
  let transcripts = filter (\f -> takeExtensions f == ".md") files
  tests (testBuilder dir <$> transcripts)

-- Transcripts that exit successfully get cleaned-up by the transcript parser.
-- Any remaining folders matching "transcript-.*" are output directories
-- of failed transcripts and should be moved under the "test-output" folder
cleanup :: Test ()
cleanup = do
  files' <- io $ listDirectory "."
  let dirs = filter ("transcript-" `isPrefixOf`) files'

  -- if any such codebases remain they are moved under test-output
  unless (null dirs) $ do
    io $ createDirectoryIfMissing True "test-output"
    io $ for_ dirs (\d -> renameDirectory d ("test-output" </> d))
    io
      . putStrLn
      . unlines
      $ [ ""
        , "NOTE: All transcript codebases have been moved into"
        , "the `test-output` directory. Feel free to delete it."
        ]

test :: Test ()
test = do

  buildTests testBuilder  $"unison-src" </> "transcripts"
  buildTests testBuilder' $"unison-src" </> "transcripts" </> "errors"
  cleanup

main :: IO ()
main = run test