{-# LANGUAGE OverloadedStrings #-}

-- | Command-line fuzzy selection of arbitrary values.
--     Shells out to fzf for the actual selection.
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect,
    Options (..),
    defaultOptions,
  )
where

import Control.Monad.Except (runExceptT, throwError)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Handle (hDuplicateTo)
import System.IO (BufferMode (NoBuffering), hPutStrLn, stderr)
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)
import UnliftIO.Exception (bracket)
import UnliftIO.IO (hGetBuffering, hSetBuffering, stdin)
import UnliftIO.Process qualified as Proc

-- | Fuzzy Selection options
data Options = Options
  { allowMultiSelect :: Bool
  }

-- | Default 'Options'
defaultOptions :: Options
defaultOptions =
  Options
    { allowMultiSelect = True
    }

-- | Convert options into command-line args for fzf
optsToArgs :: Options -> [String]
optsToArgs opts =
  defaultArgs <> case opts of
    Options {allowMultiSelect = True} -> ["-m"]
    _ -> []
  where
    defaultArgs =
      -- Don't show or match on the first column of input.
      -- This allows us to prepend each line with a number, and use that number to determine
      -- which values from the input list were selected.
      [ "--with-nth",
        "2..",
        -- Use only half the screen (it's nice to see what you were working on when searching)
        "--height=50%",
        -- But if 50% of the screen is too small, ensure show at least 10 results.
        "--min-height=10"
      ]

-- | Allows prompting the user to interactively fuzzy-select a result from a list of options, currently shells out to `fzf` under the hood.
-- If fzf is missing, or an error (other than ctrl-c) occurred, returns Nothing.
fuzzySelect :: forall a. Options -> (a -> Text) -> [a] -> IO (Maybe [a])
fuzzySelect opts intoSearchText choices =
  UnliftIO.handleAny handleException
    . handleError
    . restoreBuffering
    . runExceptT
    $ do
      fzfPath <-
        liftIO (findExecutable "fzf") >>= \case
          Nothing -> throwError "I couldn't find the `fzf` executable on your path, consider installing `fzf` to enable fuzzy searching."
          Just fzfPath -> pure fzfPath
      let fzfArgs :: [String] =
            optsToArgs opts
      let numberedChoices :: [(Int, a)] =
            zip [0 ..] choices
      let searchTexts :: [Text] =
            (\(n, ch) -> tShow (n) <> " " <> intoSearchText ch) <$> numberedChoices
      let fzfProc :: Proc.CreateProcess =
            (Proc.proc fzfPath fzfArgs)
              { Proc.std_in = Proc.CreatePipe,
                Proc.std_out = Proc.CreatePipe,
                Proc.delegate_ctlc = True
              }
      (Just stdin', Just stdout', _, procHandle) <- Proc.createProcess fzfProc
      -- Generally no-buffering is helpful for highly interactive processes.
      hSetBuffering stdin NoBuffering
      hSetBuffering stdin' NoBuffering
      result <- liftIO . UnliftIO.tryAny $ do
        -- Dump the search terms into fzf's stdin
        traverse_ (Text.hPutStrLn stdin') searchTexts
        -- Wire up the interactive terminal to fzf now that the inputs have been loaded.
        hDuplicateTo stdin stdin'
        void $ Proc.waitForProcess procHandle
        Text.lines <$> liftIO (Text.hGetContents stdout')
      -- Since we prefixed every search term with its number earlier, we know each result
      -- is prefixed with a number, we need to parse it and use it to select the matching
      -- value from our input list.
      pure $ case result of
        Left _ -> Nothing
        Right selections ->
          selections
            & mapMaybe (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
            & Set.fromList
            & ( \selectedNumbers ->
                  numberedChoices
                    & mapMaybe (\(n, a) -> if n `Set.member` selectedNumbers then Just a else Nothing)
              )
            & Just
  where
    handleException :: SomeException -> IO (Maybe [a])
    handleException err = traceShowM err *> hPutStrLn stderr "Oops, something went wrong. No input selected." *> pure Nothing
    handleError :: IO (Either Text (Maybe [a])) -> IO (Maybe [a])
    handleError m =
      m >>= \case
        Left err -> Text.hPutStrLn stderr err *> pure Nothing
        Right as -> pure as
    restoreBuffering :: IO c -> IO c
    restoreBuffering action =
      bracket (hGetBuffering stdin) (hSetBuffering stdin) (const action)
