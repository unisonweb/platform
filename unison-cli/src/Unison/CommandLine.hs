{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine
  ( allow,
    parseInput,
    prompt,
    watchFileSystem,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Lens hiding (aside)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import System.FilePath (takeFileName)
import Text.Numeral (defaultInflection)
import Text.Numeral.Language.ENG qualified as Numeral
import Text.Regex.TDFA ((=~))
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input (Event (..), Input (..))
import Unison.Codebase.Editor.Output (NumberedArgs)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Watch qualified as Watch
import Unison.CommandLine.FZFResolvers qualified as FZFResolvers
import Unison.CommandLine.FuzzySelect qualified as Fuzzy
import Unison.CommandLine.Helpers (warn)
import Unison.CommandLine.InputPattern (InputPattern (..))
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as IPs
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as P
import Unison.Util.TQueue qualified as Q
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

allow :: FilePath -> Bool
allow p =
  -- ignore Emacs .# prefixed files, see https://github.com/unisonweb/unison/issues/457
  not (".#" `isPrefixOf` takeFileName p)
    && (isSuffixOf ".u" p || isSuffixOf ".uu" p)

watchFileSystem :: Q.TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

parseInput ::
  Codebase IO Symbol Ann ->
  -- | Current location
  PP.ProjectPath ->
  IO (Branch.Branch IO) ->
  -- | Numbered arguments
  NumberedArgs ->
  -- | Input Pattern Map
  Map String InputPattern ->
  -- | command:arguments
  [String] ->
  -- Returns either an error message or the fully expanded arguments list and parsed input.
  -- If the output is `Nothing`, the user cancelled the input (e.g. ctrl-c)
  IO (Either (P.Pretty CT.ColorText) (Maybe (InputPattern.Arguments, Input)))
parseInput codebase projPath currentProjectRoot numberedArgs patterns segments = runExceptT do
  let getCurrentBranch0 :: IO (Branch0 IO)
      getCurrentBranch0 = do
        projRoot <- currentProjectRoot
        pure . Branch.head $ Branch.getAt' (projPath ^. PP.path_) projRoot

  case segments of
    [] -> throwE ""
    command : args -> case Map.lookup command patterns of
      Just pat@(InputPattern {params, help, parse}) -> do
        let expandedNumbers :: InputPattern.Arguments
            expandedNumbers =
              foldMap (\arg -> maybe [Left arg] (fmap pure) $ expandNumber numberedArgs arg) args
        lift (fzfResolve codebase projPath getCurrentBranch0 params expandedNumbers)
          >>= either
            ( \case
                NoFZFResolverForArgumentType _argDesc -> throwError help
                NoFZFOptions argDesc -> throwError (noCompletionsMessage argDesc)
                FZFCancelled -> pure Nothing
                FZFOversaturated extraArgs -> do
                  let showNum n = fromMaybe (tShow n) $ Numeral.us_cardinal defaultInflection n
                  maxCount <- maybe (throwError . P.text $ "Internal error: fuzzy finder complained that there are " <> showNum (length extraArgs) <> " too many arguments provided, but the command apparently allows an unbounded number of arguments.") pure $ InputPattern.maxArgs params
                  let foundCount = showNum $ maxCount + length extraArgs
                  throwError . P.text $
                    "I expected no more than " <> showNum maxCount <> " arguments, but received " <> foundCount <> "."
            )
            ( \resolvedArgs -> do
                parsedInput <-
                  except
                    . first
                      ( \msg ->
                          P.warnCallout $
                            P.wrap "Sorry, I wasn’t sure how to process your request:"
                              <> P.newline
                              <> P.newline
                              <> P.indentN 2 msg
                              <> P.newline
                              <> P.newline
                              <> P.wrap
                                ( "You can run"
                                    <> IPs.makeExample IPs.help [fromString command]
                                    <> "for more information on using"
                                    <> IPs.makeExampleEOS pat []
                                )
                      )
                    $ parse resolvedArgs
                pure $ Just (Left command : resolvedArgs, parsedInput)
            )
      Nothing ->
        throwE
          . warn
          . P.wrap
          $ "I don't know how to"
            <> P.group (fromString command <> ".")
            <> "Type"
            <> IPs.makeExample' IPs.help
            <> "or `?` to get help."
  where
    noCompletionsMessage argDesc =
      P.callout "⚠️" $
        P.lines
          [ ( "Sorry, I was expecting an argument for the "
                <> P.text argDesc
                <> ", and I couldn't find any to suggest to you. 😅"
            )
          ]

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: NumberedArgs -> String -> Maybe NumberedArgs
expandNumber numberedArgs s =
  (\nums -> [arg | i <- nums, Just arg <- [vargs Vector.!? (i - 1)]]) <$> expandedNumber
  where
    vargs = Vector.fromList numberedArgs
    rangeRegex = "([0-9]+)-([0-9]+)" :: String
    (junk, _, moreJunk, ns) =
      s =~ rangeRegex :: (String, String, String, [String])
    expandedNumber =
      case readMay s of
        Just i -> Just [i]
        Nothing ->
          -- check for a range
          case (junk, moreJunk, ns) of
            ("", "", [from, to]) ->
              (\x y -> [x .. y]) <$> readMay from <*> readMay to
            _ -> Nothing

data FZFResolveFailure
  = NoFZFResolverForArgumentType InputPattern.ParameterDescription
  | NoFZFOptions
      -- | argument description
      Text
  | FZFCancelled
  | -- | More arguments were provided than the command supports.
    FZFOversaturated
      -- | The arguments that couldn’t be assigned to a parameter.
      (NonEmpty InputPattern.Argument)

fzfResolve ::
  Codebase IO Symbol Ann ->
  PP.ProjectPath ->
  (IO (Branch0 IO)) ->
  InputPattern.Parameters ->
  InputPattern.Arguments ->
  IO (Either FZFResolveFailure InputPattern.Arguments)
fzfResolve codebase ppCtx getCurrentBranch params args = runExceptT do
  -- We resolve args in two steps, first we check that all arguments that will require a fzf
  -- resolver have one, and only if so do we prompt the user to actually do a fuzzy search.
  -- Otherwise, we might ask the user to perform a search only to realize we don't have a resolver
  -- for a later arg.
  let argumentResolvers :: [ExceptT FZFResolveFailure IO InputPattern.Arguments] =
        either
          (pure . throwError . FZFOversaturated)
          ( \(InputPattern.Parameters {requiredParams, trailingParams}, args) ->
              args
                <> map (meh False) requiredParams
                <> case trailingParams of
                  InputPattern.Optional _ _ -> mempty
                  InputPattern.OnePlus p -> pure $ meh True p
          )
          $ InputPattern.foldArgs (\(_, _) arg acc -> pure [arg] : acc) mempty params args
  argumentResolvers & foldMapM id
  where
    meh :: Bool -> InputPattern.Parameter -> ExceptT FZFResolveFailure IO InputPattern.Arguments
    meh allowMulti (argName, InputPattern.ParameterType {fzfResolver}) =
      maybe
        (throwError $ NoFZFResolverForArgumentType argName)
        (fuzzyFillArg allowMulti argName)
        fzfResolver

    fuzzyFillArg :: Bool -> Text -> InputPattern.FZFResolver -> ExceptT FZFResolveFailure IO InputPattern.Arguments
    fuzzyFillArg allowMulti argDesc InputPattern.FZFResolver {getOptions} = do
      currentBranch <- Branch.withoutTransitiveLibs <$> liftIO getCurrentBranch
      options <- liftIO $ getOptions codebase ppCtx currentBranch
      when (null options) . throwError $ NoFZFOptions argDesc
      liftIO $ Text.putStrLn (FZFResolvers.fuzzySelectHeader argDesc)
      results <-
        liftIO (Fuzzy.fuzzySelect Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = allowMulti} id options)
          `whenNothingM` throwError FZFCancelled
      -- If the user triggered the fuzzy finder, but selected nothing, abort the command rather than continuing execution
      -- with no arguments.
      if null results
        then throwError FZFCancelled
        else pure (Left . Text.unpack <$> results)

prompt :: String
prompt = "> "
