{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine
  ( ParseFailure (..),
    ExpansionFailure (..),
    FZFResolveFailure (..),
    allow,
    parseInput,
    prompt,
    reportParseFailure,
    watchFileSystem,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Lens hiding (aside)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
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
import Unison.Codebase.Editor.StructuredArgument (StructuredArgument)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Watch qualified as Watch
import Unison.CommandLine.FZFResolvers qualified as FZFResolvers
import Unison.CommandLine.FuzzySelect qualified as Fuzzy
import Unison.CommandLine.Helpers (warn)
import Unison.CommandLine.InputPattern (InputPattern (..))
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
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

data ExpansionFailure
  = TooManyArguments (NonEmpty InputPattern.Argument)
  | UnexpectedStructuredArgument StructuredArgument

-- | Expanding numbers is a bit complicated. Each `Parameter` expects either structured or “unstructured” arguments. So
--   we iterate over the parameters, if it doesn’t want structured, we just preserve the string. If it does want
--   structured, we have to expand the argument, which may result in /multiple/ structured arguments. We take the first
--   one for the param and pass the rest along. Now, if the next param wants unstructured, but we’ve already structured
--   it, then we’ve got an error.
expandArguments ::
  NumberedArgs ->
  InputPattern.Parameters ->
  [String] ->
  Either ExpansionFailure (InputPattern.Arguments, InputPattern.Parameters)
expandArguments numberedArgs params =
  bimap TooManyArguments (first $ reverse)
    <=< InputPattern.foldParamsWithM
      ( \acc (_, param) arg ->
          if InputPattern.isStructured param
            then
              pure $
                either
                  ( maybe (arg : acc, []) (maybe (acc, []) (\(h :| t) -> (h : acc, t)) . nonEmpty . fmap pure)
                      . expandNumber numberedArgs
                  )
                  ((,[]) . (: acc) . pure)
                  arg
            else (,[]) . (: acc) <$> either (pure . Left) (Left . UnexpectedStructuredArgument) arg
      )
      []
      params
    . fmap Left

data ParseFailure
  = NoCommand
  | UnknownCommand String
  | ExpansionFailure String InputPattern ExpansionFailure
  | FZFResolveFailure InputPattern FZFResolveFailure
  | SubParseFailure String InputPattern (P.Pretty P.ColorText)

-- |
--
--  __TODO__: Move this closer to `main`, but right now it’s shared by @ucm@ and @transcripts@, so this is the closest
--            we can get without duplicating it.
reportParseFailure :: ParseFailure -> P.Pretty P.ColorText
reportParseFailure = \case
  NoCommand -> ""
  UnknownCommand command ->
    warn . P.wrap $
      "I don't know how to"
        <> P.group (fromString command <> ".")
        <> "Type"
        <> IP.makeExample' IP.help
        <> "or `?` to get help."
  ExpansionFailure command pat@InputPattern {params} ef -> case ef of
    TooManyArguments extraArgs ->
      let showNum n = fromMaybe (tShow n) $ Numeral.us_cardinal defaultInflection n
       in wrapFailure command pat
            . P.text
            . maybe
              ( "Internal error: fuzzy finder complained that there are "
                  <> showNum (length extraArgs)
                  <> " too many arguments provided, but the command apparently allows an unbounded number of arguments."
              )
              ( \maxCount ->
                  let foundCount = showNum $ maxCount + length extraArgs
                   in case maxCount of
                        0 -> "I expected no arguments, but received " <> foundCount <> "."
                        _ -> "I expected no more than " <> showNum maxCount <> " arguments, but received " <> foundCount <> "."
              )
            $ InputPattern.maxArgs params
    UnexpectedStructuredArgument _arg -> "Internal error: Expected a String, but got a structured argument instead."
  FZFResolveFailure pat frf -> case frf of
    NoFZFResolverForArgumentType _argDesc -> InputPattern.help pat
    NoFZFOptions argDesc ->
      P.callout "⚠️" $
        "Sorry, I was expecting an argument for the " <> P.text argDesc <> ", and I couldn't find any to suggest to you. 😅"
  SubParseFailure command pat msg -> wrapFailure command pat msg
  where
    wrapFailure command pat msg =
      P.warnCallout $
        P.lines
          [ P.wrap "Sorry, I wasn’t sure how to process your request:",
            "",
            P.indentN 2 msg,
            "",
            P.wrap $
              "You can run"
                <> IP.makeExample IP.help [fromString command]
                <> "for more information on using"
                <> IP.makeExampleEOS pat []
          ]

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
  IO (Either ParseFailure (Maybe (InputPattern.Arguments, Input)))
parseInput codebase projPath currentProjectRoot numberedArgs patterns segments = runExceptT do
  let getCurrentBranch0 :: IO (Branch0 IO)
      getCurrentBranch0 = do
        projRoot <- currentProjectRoot
        pure . Branch.head $ Branch.getAt' (projPath ^. PP.path_) projRoot

  case segments of
    [] -> throwE NoCommand
    command : args -> case Map.lookup command patterns of
      Just pat@(InputPattern {params, parse}) -> do
        (expandedArgs, remainingParams) <-
          except . first (ExpansionFailure command pat) $ expandArguments numberedArgs params args
        lift (fzfResolve codebase projPath getCurrentBranch0 remainingParams)
          >>= either
            (throwE . FZFResolveFailure pat)
            ( traverse \resolvedArgs ->
                let allArgs = expandedArgs <> resolvedArgs
                 in except . bimap (SubParseFailure command pat) (Left command : allArgs,) $ parse allArgs
            )
      Nothing -> throwE $ UnknownCommand command

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: NumberedArgs -> String -> Maybe NumberedArgs
expandNumber numberedArgs s =
  catMaybes . fmap ((vargs Vector.!?) . pred) <$> expandedNumber
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
            ("", "", [from, to]) -> enumFromTo <$> readMay from <*> readMay to
            (_, _, _) -> Nothing

data FZFResolveFailure
  = NoFZFResolverForArgumentType InputPattern.ParameterDescription
  | NoFZFOptions
      -- | argument description
      Text

fzfResolve ::
  Codebase IO Symbol Ann ->
  PP.ProjectPath ->
  (IO (Branch0 IO)) ->
  InputPattern.Parameters ->
  IO (Either FZFResolveFailure (Maybe InputPattern.Arguments))
fzfResolve codebase ppCtx getCurrentBranch InputPattern.Parameters {requiredParams, trailingParams} = runExceptT do
  -- We build up a list of `ExceptT` inside an outer `ExceptT` to allow us to fail immediately if /any/ required
  -- argument is missing a resolver, before we start prompting the user to actually do a fuzzy search. Otherwise, we
  -- might ask the user to perform a search only to realize we don't have a resolver for a later arg.
  argumentResolvers :: [MaybeT (ExceptT FZFResolveFailure IO) (NonEmpty InputPattern.Argument)] <-
    liftA2 (<>) (traverse (maybeFillArg False) requiredParams) case trailingParams of
      InputPattern.Optional _ _ -> pure mempty
      InputPattern.OnePlus p -> pure <$> maybeFillArg True p
  runMaybeT $ foldM (\bs -> ((bs <>) . toList <$>)) [] argumentResolvers
  where
    maybeFillArg allowMulti (argName, InputPattern.ParameterType {fzfResolver}) =
      maybe
        (throwError $ NoFZFResolverForArgumentType argName)
        (pure . fuzzyFillArg allowMulti argName)
        fzfResolver
    fuzzyFillArg ::
      Bool -> Text -> InputPattern.FZFResolver -> MaybeT (ExceptT FZFResolveFailure IO) (NonEmpty InputPattern.Argument)
    fuzzyFillArg allowMulti argDesc InputPattern.FZFResolver {getOptions} = MaybeT do
      currentBranch <- Branch.withoutTransitiveLibs <$> liftIO getCurrentBranch
      options <- liftIO $ getOptions codebase ppCtx currentBranch
      when (null options) . throwError $ NoFZFOptions argDesc
      liftIO $ Text.putStrLn (FZFResolvers.fuzzySelectHeader argDesc)
      results <- liftIO (Fuzzy.fuzzySelect Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = allowMulti} id options)
      -- If the user triggered the fuzzy finder, but selected nothing, abort the command rather than continuing
      -- execution with no arguments.
      pure $ fmap (Left . Text.unpack <$>) . nonEmpty =<< results

prompt :: String
prompt = "> "
