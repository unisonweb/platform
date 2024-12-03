{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine.InputPattern
  ( InputPattern (..),
    ParameterDescription,
    ParameterType (..),
    Parameter,
    TrailingParameters (..),
    Parameters (..),
    Argument,
    Arguments,
    foldArgs,
    noParams,
    paramType,
    FZFResolver (..),
    Visibility (..),

    -- * Currently Unused
    minArgs,
    maxArgs,
    unionSuggestions,
    suggestionFallbacks,
  )
where

import Control.Lens
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import System.Console.Haskeline qualified as Line
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Input (Input (..))
import Unison.Codebase.Editor.StructuredArgument (StructuredArgument)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.FZFResolvers (FZFResolver (..))
import Unison.Prelude
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as P

data Visibility = Hidden | Visible
  deriving (Show, Eq, Ord)

-- | An argument to a command is either a string provided by the user which
-- needs to be parsed or a numbered argument that doesn’t need to be parsed, as
-- we’ve preserved its representation (although the numbered argument could
-- still be of the wrong type, which should result in an error).
type Argument = Either String StructuredArgument

type Arguments = [Argument]

-- | Argument description
-- It should fit grammatically into sentences like "I was expecting an argument for the <argDesc>"
-- e.g. "namespace to merge", "definition to delete", "remote target to push to" etc.
type ParameterDescription = Text

data InputPattern = InputPattern
  { patternName :: String,
    aliases :: [String],
    visibility :: Visibility, -- Allow hiding certain commands when debugging or work-in-progress
    params :: Parameters,
    help :: P.Pretty CT.ColorText,
    -- | Parse the arguments and return either an error message or a command `Input`.
    --
    --  __NB__: This function should return `Left` only on failure. For commands (like `help`) that simply produce
    --          formatted output, use `pure . Input.CreateMessage`. The failure output should be fully formatted (using
    --         `wrap`, etc.), but shouldn’t include any general error components like a warninng flag or the full help
    --          message, and shouldn’t plan for the context it is being output to (e.g., don’t `P.indentN` the entire
    --          message).
    parse ::
      Arguments ->
      Either (P.Pretty CT.ColorText) Input
  }

data ParameterType = ParameterType
  { typeName :: String,
    -- | Generate completion suggestions for this argument type
    suggestions ::
      forall m v a.
      (MonadIO m) =>
      String ->
      Codebase m v a ->
      AuthenticatedHttpClient ->
      PP.ProjectPath ->
      m [Line.Completion],
    -- | If an argument is marked as required, but not provided, the fuzzy finder will be triggered if
    -- available.
    fzfResolver :: Maybe FZFResolver
  }

type Parameter = (ParameterDescription, ParameterType)

data TrailingParameters
  = -- | Optional args followed by a possibly-empty catch-all
    Optional [Parameter] (Maybe Parameter)
  | -- | A catch-all that requires at least one value
    OnePlus Parameter

-- | The `Parameters` for an `InputPattern` are roughly
--
-- > [required …] ([optional …] [catchAll] | NonEmpty catchAll)
data Parameters = Parameters {requiredParams :: [Parameter], trailingParams :: TrailingParameters}

-- | Aligns the pattern parameters with a set of concrete arguments.
--
--   If too many arguments are provided, it returns the overflow arguments. In addition to the fold result, it returns
--  `Parameters` representing what can still be provided (e.g., via fuzzy completion). Note that if the result
--  `Parameters` has `OnePlus` or non-`null` `requiredArgs`, the application must fail unless more arguments are
--   provided somehow.
foldArgs :: (Parameter -> arg -> a -> a) -> a -> Parameters -> [arg] -> Either (NonEmpty arg) (Parameters, a)
foldArgs fn z Parameters {requiredParams, trailingParams} = foldRequiredArgs requiredParams
  where
    foldRequiredArgs = curry \case
      ([], as) -> foldTrailingArgs as
      (ps, []) -> pure (Parameters ps trailingParams, z)
      (p : ps, a : as) -> fmap (fn p a) <$> foldRequiredArgs ps as
    foldTrailingArgs = case trailingParams of
      Optional optParams zeroPlus -> foldOptionalArgs zeroPlus optParams
      OnePlus param -> foldOnePlusArgs param
    foldOptionalArgs zp = curry \case
      (ps, []) -> pure (Parameters [] $ Optional ps zp, z)
      ([], a : as) -> foldZeroPlusArgs zp $ a :| as
      (p : ps, a : as) -> fmap (fn p a) <$> foldOptionalArgs zp ps as
    foldZeroPlusArgs = maybe Left (\p -> pure . (Parameters [] . Optional [] $ pure p,) . foldr (fn p) z)
    foldOnePlusArgs p = \case
      [] -> pure (Parameters [] $ OnePlus p, z)
      args -> pure (Parameters [] . Optional [] $ pure p, foldr (fn p) z args)

noParams :: Parameters
noParams = Parameters [] $ Optional [] Nothing

-- `argType` gets called when the user tries to autocomplete an `i`th argument (zero-indexed).
-- todo: would be nice if we could alert the user if they try to autocomplete
-- past the end.  It would also be nice if
paramInfo :: Parameters -> Int -> Maybe (ParameterDescription, ParameterType)
paramInfo Parameters {requiredParams, trailingParams} i =
  if i < length requiredParams
    then pure $ requiredParams !! i
    else case trailingParams of
      Optional optParams zeroPlus ->
        let rem = i - length requiredParams
         in if rem < length optParams
              then pure $ optParams !! rem
              else zeroPlus
      OnePlus arg -> pure arg

-- `argType` gets called when the user tries to autocomplete an `i`th argument (zero-indexed).
-- todo: would be nice if we could alert the user if they try to autocomplete
-- past the end.  It would also be nice if
paramType :: Parameters -> Int -> Maybe ParameterType
paramType p = fmap snd . paramInfo p

minArgs :: Parameters -> Int
minArgs Parameters {requiredParams, trailingParams} =
  length requiredParams + case trailingParams of
    Optional _ _ -> 0
    OnePlus _ -> 1

maxArgs :: Parameters -> Maybe Int
maxArgs Parameters {requiredParams, trailingParams} =
  case trailingParams of
    Optional optParams Nothing -> pure $ length requiredParams + length optParams
    _ -> Nothing

-- | Union suggestions from all possible completions
unionSuggestions ::
  forall m v a.
  (MonadIO m) =>
  [ ( String ->
      Codebase m v a ->
      AuthenticatedHttpClient ->
      PP.ProjectPath ->
      m [Line.Completion]
    )
  ] ->
  ( String ->
    Codebase m v a ->
    AuthenticatedHttpClient ->
    PP.ProjectPath ->
    m [Line.Completion]
  )
unionSuggestions suggesters inp codebase httpClient path = do
  suggesters & foldMapM \suggester ->
    suggester inp codebase httpClient path
      & fmap List.nubOrd

-- | Try the first completer, if it returns no suggestions, try the second, etc.
suggestionFallbacks ::
  forall m v a.
  (MonadIO m) =>
  [ ( String ->
      Codebase m v a ->
      AuthenticatedHttpClient ->
      PP.ProjectPath ->
      m [Line.Completion]
    )
  ] ->
  ( String ->
    Codebase m v a ->
    AuthenticatedHttpClient ->
    PP.ProjectPath ->
    m [Line.Completion]
  )
suggestionFallbacks suggesters inp codebase httpClient path = go suggesters
  where
    go (s : rest) = do
      suggestions <- s inp codebase httpClient path
      if null suggestions
        then go rest
        else pure suggestions
    go [] = pure []
