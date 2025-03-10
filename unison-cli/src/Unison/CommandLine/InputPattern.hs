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
    noParams,
    foldParamsWithM,
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

-- | This should fit grammatically into sentences like “I was expecting an argument for the <paramDesc>”.
--   E.g. “namespace to merge”, “definition to delete”, “remote target to push to” etc.
type ParameterDescription = Text

data InputPattern = InputPattern
  { patternName :: String,
    aliases :: [String],
    -- | Allow hiding certain commands when debugging or work-in-progress
    visibility :: Visibility,
    params :: Parameters,
    help :: P.Pretty CT.ColorText,
    -- | Parse the arguments and return either an error message or a command `Input`.
    --
    --   The input list is always a valid length for the pattern. It may be necessary to have a catch-all case for
    --   coverage, but the implementation can assume that, say, a `OnePlus` parameter will always be provided at least
    --   one argument.
    --
    --  __NB__: This function should return `Left` only on failure. For commands (like `help`) that simply produce
    --          formatted output, use `pure . Input.CreateMessage`. The failure output should be fully formatted (using
    --         `wrap`, etc.), but shouldn’t include any general error components like a warning flag or the full help
    --          message, and shouldn’t plan for the context it is being output to (e.g., don’t `P.indentN` the entire
    --          message).
    parse :: Arguments -> Either (P.Pretty CT.ColorText) Input
  }

data ParameterType = ParameterType
  { typeName :: String,
    -- | Generate completion suggestions for this parameter type
    suggestions ::
      forall m v a.
      (MonadIO m) =>
      String ->
      Codebase m v a ->
      AuthenticatedHttpClient ->
      PP.ProjectPath ->
      m [Line.Completion],
    -- | If a parameter is marked as required, but no argument is provided, the fuzzy finder will be triggered if
    -- available.
    fzfResolver :: Maybe FZFResolver,
    isStructured :: Bool
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

-- | This is the parameter structure for a pattern that doesn’t accept any arguments.
noParams :: Parameters
noParams = Parameters [] $ Optional [] Nothing

-- | Applies concrete arguments to a set of `Parameters`.
foldParamsWithM ::
  (Monad m) =>
  -- | Each step needs to return a new incremental result, but can also return additional arguments to apply in later
  --   steps. This allows for the expansion of an argument to multiple arguments, as with numbered arg ranges.
  (state -> Parameter -> arg -> m (state, [arg])) ->
  -- | The initial state.
  state ->
  Parameters ->
  [arg] ->
  -- | If too many arguments are provided, it returns `Left`, with the arguments that couldn’t be assigned to a
  --   parameter. Otherwise, it returns a tuple of the `Parameters` that could still be applied to additional arguments
  --   (e.g., via fuzzy completion) and the final result. If the returned `Parameters` has remaining required arguments,
  --   they must either be provided somehow (e.g., another call to this function or fuzzy completion) or result in a
  --   “not enough arguments” error.
  m (Either (NonEmpty arg) (state, Parameters))
foldParamsWithM fn z Parameters {requiredParams, trailingParams} = foldRequiredArgs z requiredParams
  where
    foldRequiredArgs res = curry \case
      ([], as) -> case trailingParams of
        Optional optParams zeroPlus -> foldOptionalArgs res zeroPlus optParams as
        OnePlus param -> case as of
          [] -> pure $ pure (res, Parameters [] $ OnePlus param)
          a : args -> foldCatchallArg res param $ a :| args
      (ps, []) -> pure $ pure (res, Parameters ps trailingParams)
      (p : ps, a : as) -> do
        (res', extraArgs) <- fn res p a
        foldRequiredArgs res' ps $ extraArgs <> as
    foldOptionalArgs res zp = curry \case
      (ps, []) -> pure $ pure (res, Parameters [] $ Optional ps zp)
      ([], a : as) -> maybe (pure . Left) (foldCatchallArg res) zp $ a :| as
      (p : ps, a : as) -> do
        (res', extraArgs) <- fn res p a
        foldOptionalArgs res' zp ps $ extraArgs <> as
    foldCatchallArg res p =
      let collectRemainingArgs prevRes = \case
            [] -> pure $ pure (prevRes, Parameters [] . Optional [] $ pure p)
            a : args -> do
              (res', extraArgs) <- fn prevRes p a
              collectRemainingArgs res' $ extraArgs <> args
       in collectRemainingArgs res . toList

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

-- | `argType` gets called when the user tries to autocomplete an `i`th argument (zero-indexed).
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
