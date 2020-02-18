{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.Main where

import Unison.Prelude

import Control.Concurrent.STM (atomically)
import Control.Exception (finally, catch, AsyncException(UserInterrupt), asyncExceptionFromException)
import Control.Monad.State (runStateT)
import Data.IORef
import Prelude hiding (readFile, writeFile)
import System.IO.Error (catchIOError)
import System.Exit (die)
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Input (Input (..), Event)
import qualified Unison.Codebase.Editor.HandleInput as HandleInput
import qualified Unison.Codebase.Editor.HandleCommand as HandleCommand
import Unison.Codebase.Editor.Command (LoadSourceResult(..))
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase (Codebase)
import Unison.CommandLine
import Unison.PrettyTerminal
import Unison.CommandLine.InputPattern (ArgumentType (suggestions), InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyUser, notifyNumbered, shortenDirectory)
import Unison.Parser (Ann)
import Unison.Var (Var)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified System.Console.Haskeline as Line
import System.IO.Error (isDoesNotExistError)
import qualified Crypto.Random        as Random   
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase as Codebase
import qualified Unison.CommandLine.InputPattern as IP
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q
import Text.Regex.TDFA

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: [String] -> String -> [String]
expandNumber numberedArgs s =
  maybe [s]
        (map (\i -> fromMaybe (show i) . atMay numberedArgs $ i - 1))
        expandedNumber
 where
  rangeRegex = "([0-9]+)-([0-9]+)" :: String
  (junk,_,moreJunk, ns) =
    s =~ rangeRegex :: (String, String, String, [String])
  expandedNumber =
    case readMay s of
      Just i -> Just [i]
      Nothing ->
        -- check for a range
        case (junk, moreJunk, ns) of
          ("", "", [from, to]) ->
            (\x y -> [x..y]) <$> readMay from <*> readMay to
          _ -> Nothing

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch m
  -> Path.Absolute
  -> [String]
  -> m Input
getUserInput patterns codebase branch currentPath numberedArgs =
  Line.runInputT settings go
 where
  go = do
    line <- Line.getInputLine $
      P.toANSI 80 ((P.green . P.shown) currentPath <> fromString prompt)
    case line of
      Nothing -> pure QuitI
      Just l ->
        case words l of
          [] -> go
          ws ->
            case parseInput patterns . (>>= expandNumber numberedArgs) $ ws  of
              Left msg -> do
                liftIO $ putPrettyLn msg
                go
              Right i -> pure i
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure . exactComplete word $ Map.keys patterns
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p       <- Map.lookup h patterns
          argType <- IP.argType p (length t)
          pure $ suggestions argType word codebase branch currentPath
        _ -> pure []

asciiartUnison :: P.Pretty P.ColorText
asciiartUnison =
  P.red " _____"
    <> P.hiYellow "     _             "
    <> P.newline
    <> P.red "|  |  |"
    <> P.hiRed "___"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___ "
    <> P.cyan "___ "
    <> P.purple "___ "
    <> P.newline
    <> P.red "|  |  |   "
    <> P.hiYellow "| |"
    <> P.hiGreen "_ -"
    <> P.cyan "| . |"
    <> P.purple "   |"
    <> P.newline
    <> P.red "|_____|"
    <> P.hiRed "_|_"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___"
    <> P.cyan "|___|"
    <> P.purple "_|_|"

welcomeMessage :: FilePath -> P.Pretty P.ColorText
welcomeMessage dir =
  asciiartUnison
    <> P.newline
    <> P.newline
    <> P.linesSpaced
         [ P.wrap "Welcome to Unison!"
         , P.wrap
           (  "I'm currently watching for changes to .u files under "
           <> (P.group . P.blue $ fromString dir)
           )
         , P.wrap ("Type " <> P.hiBlue "help" <> " to get help. 😎")
         ]

hintFreshCodebase :: P.Pretty P.ColorText
hintFreshCodebase =
  P.wrap $ "Enter " <> P.hiBlue "pull https://github.com/unisonweb/base .base"
    <> "to set up the default base library. 🏗"

main
  :: forall v
  . Var v
  => FilePath
  -> Path.Absolute
  -> FilePath
  -> [Either Event Input]
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir initialPath configFile initialInputs startRuntime codebase = do
  dir' <- shortenDirectory dir
  root <- Codebase.getRootBranch codebase
  putPrettyLn $ if Branch.isOne root
    then welcomeMessage dir' <> P.newline <> P.newline <> hintFreshCodebase
    else welcomeMessage dir'
  eventQueue <- Q.newIO
  do
    runtime                  <- startRuntime
    -- we watch for root branch tip changes, but want to ignore ones we expect.
    rootRef                  <- newIORef root
    pathRef                  <- newIORef initialPath
    initialInputsRef         <- newIORef initialInputs
    numberedArgsRef          <- newIORef []
    pageOutput               <- newIORef True
    (config, cancelConfig)   <-
      catchIOError (watchConfig configFile) $ \_ ->
        die "Your .unisonConfig could not be loaded. Check that it's correct!"
    cancelFileSystemWatch    <- watchFileSystem eventQueue dir
    cancelWatchBranchUpdates <- watchBranchUpdates (Branch.headHash <$>
                                                      readIORef rootRef)
                                                   eventQueue
                                                   codebase
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> (patternName p, p) : ((, p) <$> aliases p))
        getInput = do
          root <- readIORef rootRef
          path <- readIORef pathRef
          numberedArgs <- readIORef numberedArgsRef
          getUserInput patternMap codebase root path numberedArgs
        loadSourceFile :: Text -> IO LoadSourceResult
        loadSourceFile fname =
          if allow $ Text.unpack fname
            then
              let handle :: IOException -> IO LoadSourceResult
                  handle e =
                    case e of
                      _ | isDoesNotExistError e -> return InvalidSourceNameError
                      _ -> return LoadError
                  go = do
                    contents <- Data.Text.IO.readFile $ Text.unpack fname
                    return $ LoadSuccess contents
                  in catch go handle
            else return InvalidSourceNameError
        notify = notifyUser dir >=> (\o ->
          ifM (readIORef pageOutput)
              (putPrettyNonempty o)
              (putPrettyLnUnpaged o))
    let
      awaitInput = do
        -- use up buffered input before consulting external events
        i <- readIORef initialInputsRef
        (case i of
          h:t -> writeIORef initialInputsRef t >> pure h
          [] ->
            -- Race the user input and file watch.
            Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
              Left _ -> do
                let e = Left <$> atomically (Q.dequeue eventQueue)
                writeIORef pageOutput False
                e
              x      -> do
                writeIORef pageOutput True
                pure x) `catch` interruptHandler
      interruptHandler (asyncExceptionFromException -> Just UserInterrupt) = awaitInput
      interruptHandler _ = pure $ Right QuitI
      cleanup = do
        Runtime.terminate runtime
        cancelConfig
        cancelFileSystemWatch
        cancelWatchBranchUpdates
      loop state = do
        writeIORef pathRef (HandleInput._currentPath state)
        let free = runStateT (runMaybeT HandleInput.loop) state
        
        (o, state') <- HandleCommand.commandLine config awaitInput
                                     (writeIORef rootRef)
                                     runtime
                                     notify
                                     (\o -> let (p, args) = notifyNumbered o in
                                      putPrettyNonempty p $> args)
                                     loadSourceFile
                                     codebase
                                     (const Random.getSystemDRG)
                                     free
        case o of
          Nothing -> pure ()
          Just () -> do
            writeIORef numberedArgsRef (HandleInput._numberedArgs state')
            loop state'
    (`finally` cleanup)
      $ loop (HandleInput.loopState0 root initialPath)
