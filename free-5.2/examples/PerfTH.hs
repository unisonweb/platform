{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.CPUTime.Rdtsc
import System.IO.Unsafe
import Data.IORef
import Data.Word
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Fail as Fail (MonadFail)
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Control.Monad.Free.Church as Church
import Control.Monad.Trans.State.Strict
import Text.Printf

-- | A data type representing basic commands for our performance-testing eDSL.
data PerfF next where
  Output    :: String -> next -> PerfF next
  Input     :: (Show a, Read a) => (a -> next) -> PerfF next

-- | Unfortunately this Functor instance cannot yet be derived
-- automatically by GHC.
instance Functor PerfF where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)

makeFreeCon 'Output
makeFreeCon 'Input

type PerfCnt = Word64

-- | Unsafe state variable: base CPU cycles
{-# NOINLINE g_base_counter #-}
g_base_counter :: IORef PerfCnt
g_base_counter = unsafePerformIO $ do
  rdtsc >>= newIORef

-- | Prints number of CPU cycles since last call
g_print_time_since_prev_call :: (MonadIO m) => m ()
g_print_time_since_prev_call = liftIO $ do
  cb <- readIORef g_base_counter
  c <- rdtsc
  writeIORef g_base_counter c
  putStr $ printf "\r%-10s" (show $ c - cb)

-- | Free-based interpreter
runPerfFree :: (MonadIO m) => [String] -> Free PerfF () -> m ()
runPerfFree [] _ = return ()
runPerfFree (s:ss) x = case x of
  Free (Output _o next) -> do
    runPerfFree (s:ss) next
  Free (Input next) -> do
    g_print_time_since_prev_call
    runPerfFree ss (next (read s))
  Pure a -> do
    return a

-- | Church-based interpreter
runPerfF :: (Fail.MonadFail m, MonadIO m) => [String] -> Church.F PerfF () -> m ()
runPerfF [] _ = return ()
runPerfF ss0 f =
  fst `liftM` do
  flip runStateT ss0 $ Church.iterM go f where
    go (Output _o next) = do
      next
    go (Input next) = do
      g_print_time_since_prev_call
      (s:ss) <- get
      put ss
      next (read s)

-- | Test input is the same for all cases
test_input :: [String]
test_input = [show i | i<-([1..9999] ++ [0 :: Int])]

-- | Tail-recursive program
test_tail :: (MonadFree PerfF m) => m ()
test_tail = do
  output "Enter something"
  (n :: Int) <- input
  output $ "Just entered: " ++ (show n)
  when (n > 0) $ do
    test_tail

run_tail_free,run_tail_f :: IO ()
run_tail_free = runPerfFree test_input test_tail
run_tail_f = runPerfF test_input test_tail


-- | Deep-recursive program
test_loop :: (MonadFree PerfF m) => m ()
test_loop = do
  output "Enter something"
  (n :: Int) <- input
  when (n > 0) $ do
    test_loop
  output $ "Just entered: " ++ (show n)

run_loop_free,run_loop_f :: IO ()
run_loop_free = runPerfFree test_input test_loop
run_loop_f = runPerfF test_input test_loop

main :: IO ()
main = do
  putStr $ unlines [
      "Running two kinds of FreeMonad programs against two kinds of interpreters.",
      "Counters represent approx. number of CPU ticks per program iteration" ]
  putStrLn ">> (1/4) Tail-recursive program/Free interpreter"
  run_tail_free
  putStrLn "\n>> (2/4) Tail-recursive program/Church interpreter"
  run_tail_f
  putStrLn "\n>> (3/4) Deep-recursive program/Free interpreter (a slower one)"
  run_loop_free
  putStrLn "\n>> (4/4) Deep-recursive program/Church interpreter"
  run_loop_f
  putStrLn "\n"

