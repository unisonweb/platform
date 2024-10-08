module Unison.PrettyTerminal where

import System.Console.Terminal.Size qualified as Terminal
import Unison.Util.ColorText qualified as CT
import Unison.Util.Less (less)
import Unison.Util.Pretty qualified as P

-- like putPrettyLn' but prints a blank line before and after.
putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p | p == mempty = pure ()
putPrettyLn p = do
  width <- getAvailableWidth
  less . P.toANSI width $ P.border 2 p

putPrettyLnUnpaged :: P.Pretty CT.ColorText -> IO ()
putPrettyLnUnpaged p | p == mempty = pure ()
putPrettyLnUnpaged p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.border 2 p

putPrettyLn' :: P.Pretty CT.ColorText -> IO ()
putPrettyLn' p | p == mempty = pure ()
putPrettyLn' p = do
  width <- getAvailableWidth
  less $ P.toANSI width p

clearCurrentLine :: IO ()
clearCurrentLine = do
  width <- getAvailableWidth
  putStr "\r"
  putStr $ replicate (P.widthToInt width) ' '
  putStr "\r"

putPretty' :: P.Pretty CT.ColorText -> IO ()
putPretty' p = do
  width <- getAvailableWidth
  putStr $ P.toANSI width p

getAvailableWidth :: IO P.Width
getAvailableWidth =
  maybe 80 (\s -> 100 `min` P.Width (Terminal.width s)) <$> Terminal.size

putPrettyNonempty :: P.Pretty P.ColorText -> IO ()
putPrettyNonempty msg = do
  if msg == mempty then pure () else putPrettyLn msg
