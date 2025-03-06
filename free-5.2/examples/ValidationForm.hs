module Main where

import Control.Applicative.Free
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State

import Data.Monoid (Sum(..))

import Text.Read (readEither)
import Text.Printf

import System.IO

-- | Field reader tries to read value or generates error message.
type FieldReader a = String -> Either String a

-- | Convenient synonym for field name.
type Name = String

-- | Convenient synonym for field help message.
type Help = String

-- | A single field of a form.
data Field a = Field
  { fName     :: Name           -- ^ Name.
  , fValidate :: FieldReader a  -- ^ Pure validation function.
  , fHelp     :: Help           -- ^ Help message.
  }

-- | Validation form is just a free applicative over Field.
type Form = Ap Field

-- | Build a form with a single field.
field :: Name -> FieldReader a -> Help -> Form a
field n f h = liftAp $ Field n f h

-- | Singleton form accepting any input.
string :: Name -> Help -> Form String
string n h = field n Right h

-- | Singleton form accepting anything but mentioned values.
available :: [String] -> Name -> Help -> Form String
available xs n h = field n check h
  where
    check x | x `elem` xs = Left "the value is not available"
            | otherwise   = Right x

-- | Singleton integer field form.
int :: Name -> Form Int
int name = field name readEither "an integer value"

-- | Generate help message for a form.
help :: Form a -> String
help = unlines . runAp_ (\f -> [fieldHelp f])

-- | Get help message for a field.
fieldHelp :: Field a -> String
fieldHelp (Field name _ msg) = printf "  %-15s - %s" name msg

-- | Count fields in a form.
count :: Form a -> Int
count = getSum . runAp_ (\_ -> Sum 1)

-- | Interactive input of a form.
-- Shows progress on each field.
-- Repeats field input until it passes validation.
-- Show help message on empty input.
input :: Form a -> IO a
input m = evalStateT (runAp inputField m) 1
  where
    inputField :: Field a -> StateT Int IO a
    inputField f@(Field n g h) = do
      i <- get
      -- get field input with prompt
      x <- liftIO $ do
        putStr $ printf "[%d/%d] %s: " i (count m) n
        hFlush stdout
        getLine
      case words x of
        -- display help message for empty input
        [] -> do
          liftIO . putStrLn $ "help: " ++ h
          inputField f
        -- validate otherwise
        _ -> case g x of
               Right y -> do
                 modify (+ 1)
                 return y
               Left  e -> do
                 liftIO . putStrLn $ "error: " ++ e
                 inputField f

-- | User datatype.
data User = User
  { userName     :: String
  , userFullName :: String
  , userAge      :: Int }
  deriving (Show)

-- | Form for User.
form :: [String] -> Form User
form us = User
  <$> available us  "Username"  "any vacant username"
  <*> string        "Full name" "your full name (e.g. John Smith)"
  <*> int           "Age"

main :: IO ()
main = do
  putStrLn "Creating a new user."
  putStrLn "Please, fill the form:"
  user <- input (form ["bob", "alice"])
  putStrLn $ "Successfully created user \"" ++ userName user ++ "\"!"

