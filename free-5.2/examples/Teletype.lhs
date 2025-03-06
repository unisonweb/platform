> {-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-} --
> module Main where

> import qualified Control.Exception as E (catch)
> import Control.Monad         (mfilter)
> import Control.Monad.Loops   (unfoldM)
> import Control.Monad.Free    (liftF, Free, iterM, MonadFree)
> import Control.Monad.Free.TH (makeFree)
> import System.IO             (isEOF)
> import System.IO.Error       (ioeGetErrorString)
> import System.Exit           (exitSuccess)

First, we define a data type with the primitive actions of a teleprinter. The
@param@ will stand for the next action to execute.

> type Error = String
>
> data Teletype param = Halt                                  -- Abort (ignore all following instructions)
>                     | NL param                              -- Newline
>                     | Read (Char -> param)                  -- Get a character from the terminal
>                     | ReadOrEOF { onEOF  :: param,
>                                   onChar :: Char -> param } -- GetChar if not end of file
>                     | ReadOrError (Error -> param)
>                                   (Char -> param)           -- GetChar with error code
>                     | param :\^^ String                     -- Write a message to the terminal
>                     | (:%) param String [String]            -- String interpolation
>                     deriving (Functor)

By including a 'makeFree' declaration:

> makeFree ''Teletype

the following functions have been made available:

@
 halt        :: (MonadFree Teletype m) => m a
 nL          :: (MonadFree Teletype m) => m ()
 read        :: (MonadFree Teletype m) => m Char
 readOrEOF   :: (MonadFree Teletype m) => m (Maybe Char)
 readOrError :: (MonadFree Teletype m) => m (Either Error Char)
 (\\^^)      :: (MonadFree Teletype m) => String -> m ()
 (%)         :: (MonadFree Teletype m) => String -> [String] -> m ()
@

To make use of them, we need an instance of 'MonadFree Teletype'. Since 'Teletype' is a
'Functor', we can use the one provided in the 'Control.Monad.Free' package.

> type TeletypeM = Free Teletype

Programs can be run in different ways. For example, we can use the
system terminal through the @IO@ monad.

> runTeletypeIO :: TeletypeM a -> IO a
> runTeletypeIO = iterM run where
>   run :: Teletype (IO a) -> IO a
>   run Halt                      = do
>     putStrLn "This conversation can serve no purpose anymore. Goodbye."
>     exitSuccess
>
>   run (Read f)                  = getChar >>= f
>   run (ReadOrEOF eof f)         = isEOF >>= \b -> if b then eof
>                                                        else getChar >>= f
>
>   run (ReadOrError ferror f)    = E.catch (getChar >>= f) (ferror . ioeGetErrorString)
>   run (NL rest)                 = putChar '\n' >> rest
>   run (rest :\^^ str)           = putStr str >> rest
>   run ((:%) rest format tokens) = ttFormat format tokens >> rest
>
>   ttFormat :: String -> [String] -> IO ()
>   ttFormat []            _          = return ()
>   ttFormat ('\\':'%':cs) tokens     = putChar '%'  >> ttFormat cs tokens
>   ttFormat ('%':cs)      (t:tokens) = putStr t     >> ttFormat cs tokens
>   ttFormat (c:cs)        tokens     = putChar c    >> ttFormat cs tokens

Now, we can write some helper functions:

> readLine :: TeletypeM String
> readLine = unfoldM $ mfilter (/= '\n') <$> readOrEOF

And use them to interact with the user:

> hello :: TeletypeM ()
> hello = do
>           (\^^) "Hello! What's your name?"; nL
>           name <- readLine
>           "Nice to meet you, %." % [name]; nL
>           halt

We can transform any @TeletypeM@ into an @IO@ action, and run it:

> main :: IO ()
> main = runTeletypeIO hello

@
 Hello! What's your name?
 $ Dave
 Nice to meet you, Dave.
 This conversation can serve no purpose anymore. Goodbye.
@

When specifying DSLs in this way, we only need to define the semantics
for each of the actions; the plumbing of values is taken care of by
the generated monad instance.

