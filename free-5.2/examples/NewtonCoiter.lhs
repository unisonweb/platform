Many numerical approximation methods compute infinite sequences of results; each,
hopefully, more accurate than the previous one.

<https://en.wikipedia.org/wiki/Newton's_method Newton's method>
to find zeroes of a function is one such algorithm.

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
> module Main where

> import Control.Comonad.Trans.Coiter
> import Control.Comonad.Env
> import Data.Foldable (toList, find)

> data Function = Function {
>   -- Function to find zeroes of
>   function   :: Double -> Double,
>   -- Derivative of the function
>   derivative :: Double -> Double
> }
>
> data Result = Result {
>   -- Estimated zero of the function
>   value  :: Double,
>   -- Estimated distance to the actual zero
>   xerror :: Double,
>   -- How far is value from being an actual zero; that is,
>   -- the difference between @0@ and @f value@
>   ferror :: Double
> } deriving (Show)
>
> data Outlook = Outlook { result :: Result,
>                          -- Whether the result improves in future steps
>                          progress :: Bool } deriving (Show)

To make our lives easier, we will store the problem at hand using the Env
environment comonad.

> type Solution a = CoiterT (Env Function) a

Problems consist of a function and its derivative as the environment, and
an initial value.

> type Problem = Env Function Double

We can express an iterative algorithm using unfold over an initial environment.

> newton :: Problem -> Solution Double
> newton = unfold (\wd ->
>                     let  f  = asks function wd in
>                     let df  = asks derivative wd in
>                     let  x  = extract wd in
>                     x - f x / df x)
>
>

To estimate the error, we look forward one position in the stream. The next value
will be much more precise than the current one, so we can consider it as the
actual result.

We know that the exact value of a function at one of it's zeroes is 0. So,
@ferror@ can be computed exactly as @abs (f a - f 0) == abs (f a)@

> estimateError :: Solution Double -> Result
> estimateError s =
>   let (a, s') = extract $ runCoiterT s in
>   let a' = extract s' in
>   let f = asks function s in
>   Result { value = a,
>            xerror = abs $ a - a',
>            ferror = abs $ f a
>          }

To get a sense of when the algorithm is making any progress, we can sample the
future and check if the result improves at all.

> estimateOutlook :: Int -> Solution Result -> Outlook
> estimateOutlook sampleSize solution =
>   let sample = map ferror $ take sampleSize $ tail $ toList solution in
>   let result' = extract solution in
>   Outlook { result = result',
>             progress = ferror result' > minimum sample }

To compute the square root of @c@, we solve the equation @x*x - c = 0@. We will
stop whenever the accuracy of the result doesn't improve in the next 5 steps.

The starting value for our algorithm is @c@ itself. One could compute a better
estimate, but the algorithm converges fast enough that it's not really worth it.

> squareRoot :: Double -> Maybe Result
> squareRoot c = let problem = flip env c (Function { function = (\x -> x*x - c),
>                                                     derivative = (\x -> 2*x) })
>                in
>                fmap result $ find (not . progress) $
>                  newton problem =>> estimateError =>> estimateOutlook 5

This program will output the result together with the error.

> main :: IO ()
> main = putStrLn $ show $ squareRoot 3

