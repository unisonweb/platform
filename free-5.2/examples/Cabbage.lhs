> {-# LANGUAGE ViewPatterns #-}
> module Cabbage where

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Trans.Iter
> import Control.Monad.Writer
> import Data.Functor.Identity
> import Data.Maybe
> import Data.Tuple
> import Data.List (inits, tails)

Consider the following problem:

A farmer must cross a river with a wolf, a sheep and a cabbage.
He owns a boat, which can only carry himself and one other item.
The sheep must not be left alone with the wolf, or with the cabbage:
if that happened, one of them would eat the other.

> data Item = Wolf | Sheep | Cabbage | Farmer deriving (Ord, Show, Eq)
>
> eats :: Item -> Item -> Bool
> Sheep `eats` Cabbage = True
> Wolf `eats` Sheep    = True
> _ `eats` _           = False

The problem can be represented as the set of items on each side of the river.

> type Situation = ([Item],[Item])

> initial :: Situation
> initial = ([Farmer, Wolf, Sheep, Cabbage], [])

First, some helper functions to extract single elements from lists, leaving the
rest intact:

> plusTailOf :: [a] -> [a] -> (Maybe a, [a])
> a `plusTailOf` b = (listToMaybe b,  a ++ drop 1 b)

> singleOut1 :: (a -> Bool) -> [a] -> (Maybe a,[a])
> singleOut1 sel = uncurry plusTailOf . break sel

@
*Cabbage> singleOut1 (== Sheep) [Wolf, Sheep, Cabbage]
(Just Sheep,[Wolf,Cabbage])
@

> singleOutAll :: [a] -> [(Maybe a,[a])]
> singleOutAll = zipWith plusTailOf <$> inits <*> tails

@
*Cabbage> singleOutAll [Wolf, Sheep, Cabbage]
[(Just Wolf,[Sheep,Cabbage]),(Just Sheep,[Wolf,Cabbage]),(Just Cabbage,[Wolf,Sheep]),(Nothing,[Wolf,Sheep,Cabbage])]
@

In every move, the farmer goes from one side of the river to the other,
together with (optionally) one item.

The remaining items must not eat each other for the move to be valid.

> move :: Situation -> [Situation]
> move = move2
>   where
>   move2 (singleOut1 (== Farmer) -> (Just Farmer,as), bs)  = move1 as bs
>   move2 (bs, singleOut1 (== Farmer) -> (Just Farmer,as))  = map swap $ move1 as bs
>   move2 _                                            = []
>
>   move1 as bs = [(as', [Farmer] ++ maybeToList b ++ bs) |
>                  (b, as') <- singleOutAll as,
>                  and [not $ x `eats` y | x <- as', y <- as']]

@
*Cabbage> move initial
[([Wolf,Cabbage],[Farmer,Sheep])]
@

When the starting side becomes empty, the farmer succeeds.

> success :: Situation -> Bool
> success ([],_) = True
> success _      = False

A straightforward implementation to solve the problem could use the
list monad, trying all possible solutions and

> solution1 :: Situation
> solution1 = head $ solutions' initial
>             where
>             solutions' a = if success a
>                            then return a
>                            else move a >>= solutions'

However, when it's run, it will get stuck in an infinite loop, as the sheep
is shuffled back and forth. The solution is being searched in depth.

To guarantee termination, we can use the 'Iter' monad with its MonadPlus instance.
As long as one of the possible execution paths finds a solution, the program
will terminate: the solution is looked for _in breadth_.

> solution2 :: Iter Situation
> solution2 = solution' initial
>             where
>               solution' a =
>                 if success a
>                   then return a
>                   else delay $ msum $ map solution' (move a)

Each of the alternative sequences of movements will be evaluated
concurrently; and the shortest one will be the result. In case of ties,
the leftmost solution takes priority.

@
 *Cabbage> solution2
 IterT (Identity (Right ( …
   (IterT (Identity (Right
     (IterT (Identity (Left
       ([],[Farmer,Sheep,Cabbage,Wolf]))))))))))))))))))))))))
@

For a cleaner display, use 'retract' to escape 'Iter' monad:

@
 *Cabbage> retract solution2
 Identity ([],[Farmer,Sheep,Cabbage,Wolf])
@

'unsafeIter' will also get rid of the 'Identity' wrapper:

> unsafeIter :: Iter a -> a
> unsafeIter = runIdentity . retract

@
 *Cabbage> unsafeIter solution2
 ([],[Farmer,Sheep,Cabbage,Wolf])
@

Suppose that we not only want the solution, but also the steps that we
took to arrive there. Enter the Writer monad transformer:

> solution3 :: Iter (Situation, [Situation])
> solution3 = runWriterT $ solution' initial
>             where
>               solution' :: Situation -> WriterT [Situation] Iter Situation
>               solution' a = do
>                 tell [a]
>                 if success a
>                   then return a
>                   else mapWriterT delay $ msum $ map solution' (move a)

The second component contains the complete path to the solution:

@
 *Cabbage> snd $ unsafeIter solution3
 [([Farmer,Wolf,Sheep,Cabbage],[]),
  ([Wolf,Cabbage],[Farmer,Sheep]),
  ([Farmer,Wolf,Cabbage],[Sheep]),
  ([Cabbage],[Farmer,Wolf,Sheep]),
  ([Farmer,Sheep,Cabbage],[Wolf]),
  ([Sheep],[Farmer,Cabbage,Wolf]),
  ([Farmer,Sheep],[Cabbage,Wolf]),
  ([],[Farmer,Sheep,Cabbage,Wolf])]
@

When the transformer is applied _over_ the Iter monad, it acts locally for each solution.
If we apply the IterT transformer over another monad,
the behaviour for that monad will be shared among all threads.

For example, let's keep track of how many moves we perform. We could
do so with the writer monad again (numbers form a monoid under addition), but
we'll use the state monad this time.

> solution4 :: Iter (Situation, Integer)
> solution4 = flip runStateT 0 $ solution' initial
>             where
>               solution' :: Situation -> StateT Integer Iter Situation
>               solution' a =
>                 if success a
>                   then return a
>                   else do
>                          modify (+1)
>                          mapStateT delay $ msum $ map solution' (move a)

This gives us seven moves (one for each transition between two states).

@
 *Cabbage> unsafeIter solution4
 (([],[Farmer,Sheep,Cabbage,Wolf]),7)
@

On the other hand, if move the state inside Iter, we get a global count of
explored nodes until the solution was found.

> solution5 :: State Integer Situation
> solution5 = retract $ solution' initial
>             where
>               solution' :: Situation -> IterT (State Integer) Situation
>               solution' a =
>                 if success a
>                   then return a
>                   else do
>                          modify (+1)
>                          delay $ msum $ map solution' (move a)

@
 *Cabbage> runState solution5 0
 (([],[Farmer,Sheep,Cabbage,Wolf]),113)
@
