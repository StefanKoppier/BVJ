{-|
Module      : Data.Accumulator
Description : Module containing an accumulator data type.

This module contains a monadic accumulator data type.
-}
module Data.Accumulator where

import Control.Monad (ap)

-- | An accumulator value type a with result type v.
newtype Accumulator a v = Accumulator { runAccumulator :: a -> (a, v) }

instance Functor (Accumulator a) where
    fmap f (Accumulator v) = Accumulator (\ a -> let (a', v') = v a
                                                  in (a', f v')) 

instance Applicative (Accumulator a) where
    pure  = return
    (<*>) = ap

instance Monad (Accumulator a) where
    return v = Accumulator (\ a -> (a, v))
    (Accumulator m) >>= f = Accumulator (\ a -> let (a', v')         = m a
                                                    (Accumulator m') = f v'
                                                 in m' a')

-- | Get the current accumulator value.
getAccumulator :: Accumulator a a
getAccumulator = Accumulator (\ a -> (a, a))

-- | Set the current accumulator value.
setAccumulator :: a -> Accumulator a ()
setAccumulator a = Accumulator (const (a, ()))

-- | Update the current accumulator value.
updateAccumulator :: (a -> a) -> Accumulator a ()
updateAccumulator f = Accumulator (\ a -> (f a, ()))

-- | Perform the accumulator and reset the accumulator value to its original value.
keepOldAccumulator :: Accumulator a b -> Accumulator a b
keepOldAccumulator f = do
    acc    <- getAccumulator
    result <- f
    setAccumulator acc
    return result