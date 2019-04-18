{-|
Module      : Data.Stack
Description : Module containing a stack data type.

This module contains a stack data type.
-}
module Data.Stack (
      Stack
    , push
    , pop
    , peek
    , update
    , empty
    , size
    , singleton
    , toList
    , fromList
) where

-- | The type of the stack.
newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
    show (Stack xs) = show xs

instance Functor Stack where
    fmap f (Stack xs) = Stack $ fmap f xs

instance Applicative Stack where
    pure                  = singleton
    Stack fs <*> Stack xs = Stack (fs <*> xs)

-- | Pushes the value on top of the stack.
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | Pops the value from the top of the stack.
pop :: Stack a -> Stack a
pop (Stack (_:xs)) = Stack xs
pop (Stack [])     = Stack []

-- | Returns the top value of the stack.
peek :: Stack a -> Maybe a
peek (Stack [])    = Nothing
peek (Stack (x:_)) = Just x

-- | Updates the top element of the stack.
update :: a -> Stack a -> Stack a
update x = push x . pop

-- | Creates an empty stack.
empty :: Stack a
empty = Stack []

-- | Returns the number of elements in the stack.
size :: Stack a -> Int
size (Stack xs) = length xs 

-- | Creates a stack from a single value.
singleton :: a -> Stack a
singleton x = Stack [x]

-- | Tranforms the stack into a list representation.
toList :: Stack a -> [a]
toList (Stack xs) = xs

-- | Tranforms a list into a stack representation.
fromList :: [a] -> Stack a
fromList = Stack