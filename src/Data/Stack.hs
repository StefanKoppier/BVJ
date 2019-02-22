module Data.Stack (
      Stack
    , push
    , pop
    , peek
    , empty
    , singleton
) where

newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
    show (Stack xs) = show xs

instance Functor Stack where
    fmap f (Stack xs) = Stack $ fmap f xs

instance Applicative Stack where
    pure                  = singleton
    Stack fs <*> Stack xs = Stack (fs <*> xs)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Stack a
pop (Stack (_:xs)) = Stack xs
pop (Stack [])     = Stack []

peek :: Stack a -> Maybe a
peek (Stack [])    = Nothing
peek (Stack (x:_)) = Just x

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton x = Stack [x]