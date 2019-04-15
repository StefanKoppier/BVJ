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

-- | Update the top element of the stack.
update :: a -> Stack a -> Stack a
update x = push x . pop

empty :: Stack a
empty = Stack []

size :: Stack a -> Int
size (Stack xs) = length xs 

singleton :: a -> Stack a
singleton x = Stack [x]

toList :: Stack a -> [a]
toList (Stack xs) = xs

fromList :: [a] -> Stack a
fromList = Stack