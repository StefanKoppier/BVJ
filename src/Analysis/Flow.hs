module Analysis.Flow(
      Label
    , Labels
    , Edge
    , Flow
    , intraEdge
    , intraEdges
    , module Data.Monoid
) where

import Data.List   (union)
import Data.Monoid

type Label = Int

type Labels = [Label]

data Edge 
    = IntraEdge Label Label
    deriving (Eq)

instance Show Edge where
    show (IntraEdge l l') = "(" ++ show l ++ "," ++ show l' ++ ")"

newtype Flow = Flow [Edge]

instance Monoid Flow where
    mempty = Flow []
    mappend (Flow xs) (Flow ys) = Flow $ union xs ys

instance Show Flow where
    show (Flow xs) = show xs

intraEdge :: Label -> Label -> Flow
intraEdge a b = Flow [IntraEdge a b]

-- TODO: filter (/= b) is a hacky way to remove the generated self-edges
-- these shouldn't be generated in the first place.
intraEdges :: Labels -> Label -> Flow
intraEdges es b = Flow $ map (`IntraEdge` b) es $ filter (/= b) es