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

intraEdges :: Labels -> Label -> Flow
intraEdges es b = Flow $ map (`IntraEdge` b) es

incoming :: Flow -> Label -> [Label]
incoming (Flow es) l = (map (\ (IntraEdge x _) -> x) . filter (\ (IntraEdge x y) -> x == l)) es

outgoing :: Flow -> Label -> [Label]
outgoing (Flow es) l = (map (\ (IntraEdge _ y) -> y) . filter (\ (IntraEdge x y) -> y == l)) es
