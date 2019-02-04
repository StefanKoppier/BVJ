module Linearization.Utility where

import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.BFS
import qualified Data.Set                       as S
import qualified Data.Map                       as M

-- | Compute the minimum distance for all nodes to any final node.
distance :: DynGraph gr => S.Set Node -> gr a b -> M.Map Node Int
distance finals cfg
    | []     <- levels = M.empty
    | (l:ls) <- levels = foldr (M.unionWith min) l ls
    where
        cfgReverse = grev cfg
        levels     = S.toList $ S.map (\ f -> M.fromList $ level f cfgReverse) finals
