module Analysis.Utility where

import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.BFS
import qualified Data.Set                       as S
import qualified Data.Map                       as M
import           Analysis.Complete

prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify = Data.Graph.Inductive.Graph.prettify

-- | Compute the minimum distance for all nodes to any final node.
distance :: DynGraph gr => Block' -> gr a b -> M.Map Node Int
distance block cfg
    | []     <- levels = M.empty
    | (l:ls) <- levels = foldr (M.unionWith min) l ls
    where
        cfgReverse = grev cfg
        final      = S.map fst $ finalOfBlock block
        levels     = S.toList $ S.map (\ f -> M.fromList $ level f cfgReverse) final