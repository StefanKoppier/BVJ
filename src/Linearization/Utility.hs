module Linearization.Utility(
    distance
) where

import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.BFS
import           Data.Tuple
import qualified Data.Map                       as M
import           Analysis.CFG

import Analysis.Pretty
import Auxiliary.Pretty

-- | Compute the minimum distance for all nodes to any final node.
distance :: CFG -> Node -> M.Map Node Int
distance graph final = M.fromList $ level final cfgReverse
    where
        cfgReverse = grev (cfg graph)