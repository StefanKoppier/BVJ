module Analysis.Reachability(
    reachableFrom
) where

import Data.Graph.Inductive

-- | Returns the subgraph containing the nodes that are
-- reachable from the starting node.
reachableFrom :: DynGraph ge => Node -> ge a b -> ge a b
reachableFrom s g = subgraph (dfs [s] g) g