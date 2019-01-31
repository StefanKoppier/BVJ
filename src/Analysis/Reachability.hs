module Analysis.Reachability(
    reachableFrom
) where

import Data.Graph.Inductive

-- | Returns the subgraph containing the nodes that are
-- reachable from the starting node.
reachableFrom :: DynGraph ge => LNode a -> ge a b -> ge a b
reachableFrom (s,_) g = subgraph (dfs [s] g) g