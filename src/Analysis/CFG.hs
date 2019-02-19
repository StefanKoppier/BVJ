module Analysis.CFG where

import qualified Data.Map                          as M
import           Data.Maybe
import           Parsing.Syntax
import           Data.Graph.Inductive.Graph    
import           Data.Graph.Inductive.PatriciaTree    

instance {-# OVERLAPS #-} Eq (LNode a) where
    (x,_) == (y,_) = x == y

instance {-# OVERLAPS #-} Ord (LNode a) where
    (x,_) <= (y,_) = x <= y

--------------------------------------------------------------------------------
-- Control Flow Graph
--------------------------------------------------------------------------------

data CFGNodeValue 
    = Block CompoundStmt'
    | Entry Name'
    | Exit  Name'
    deriving (Show, Eq)
    
type CFGNode = LNode CFGNodeValue

type CFGNodes = [CFGNode]

data CFGEdgeValue
    = InterEdge
    | ConditionalEdge Exp'
    | IntraEdge
    deriving (Show, Eq)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

data CFG = CFG { cfg :: Gr CFGNodeValue CFGEdgeValue }

constructCFG :: CFGNodes -> CFGEdges -> CFG
constructCFG nodes edges = (CFG . insEdges edges . insNodes nodes) empty

entryOfMethod :: Name' -> CFG -> Maybe CFGNode
entryOfMethod name CFG{cfg} 
    | [entry'] <- entry = Just (entry', fromJust $ lab cfg entry')
    | otherwise         = Nothing
    where
        entry = nodes $ labfilter (\case (Entry n) -> n == name
                                         _         -> False) cfg

isIntraEdge, isInterEdge :: CFGEdgeValue -> Bool
isIntraEdge (ConditionalEdge _) = True
isIntraEdge IntraEdge           = True
isIntraEdge InterEdge           = False
isInterEdge = not . isIntraEdge