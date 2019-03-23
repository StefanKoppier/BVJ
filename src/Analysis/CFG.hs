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
    = Block CompoundStmt' -- ^ The statement of the node.
    | Call  Scope         -- ^ The method that is being called.
            Node          -- ^ The node containing the statement this call belongs to
            Name'         -- ^ The method invocation this call belongs to.
    | Entry Scope
    | Exit  Scope
    deriving (Show, Eq)
    
type CFGNode = LNode CFGNodeValue

type CFGNodes = [CFGNode]

data CFGEdgeValue
    = InterEdge       Scope
                      Int
    | ConditionalEdge Exp'
                      Int
    | IntraEdge       Int
    deriving (Show, Eq)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

newtype CFG = CFG { cfg :: Gr CFGNodeValue CFGEdgeValue }

constructCFG :: CFGNodes -> CFGEdges -> CFG
constructCFG nodes edges = (CFG . insEdges edges . insNodes nodes) empty

entryOfMain :: CFG -> Maybe CFGNode
entryOfMain CFG{cfg}
    | [entry'] <- entry = Just (entry', fromJust $ lab cfg entry')
    | otherwise = Nothing
    where
        entry = nodes $ labfilter (\case (Entry (Scope _ _ method)) 
                                            -> method == "main"
                                         _  -> False) cfg

{-
entryOfMethod :: Scope -> CFG -> Maybe CFGNode
entryOfMethod scope CFG{cfg}
    | [entry'] <- entry = Just (entry', fromJust $ lab cfg entry')
    | otherwise         = Nothing
    where
        entry = nodes $ labfilter (\case (Entry scope') -> scope == scope'
                                         _              -> False) cfg
-}

isIntraEdge, isInterEdge :: CFGEdgeValue -> Bool
isIntraEdge (ConditionalEdge _ _) = True
isIntraEdge (IntraEdge _)         = True
isIntraEdge (InterEdge _ _)       = False

isInterEdge = not . isIntraEdge