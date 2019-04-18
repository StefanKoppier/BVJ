{-|
Module      : Analysis.CFG
Description : Module containing the Control Flow Graph.

This module contains the data definitions and some utility functions of the 
Control Flow Graph (CFG).
-}
module Analysis.CFG where

import Data.Maybe
import Parsing.Syntax
import Data.Graph.Inductive.Graph    
import Data.Graph.Inductive.PatriciaTree    

instance {-# OVERLAPS #-} Eq (LNode a) where
    (x,_) == (y,_) = x == y

instance {-# OVERLAPS #-} Ord (LNode a) where
    (x,_) <= (y,_) = x <= y

--------------------------------------------------------------------------------
-- Control Flow Graph
--------------------------------------------------------------------------------

-- | Data type containing the value of a node in the control flow graph.
data CFGNodeValue 
    -- | A statement.
    = StatNode        CompoundStmt'  
    -- | A for init.
    | ForInitNode     ForInit'       
    -- | A for update.
    | ForUpdateNode   Exps'          
    -- | A catch entry.
    | CatchNode       Catch'        
    -- | A finally entry.
    | FinallyNode     CompoundStmts' 
    -- | An invocation of a method.
    | CallNode        Scope          
                      Node           
                      Name'          
    -- | An entry of a method.
    | MethodEntryNode Scope          
    -- | An exit of a method.
    | MethodExitNode  Scope          
    deriving (Show, Eq)
    
-- | Type declaration of the node in the control flow graph.
type CFGNode = LNode CFGNodeValue

-- | Type declaration of the nodes in the control flow graph.
type CFGNodes = [CFGNode]

-- | Data type containing the value of an edge in the control flow graph.
data CFGEdgeValue
    = InterEdge           Scope
    | IntraEdge       
    | BlockEntryEdge      BlockEntryType
    | BlockExitEdge       BlockEntryType
    | BlockExitsEdge      [BlockEntryType]
    | BlockExitEntryEdge  BlockEntryType 
                          BlockEntryType
    deriving (Show, Eq)

-- | Data type containing the type of entry or exit block.
data BlockEntryType
    = TryEntryType
    | CatchEntryType       (Maybe FormalParam')
    | ConditionalEntryType (Maybe Exp')
    | FinallyEntryType
    | BlockEntryType       (Maybe String)
    deriving (Show)

instance Eq BlockEntryType where
    TryEntryType             == TryEntryType             = True
    (CatchEntryType _)       == (CatchEntryType _)       = True
    (ConditionalEntryType _) == (ConditionalEntryType _) = True
    FinallyEntryType         == FinallyEntryType         = True
    (BlockEntryType _)       == (BlockEntryType _)       = True
    _                        == _                        = False

-- | Type declaration of the edge in the control flow graph.
type CFGEdge = LEdge CFGEdgeValue

-- | Type declaration of the edges in the control flow graph.
type CFGEdges = [CFGEdge]

-- | Type declaration of the adjacencies in the control flow graph.
type CFGAdj = Adj CFGEdgeValue

-- | Type declaration of the context in the control flow graph.
type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

-- | Type declaration of the control flow graph.
newtype CFG = CFG { cfg :: Gr CFGNodeValue CFGEdgeValue }

-- | Constructs the control flow graph from the given nodes and edges.
constructCFG :: CFGNodes -> CFGEdges -> CFG
constructCFG ns es = (CFG . insEdges es . insNodes ns) empty

-- | Retruns the method entry node of the scope, if it exists.
entryOfMethod :: Scope -> CFG -> Maybe CFGNode
entryOfMethod method CFG{cfg}
    | [entry'] <- entry = Just (entry', fromJust $ lab cfg entry')
    | otherwise = Nothing
    where
        entry = nodes $ labfilter (\case (MethodEntryNode scope) 
                                            -> scope == method
                                         _  -> False) cfg

-- | Return true if the edge is an intraprocedural edge.
isIntraEdge :: CFGEdgeValue -> Bool
isIntraEdge IntraEdge                = True
isIntraEdge (BlockEntryEdge _)       = True
isIntraEdge (BlockExitEdge _)        = True
isIntraEdge (BlockExitsEdge _)       = True
isIntraEdge (BlockExitEntryEdge _ _) = True
isIntraEdge (InterEdge _)            = False

-- | Return true if the edge is an interprocedural edge.
isInterEdge :: CFGEdgeValue -> Bool
isInterEdge = not . isIntraEdge