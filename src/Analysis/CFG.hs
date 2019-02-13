module Analysis.CFG where

import qualified Data.Set                          as S
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

type CFGNodeValue = CompoundStmt'
    
type CFGNode = LNode CFGNodeValue

type CFGNodes = S.Set CFGNode

data CFGEdgeValue
    = Edge
    | ConditionalEdge Bool
    deriving (Show, Eq, Ord)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

data CFG = CFG (Gr CFGNodeValue CFGEdgeValue) Node

--------------------------------------------------------------------------------
-- Extended Control Flow Graph
--------------------------------------------------------------------------------

type ECFGNodeValue = (Name', CFG)

type ECFGNode = LNode ECFGNodeValue

type ECFGNodes = S.Set ECFGNode

type ECFGEdgeValue = Stmt'

type ECFGEdge = LEdge ECFGEdgeValue

type ECFGEdges = [ECFGEdge]

type ECFGAdj = Adj ECFGEdgeValue

type ECFGContext = (ECFGAdj, Node, ECFGNodeValue, ECFGAdj)

type ECFG = Gr ECFGNodeValue ECFGEdgeValue
