module Analysis.CFG where

import qualified Data.Set                          as S
import           Parsing.Syntax
import           Data.Graph.Inductive.Graph    
import           Data.Graph.Inductive.PatriciaTree    

type CFGNodeValue = CompoundStmt'
    
type CFGNode = LNode CFGNodeValue

instance {-# OVERLAPS #-} Ord CFGNode where
    (x,_) <= (y,_) = x <= y

type CFGNodes = S.Set CFGNode

data CFGEdgeValue
    = Edge
    | ConditionalEdge Bool
    deriving (Show, Eq, Ord)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

type CFG = Gr CFGNodeValue CFGEdgeValue
