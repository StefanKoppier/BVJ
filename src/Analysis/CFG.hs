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

data Scope = Scope {
       scopePackage :: Maybe Name'
     , scopeClass   :: String
     , scopeMember  :: String
     } deriving (Show, Eq, Ord)

data CFGNodeValue 
    = Block CompoundStmt'
    | Call  Scope        
    | Entry Scope
    | Exit  Scope
    deriving (Show, Eq)
    
type CFGNode = LNode CFGNodeValue

type CFGNodes = [CFGNode]

data CFGEdgeValue
    = InterEdge       Scope
    | ConditionalEdge Exp'
    | IntraEdge
    deriving (Show, Eq)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

newtype CFG = CFG { cfg :: Gr CFGNodeValue CFGEdgeValue }

constructCFG :: CFGNodes -> CFGEdges -> CFG
constructCFG nodes edges = (CFG . insEdges edges . insNodes nodes) empty

entryOfMethod :: Scope -> CFG -> Maybe CFGNode
entryOfMethod scope CFG{cfg}
    | [entry'] <- entry = Just (entry', fromJust $ lab cfg entry')
    | otherwise         = Nothing
    where
        entry = nodes $ labfilter (\case (Entry scope') -> scope == scope'
                                         _              -> False) cfg
         
isIntraEdge, isInterEdge :: CFGEdgeValue -> Bool
isIntraEdge (ConditionalEdge _) = True
isIntraEdge IntraEdge           = True
isIntraEdge (InterEdge _)       = False
isInterEdge = not . isIntraEdge