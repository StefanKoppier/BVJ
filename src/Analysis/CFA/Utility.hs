{-|
Module      : Analysis.CFA.Utility
Description : Utility functions for the Control Flow Analysis.

This module contains utility functions to generate nodes and edges, used in the
Control Flow Analysis.
-}
module Analysis.CFA.Utility where

import qualified Data.Map                   as M
import           Data.Graph.Inductive.Graph
import           Parsing.Syntax
import           Analysis.CFG

-- | Map containing the entry nodes and exit nodes of all methods.
type Methods = M.Map Scope (Node, Node)

-- | Node representation of a non-existing node.
noneNode :: CFGNode
noneNode = (-1, undefined)

ifNoneNode :: CFGNode -> CFGNode -> CFGNode
ifNoneNode node alternative
    | node == noneNode = alternative
    | otherwise        = node

-- | List node representation of non-existing nodes.
noneNodes :: CFGNodes
noneNodes = [noneNode]

-- | Returns the second set of nodes if the first set is noneNodes.
ifNoneNodes :: CFGNodes -> CFGNodes -> CFGNodes
ifNoneNodes node alternative
    | node == noneNodes = alternative
    | otherwise         = node

-- | Add the node to the set if it is Just.
addMaybeNode :: Maybe CFGNode -> CFGNodes -> CFGNodes
addMaybeNode Nothing     = id
addMaybeNode (Just node) = (node :)

-- | Add the edge to the set if it is Just.
addMaybeEdge :: Maybe CFGEdge -> CFGEdges -> CFGEdges
addMaybeEdge Nothing     = id
addMaybeEdge (Just edge) = (edge :)

--------------------------------------------------------------------------------
-- Node creation
--------------------------------------------------------------------------------

-- | Create a new label, assuming the labels 
-- greater than the given label are not used.
new :: Node -> Node
new = (1+)

-- | Create a new CFG statement node.
statNode :: Node -> CompoundStmt' -> CFGNode
statNode node stat = (node, StatNode stat)

-- | Create a for update node.
forUpdateNode :: Node -> MaybeExps' -> CFGNodes
forUpdateNode _    Nothing     = []
forUpdateNode node (Just exps) = [(node, ForUpdateNode exps)]

-- | Create a for init node.
forInitNode :: Node -> ForInit' -> CFGNode
forInitNode node init = (node, ForInitNode init)

-- | Create a new CFG catch node.
catchNode :: Node -> Catch' -> CFGNode
catchNode node catch = (node, CatchNode catch)

-- | Create a new CFG finally node, if the block exists.
finallyNode :: Node -> MaybeCompoundStmts' -> Maybe CFGNode
finallyNode _    Nothing        = Nothing
finallyNode node (Just finally) = Just (node, FinallyNode finally)

-- | Create a new CFG call node, if the given method exists.
callNode :: Node -> Maybe Scope -> Node -> Name' -> Maybe CFGNode
callNode _ Nothing _ _ 
    = Nothing

callNode node (Just scope) expressionNode name
    = Just (node, CallNode scope expressionNode name)

-- | Create a new CFG method entry node.
methodEntryNode :: Node -> Scope -> CFGNode
methodEntryNode node scope = (node, MethodEntryNode scope)

-- | Create a new CFG method exit node.
methodExitNode :: Node -> Scope -> CFGNode
methodExitNode node scope = (node, MethodExitNode scope)

--------------------------------------------------------------------------------
-- Edge creation
--------------------------------------------------------------------------------

-- | Create a call edge from the first node to the second scope, if it is found.
callEdge :: (Node, Maybe Scope) -> Methods -> Maybe CFGEdge
callEdge (_, Nothing) _ = Nothing

callEdge (from, Just to) methods = do
    toNode <- fst <$> (methods M.!? to)
    return (from, toNode, InterEdge to)

-- | Create a return edge from the first scope to the second node, if it is found.
returnEdge :: (Maybe Scope, Node) -> Methods -> Maybe CFGEdge
returnEdge (Nothing, _) _ = Nothing

returnEdge (Just from, to) methods = do
    fromNode <- snd <$> (methods M.!? from)
    return (fromNode, to, InterEdge from)

-- | Create intraprocedural edges from the first set to the second node.
intraEdges :: (CFGNodes, CFGNode) -> CFGEdges
intraEdges (froms, to)
    = concatMap (\ from -> intraEdge (from, to)) froms

-- | Create an intraprocedural edge from the first node to the second node.
intraEdge :: (CFGNode, CFGNode) -> CFGEdges
intraEdge (fromNode@(from, _), toNode@(to, _))
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, IntraEdge)]

-- | Create an block entry edge from the first node to the second node.
blockEntryEdge :: (CFGNode, CFGNode) -> BlockEntryType -> CFGEdges
blockEntryEdge (fromNode@(from, _), toNode@(to, _)) entryType
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, BlockEntryEdge entryType)]

-- | Create block exit edges from the first set to the second node.
blockExitEdges :: (CFGNodes, CFGNode) -> BlockEntryType -> CFGEdges
blockExitEdges (froms, to) entryType
    = concatMap (\ from -> blockExitEdge (from, to) entryType) froms

-- | Create a block exit edge from the first node to the second node.
blockExitEdge :: (CFGNode, CFGNode) -> BlockEntryType -> CFGEdges
blockExitEdge (fromNode@(from, _), toNode@(to, _)) entryType
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, BlockExitEdge entryType)]

-- | Create block exit entry edges from the first set to the second node.
blockExitEntryEdges :: (CFGNodes, CFGNode) -> BlockEntryType -> BlockEntryType -> CFGEdges
blockExitEntryEdges (froms, to) exit entry
    = concatMap (\ from -> blockExitEntryEdge (from, to) exit entry) froms

-- | Create a block exit entry edge from the first node to the second node.
blockExitEntryEdge :: (CFGNode, CFGNode) -> BlockEntryType -> BlockEntryType -> CFGEdges
blockExitEntryEdge (fromNode@(from, _), toNode@(to, _)) exit entry 
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, BlockExitEntryEdge exit entry)]

-- | Create the edges between the first set to the second set for the ; operator.
seqEdges :: (CFGNodes, CFGNode) -> Maybe CompoundStmt' -> CompoundStmts' -> CFGEdges
seqEdges (froms, to) currentStat nextStats 
    = concatMap (\ from -> seqEdge (from, to) currentStat nextStats) froms

-- | Create an edge between the first node to the second set for the ; operator.
seqEdge :: (CFGNode, CFGNode) -> Maybe CompoundStmt' -> CompoundStmts' -> CFGEdges
seqEdge edge@(fromNode, toNode) currentStat nextStats
    -- Case: no source or destination node.
    | fromNode == noneNode || toNode == noneNode
        = []
        
    -- Case: We're entering a block.
    | (Block' _ _:_) <- nextStats
        = blockEntryEdge edge (BlockEntryType Nothing)
    
    -- Case: we're leaving a catch or finally block.
    | Just (Try' _ _ finally) <- currentStat
        = case finally of
            Just _  -> blockExitEdge edge FinallyEntryType
            Nothing -> blockExitEdge edge (CatchEntryType Nothing)

    -- Case: anything else.
    | otherwise
        = intraEdge edge

-- | Create the edges from continue statement nodes to the second node.
continueExitEdges :: ([(CFGNode, [BlockEntryType])], CFGNode) -> [BlockEntryType] -> CFGEdges
continueExitEdges (froms, to) entries 
    = concatMap (\ (from, entries') -> continueExitEdge ((from, exits entries'), to)) froms
    where
        exits entries' = take (length entries' - length entries) entries'

-- | Create an edge from a continue statement node to the second node.
continueExitEdge :: ((CFGNode, [BlockEntryType]), CFGNode) -> CFGEdges
continueExitEdge ((fromNode@(from, _), entries), toNode@(to, _))
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, BlockExitsEdge entries)]

-- | Create the edges from break statement nodes to the second node.
breakExitEdges :: ([(CFGNode, [BlockEntryType])], CFGNode) -> [BlockEntryType] -> CFGEdges
breakExitEdges (froms, to) entries
    = concatMap ( \ (from, entries') -> breakExitEdge ((from, exits entries'), to)) froms
    where
        exits entries' = take (length entries' - length entries) entries'

-- | Create an edge from a break statement node to the second node.
breakExitEdge :: ((CFGNode, [BlockEntryType]), CFGNode) -> CFGEdges
breakExitEdge ((fromNode@(from, _), entries), toNode@(to, _))
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise
        = [(from, to, BlockExitsEdge entries)]

-- | Create the edges from return statement nodes to the second node.
returnExitEdges :: ([(CFGNode, [BlockEntryType])], CFGNode) -> CFGEdges
returnExitEdges (froms, to) = concatMap ( \ from -> returnExitEdge (from, to)) froms

-- | Create an edge from a return statement node to the second node.
returnExitEdge :: ((CFGNode, [BlockEntryType]), CFGNode) -> CFGEdges
returnExitEdge ((fromNode@(from, _), entries), toNode@(to, _))
    | fromNode == noneNode || toNode == noneNode
        = []
    | otherwise 
        = [(from, to, BlockExitsEdge entries)]
        
--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- TODO: not used at this moment, used for generating the assumption exp for switch
-- statements.
caseCondExp :: Exp' -> [SwitchBlock'] -> SwitchBlock' -> Exp'
caseCondExp e1 previous (SwitchBlock' Nothing _)   
    = let exps = [e | (SwitchBlock' (Just e) _) <- previous]
       in foldr (\ e -> BinOp' (BinOp' e1 NotEq' e) CAnd') (Lit' (Boolean' True)) exps
caseCondExp e1 _ (SwitchBlock' (Just e2) _) 
    = BinOp' e1 Equal' e2

-- | Returns true if this label belongs to this node.
isLabelOfThisNode :: Maybe String -> CFGNode -> Bool
isLabelOfThisNode _        (_, StatNode (Stmt' (Break' Nothing)))      
    = True
isLabelOfThisNode _        (_, StatNode (Stmt' (Continue' Nothing)))   
    = True
isLabelOfThisNode Nothing  (_, StatNode (Stmt' (Break' (Just _))))    
    = False
isLabelOfThisNode Nothing  (_, StatNode (Stmt' (Continue' (Just _))))  
    = False
isLabelOfThisNode (Just l) (_, StatNode (Stmt' (Break' (Just l'))))    
    = l == l'
isLabelOfThisNode (Just l) (_, StatNode (Stmt' (Continue' (Just l')))) 
    = l == l'
isLabelOfThisNode _        _                                             
    = error "Not a break or continue statement."
