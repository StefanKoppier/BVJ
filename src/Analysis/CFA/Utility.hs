module Analysis.CFA.Utility where

import qualified Data.Map                   as M
import           Data.Graph.Inductive.Graph
import           Parsing.Syntax
import           Analysis.CFG

type Methods = M.Map Scope (Node, Node)

type CFGDepthNode  = (CFGNode, Int)

type CFGDepthNodes = [CFGDepthNode]

caseCondExp :: Exp' -> [SwitchBlock'] -> SwitchBlock' -> Exp'
caseCondExp e1 previous (SwitchBlock' Nothing _)   
    = let exps = [e | (SwitchBlock' (Just e) _) <- previous]
       in foldr (\ e -> BinOp' (BinOp' e1 NotEq' e) CAnd') (Lit' (Boolean' True)) exps
caseCondExp e1 _ (SwitchBlock' (Just e2) _) 
    = BinOp' e1 Equal' e2

unknownScope :: Scope
unknownScope = Scope Nothing "UNKNOWN" "UNKNOWN"

unknownNode :: (Node, CFGNodeValue)
unknownNode = (-1, Entry unknownScope)

noNode :: (Node, CFGNodeValue)
noNode = (-2, undefined)

new :: Node -> Node
new = (1+)

newIfJust :: Maybe a -> Node -> Node
newIfJust = maybe id (const new)

call :: Node -> Scope -> Node -> Name' -> CFGNode
call n scope statNode name = (n, Call scope statNode name)

block :: Node -> CompoundStmt' -> CFGNode
block n s = (n, Block s)

catch :: Node -> Catch' -> CFGNode
catch n c = (n, Catch c)

entry :: Node -> Scope -> CFGNode
entry n scope = (n, Entry scope)

exit :: Node -> Scope -> CFGNode
exit n scope =  (n, Exit scope)

seqEdges :: CompoundStmts' -> CFGDepthNodes -> CFGNode -> CFGEdges
seqEdges [] _ _ = []
seqEdges (stat:_) final init
    | init == noNode
        = []
    | Block' _ <- stat
        = intraEdges final init 1
    | otherwise
        = intraEdges final init 0

intraEdge :: CFGNode -> CFGNode -> Int -> CFGEdges
intraEdge (x,_) yNode@(y,_) s 
    | noNode == yNode = []
    | otherwise       = [(x, y, IntraEdge s)]

intraEdges :: CFGDepthNodes -> CFGNode -> Int -> CFGEdges
intraEdges xs' y s
    | noNode == y = []
    | otherwise   = concatMap (\ x -> intraEdge x y s) xs
    where
        xs = map fst xs'

callEdge :: (Node, Scope) -> Methods -> CFGEdge
callEdge (node, scope) methods
    | scope == unknownScope
        = (node, fst unknownNode, InterEdge unknownScope 0)
    | Just method' <- method
        = (node, fst method', InterEdge scope 0)
    | Nothing  <- method
        = (node, fst unknownNode, InterEdge unknownScope 0)
    where
        method = methods M.!? scope

returnEdge :: (Scope, Node) -> Methods -> CFGEdges
returnEdge (scope, node) methods
    | scope == unknownScope
        = []
    | Just method' <- method
        = [(snd method', node, InterEdge scope 0)]
    | Nothing      <- method
        = []
    where
        method = methods M.!? scope

condEdge :: CFGNode -> CFGNode -> Exp' -> Int -> CFGEdges
condEdge (x,_) yNode@(y,_) e s 
    | yNode == noNode = []
    | otherwise       = [(x, y, ConditionalEdge e s)]

isLabelOfThisNode :: Maybe String -> CFGDepthNode -> Bool
isLabelOfThisNode _        ((_, Block (Stmt' (Break' Nothing))), _)      = True
isLabelOfThisNode _        ((_, Block (Stmt' (Continue' Nothing))), _)   = True
isLabelOfThisNode Nothing  ((_, Block (Stmt' (Break' (Just _)))), _)     = False
isLabelOfThisNode Nothing  ((_, Block (Stmt' (Continue' (Just _)))), _)  = False
isLabelOfThisNode (Just l) ((_, Block (Stmt' (Break' (Just l')))), _)    = l == l'
isLabelOfThisNode (Just l) ((_, Block (Stmt' (Continue' (Just l')))), _) = l == l'
isLabelOfThisNode _        _                                             = error "Not a break or continue statement."
    