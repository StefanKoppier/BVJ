module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive as G
import           Analysis.CFA
import           Analysis.CFG
import           Analysis.Pretty
import           Parsing.Syntax
import           Linearization.Path
import           Linearization.Utility
import           Auxiliary.Phase
import           Auxiliary.Pretty

linearizationPhase :: Phase CFG ProgramPaths
linearizationPhase Arguments{maximumDepth} cfg@(CFG graph start) = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printPretty cfg
    return $ map (reverse . clean) (paths' [[]] cfg (G.context graph start) maximumDepth)

paths' :: ProgramPaths -> CFG -> CFGContext -> Int -> ProgramPaths
paths' acc _ (_, _, _, neighbour:neighbours) 0
    = []

paths' acc (CFG cfg _) (_, _, Stmt' stat, []) n
    = map (stat:) acc

paths' acc cfg (_, _, stat, neighbours) n
    | IfThenElse' exp _ _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp) neighbour cfg n) neighbours

    | While' _ exp _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp) neighbour cfg n) neighbours

    | Stmt' stat <- stat 
        = concatMap (\ neighbour -> next acc stat neighbour cfg n) neighbours
        
next :: ProgramPaths -> Stmt' -> (CFGEdgeValue, G.Node) -> CFG -> Int -> ProgramPaths
next acc stat (Edge, neighbour) cfg@(CFG graph _) n 
    = paths' (map (stat:) acc) cfg (G.context graph neighbour) (n-1)

next acc (Assume' exp) (ConditionalEdge True, neighbour) cfg@(CFG graph _) n 
    = paths' (map (Assume' exp:) acc) cfg (G.context graph neighbour) (n-1)
    
next acc (Assume' exp) (ConditionalEdge False, neighbour) cfg@(CFG graph _) n 
    = paths' (map (Assume' (PreNot' exp):) acc) cfg (G.context graph neighbour) (n-1)

clean :: ProgramPath -> ProgramPath
clean (Empty'     :ss) = clean ss
clean (Break'    _:ss) = clean ss
clean (Continue' _:ss) = clean ss
clean (s:ss)           = s : clean ss
clean []               = []