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
linearizationPhase Arguments{maximumDepth} graph@CFG{cfg} = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printPretty graph
    return []
    --return $ map (reverse . clean) (paths' [[]] graph (G.context cfg entry) maximumDepth)
{-
paths' :: ProgramPaths -> CFG -> CFGContext -> Int -> ProgramPaths
paths' acc _ (_, _, _, neighbour:neighbours) 0
    = []

paths' acc _ (_, _, Stmt' stat, []) n
    = map (stat:) acc

paths' acc cfg (_, _, stat, neighbours) n
    | IfThenElse' exp _ _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp) neighbour cfg n) neighbours

    | While' _ exp _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp) neighbour cfg n) neighbours

    | Stmt' stat <- stat 
        = concatMap (\ neighbour -> next acc stat neighbour cfg n) neighbours
        
next :: ProgramPaths -> Stmt' -> (CFGEdgeValue, G.Node) -> CFG -> Int -> ProgramPaths
next acc stat (IntraEdge, neighbour) graph@CFG{cfg} n 
    = paths' (map (stat:) acc) graph (G.context cfg neighbour) (n-1)

next acc (Assume' exp) (ConditionalEdge True, neighbour) graph@CFG{cfg} n 
    = paths' (map (Assume' exp:) acc) graph (G.context cfg neighbour) (n-1)
    
next acc (Assume' exp) (ConditionalEdge False, neighbour) graph@CFG{cfg} n 
    = paths' (map (Assume' (PreNot' exp):) acc) graph (G.context cfg neighbour) (n-1)

clean :: ProgramPath -> ProgramPath
clean (Empty'     :ss) = clean ss
clean (Break'    _:ss) = clean ss
clean (Continue' _:ss) = clean ss
clean (s:ss)           = s : clean ss
clean []               = []
-}