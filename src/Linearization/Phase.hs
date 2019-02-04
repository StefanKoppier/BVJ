module Linearization.Phase(
      ProgramPath
    , ProgramPaths
    , linearizationPhase
) where

import qualified Data.Graph.Inductive as G
import           Language.Java.Syntax
import           Analysis.Complete
import           Linearization.Utility
import           Control.Phase

type ProgramPath = [Stmt']

type ProgramPaths = [ProgramPath]

linearizationPhase :: Phase (G.Node, CFG, Int) ProgramPaths
linearizationPhase verbosity (start, cfg, n) = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printTitled "Input CFG" (G.prettify cfg)
    return $ map reverse (paths' [[]] cfg (G.context cfg start) n)

paths' :: ProgramPaths -> CFG -> CFGContext -> Int -> ProgramPaths
paths' acc cfg (_, _, _, neighbour:neighbours) 0
    = []

paths' acc cfg (_, _, Stmt' stat, []) n
    = map (stat:) acc

paths' acc cfg (_, _, stat, neighbours) n
    | IfThenElse' exp _ _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp Nothing) neighbour cfg n) neighbours

    | While' _ exp _ <- stat 
        = concatMap (\ neighbour -> next acc (Assume' exp Nothing) neighbour cfg n) neighbours

    | Stmt' stat <- stat 
        = concatMap (\ neighbour -> next acc stat neighbour cfg n) neighbours
        
next :: ProgramPaths -> Stmt' -> (CFGEdgeValue, G.Node) -> CFG -> Int -> ProgramPaths
next acc stat (Edge, neighbour) cfg n 
    = paths' (map (stat:) acc) cfg (G.context cfg neighbour) (n-1)

next acc (Assume' exp _) (ConditionalEdge True, neighbour) cfg n 
    = paths' (map (Assume' exp Nothing:) acc) cfg (G.context cfg neighbour) (n-1)
    
next acc (Assume' exp _) (ConditionalEdge False, neighbour) cfg n 
    = paths' (map (Assume' (PreNot exp) Nothing:) acc) cfg (G.context cfg neighbour) (n-1)