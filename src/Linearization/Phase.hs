module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import           Data.List
import           Analysis.CFA
import           Analysis.CFG
import           Analysis.Pretty
import           Parsing.Syntax
import           Linearization.Path
import           Linearization.Utility
import           Auxiliary.Phase
import           Auxiliary.Pretty

import Debug.Trace

linearizationPhase :: Phase (CompilationUnit', CFG) ProgramPaths
linearizationPhase Arguments{maximumDepth,method} (unit, graph@CFG{cfg}) = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printPretty graph
    case entryOfMethod method graph of
        Just (entry,_) -> 
            return $ map (reverse . clean) (paths [[]] method graph (G.context cfg entry) maximumDepth)
        Nothing        -> 
            left $ MethodNotFound method

paths :: ProgramPaths -> Name' -> CFG -> CFGContext -> Int -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths _ _ _ (_,_,_,neighbour:neighbours) 0
    = []

-- Case: the entry of an method.
paths acc _ graph@CFG{cfg} (_,_,Entry scope,[(_,neighbour)]) n
    = paths acc scope graph (G.context cfg neighbour) n

-- Case: the exit of an method.
paths acc _ _ (_,_,Exit _,neighbours) n
    = acc

-- Case: a statement.
paths acc scope graph@CFG{cfg} (_,_,Block s,neighbours) n
    = let intras' = intras neighbours
          inters' = inters neighbours
          acc'    = merge acc (map (\ (_ ,neighbour) -> paths [[]] noScope graph (G.context cfg neighbour) n) inters')
       in concatMap (next acc' scope s graph n) intras'

next :: ProgramPaths -> Name' -> CompoundStmt' -> CFG -> Int -> (CFGEdgeValue, G.Node) -> ProgramPaths
next acc scope s graph@CFG{cfg} n (edge,neighbour)
    = paths (map (stat:) acc) scope graph (G.context cfg neighbour) (n-1)
    where
        stat | ConditionalEdge e <- edge = (scope, Assume' e)
             | (Stmt' s')        <- s    = (scope, s')

merge :: ProgramPaths -> [ProgramPaths] -> ProgramPaths
merge = foldl (\ acc call -> concatMap (\ a -> map (a++) call) acc)

-- | Retrieve all inter flow edges from the list.
inters :: CFGAdj -> CFGAdj
inters = filter (isInterEdge . fst)             

-- | Retrieve all intra and conditional flow edges from the list.
intras :: CFGAdj -> CFGAdj
intras = filter (isIntraEdge . fst)

clean :: ProgramPath -> ProgramPath
clean ((_,Empty'     ):ss) = clean ss
clean ((_,Break'    _):ss) = clean ss
clean ((_,Continue' _):ss) = clean ss
clean (s:ss)               = s : clean ss
clean []                   = []

noScope :: Name'
noScope = ["NOSCOPE"]