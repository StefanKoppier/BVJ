module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive  as G
import qualified Data.Map              as M
import           Data.Stack            as S
import           Data.Maybe
import           Data.List                  (mapAccumL)
import           Analysis.CFG
import           Analysis.Pretty()
import           Parsing.Syntax
import           Linearization.Path
import           Linearization.Renaming
import           Auxiliary.Phase
import           Auxiliary.Pretty

linearizationPhase :: Phase CFG ProgramPaths
linearizationPhase Arguments{maximumDepth, method} graph@CFG{cfg} = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printPretty graph
    case entryOfMethod method graph of
        Just (entry,_) -> do
            let (Scope _ _ scopeMember) = method
            let history   = M.fromList [(n, 0) | (_,Entry n) <- G.labNodes cfg]
            let callStack = S.singleton (method, scopeMember, -1)
            let acc       = (history, M.empty, callStack, [[]], maximumDepth)
            let ps        = paths acc graph (G.context cfg entry)
            return . map reverse $ ps
        Nothing        -> semanticalError (UndefinedMethodReference method)

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- | The history of number of calls renamed.
type CallHistory = M.Map Scope Int

-- | A stack frame containing the current method, the current call name, and 
-- the node the method should return to.
type StackFrame = (Scope, String, G.Node)

-- | The stack of method calls.
type CallStack = Stack StackFrame

type Accumulator = (CallHistory, StmtManipulations, CallStack, ProgramPaths, Int)

paths :: Accumulator -> CFG -> CFGContext -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths (_,_,_,_,0) _ (_, _, _, _:_)
    = []

-- Case: the final statement.
paths (_,_,_,ps,_)  _ (_,_,Exit _, [])
    = ps

-- Case: the entry of an method.
paths acc graph@CFG{cfg} (_,_,Entry _,[(_,neighbour)])
    = paths acc graph (G.context cfg neighbour)
    
-- Case: the exit of an method.
paths (history, manipulations, callStack, ps, k) graph@CFG{cfg} (_,_,Exit _,_)
    = let (_, _, destination) = fromJust $ S.peek callStack
          acc'                = (history, manipulations, S.pop callStack, ps, k)
       in paths acc' graph (G.context cfg destination)

-- Case: the call of an method.
paths (history, manipulations, callStack, ps, k) graph@CFG{cfg} (_,node, Call method statNode name, [(_,neighbour)])
    = let callNumber     = history M.! method
          history'       = M.insert method (callNumber + 1) history
          newName        = renameMethodName method callNumber
          manipulations' = insertManipulation statNode name (method, callNumber) manipulations
          callStack'     = S.push (method, newName, node + 1) callStack
          acc'           = (history', manipulations', callStack', ps, k)
       in paths acc' graph (G.context cfg neighbour)

-- Case: a statement.
paths acc graph (_, node, Block s, neighbours)
    = concatMap (next acc node s graph) neighbours

next :: Accumulator -> G.Node -> CompoundStmt' -> CFG -> (CFGEdgeValue, G.Node) -> ProgramPaths
next (history, manipulations, callStack, ps, k) node s graph@CFG{cfg} (edge, neighbour) 
    = let pathInfo = PathStmtInfo{callName=callName', original=scope'}
          acc'     = (history, manipulations', callStack, map ((stat', pathInfo):) ps, k-1)
       in paths acc' graph (G.context cfg neighbour)
    where
        renaming                           = manipulations M.!? node
        manipulations'                     =  M.insert node renaming' manipulations
        (scope', callName', _)             = fromJust $ S.peek callStack
        stat | ConditionalEdge e <- edge   = Assume' e
             | (Stmt' s')        <- s      = s'
        (renaming', stat') = maybe (M.empty, stat) (\ r -> renameStmt r stat) renaming
           