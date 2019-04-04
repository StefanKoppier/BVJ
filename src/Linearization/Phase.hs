module Linearization.Phase(
    linearizationPhase
) where

import           Data.Graph.Inductive
import qualified Data.Map              as M
import           Data.Stack            as S
import           Data.Accumulator            
import           Data.Maybe
import           Analysis.CFG
import           Analysis.Pretty()
import           Parsing.Syntax
import           Parsing.Utility
import           Linearization.Path
import           Linearization.Renaming
import           Auxiliary.Phase
import           Auxiliary.Pretty

import Debug.Trace

--------------------------------------------------------------------------------
-- Linearization phase
--------------------------------------------------------------------------------

linearizationPhase :: Phase (CompilationUnit', CFG) ProgramPaths
linearizationPhase args@Arguments{maximumDepth, verbosity} (unit, graph@CFG{cfg})
    | (Just method)     <- entryMethod
    , (Just (entry, _)) <- entryOfMain graph = do
        liftIO $ printInformation verbosity graph
        let history   = M.fromList [(n, 0) | (_,MethodEntryNode n) <- labNodes cfg]
        let callStack = S.singleton (method, "main", -1)
        let acc       = (history, M.empty, callStack, [[]], maximumDepth)
        let ps        = map (clean . reverse) $ paths acc graph (context cfg entry)
        liftIO $ printText ("Generated " ++ show (length ps) ++ " program path(s).")
        filteredPs <- filteringPhase args (unit, ps)
        liftIO $ printText ("After filtering " ++ show (length filteredPs) ++ " program path(s) remain.")
        return filteredPs
    | otherwise = do
        liftIO $ printInformation verbosity graph
        throwSemanticalError (UndefinedMethodReference ["main"])
    where
        entryMethod      = findMainScope unit

printInformation :: Verbosity -> CFG -> IO ()
printInformation verbosity graph = do
    printHeader "3. LINEARIZATION"
    case verbosity of
        Informative -> printPretty graph
        _           -> return ()

clean :: ProgramPath -> ProgramPath
clean []                               = []
clean ((PathStmt (Continue' _), i):ps) = (PathStmt Empty', i) : clean ps
clean ((PathStmt (Break' _)   , i):ps) = (PathStmt Empty', i) : clean ps
clean (s:ps)                           = s : clean ps

-- | Subphase allowing filtering of paths.
filteringPhase :: Subphase (CompilationUnit', ProgramPaths) ProgramPaths
filteringPhase Arguments{pathFilter} (unit, paths)
    = return (pathFilter unit paths)

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- | The history of number of calls renamed.
type CallHistory = M.Map Scope Int

-- | A stack frame containing the current method, the current call name, and
-- the node the method should return to.
type StackFrame = (Scope, String, Node)

-- | The stack of method calls.
type CallStack = Stack StackFrame

-- | Accumulator containing the number of calls made, the pending renamings for
-- specific nodes, the call stack, the generated program paths thus far, the 
-- remaining number of statements allowed, and the current scope depth.
type PathAccumulator = (CallHistory, StmtManipulations, CallStack, ProgramPaths, Int)

-- | Generates all program paths from the first until last node up to length k.
paths :: PathAccumulator -> CFG -> CFGContext -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths (_,_,_,_,0) _ (_, _, _, _:_)
    = []

-- Case: a statement.
paths acc graph (_,currentNode, StatNode (Stmt' stat), neighbours)
    = concatMap (next (prepend currentNode (PathStmt stat) acc) graph . createEdge currentNode) neighbours

-- Case: a compound statement.
paths acc graph (_, currentNode, StatNode _, neighbours)
    = concatMap (next acc graph . createEdge currentNode) neighbours

-- Case: a catch node.
paths acc graph (_, currentNode, CatchNode _, neighbours)
    = concatMap (next acc graph . createEdge currentNode) neighbours

-- Case: a finally node.
paths acc graph (_, currentNode, FinallyNode _, neighbours)
    = concatMap (next acc graph . createEdge currentNode) neighbours

-- Case: a method call node.
paths (history,manipulations,callStack,paths,k) graph (_, currentNode, CallNode scope statNode name, [edge@(_,neighbour)])
    = let callNumber     = history M.! scope
          history1       = M.insert scope (callNumber + 1) history
          newName        = renameMethodName scope callNumber
          manipulations1 = insertManipulation statNode name (scope, callNumber) manipulations
          callStack1     = S.push (scope, newName, currentNode + 1) callStack
          acc1           = (history1, manipulations1, callStack1, paths, k)
       in next acc1 graph (createEdge currentNode edge)

-- Case: the entry of a method.
paths acc graph (_,currentNode, MethodEntryNode scope, neighbours)
    = concatMap (next acc graph . createEdge currentNode) neighbours

-- Case: the exit of a method with no neighbours, i.e. the final statement.
paths (_,_,_,paths,_) graph (_,_, MethodExitNode scope, [])
    = paths

-- Case: the exit of a method.
paths (history, manipulations, callStack, ps, k) graph (_,currentNode, MethodExitNode scope, neighbours)
    = let (_, _, destination) = fromJust $ S.peek callStack
          acc1                = (history, manipulations, S.pop callStack, ps, k)
          [(edgeValue, _)]    = filter ((destination ==) . snd) neighbours
       in next acc1 graph (currentNode, destination, edgeValue)

-- | Create a CFGEdge from a node a destination, and edge value.
createEdge :: Node -> (CFGEdgeValue, Node) -> CFGEdge
createEdge currentNode (edgeValue, destinationNode)
    = (currentNode, destinationNode, edgeValue)

-- | Add the information of the edge to the program paths.
next :: PathAccumulator -> CFG -> CFGEdge -> ProgramPaths
next acc graph@CFG{cfg} (currentNode, destinationNode, edgeValue)
    = let neighbour = context cfg destinationNode
       in case edgeValue of
            InterEdge _  
                -> paths acc graph neighbour
            IntraEdge      
                -> paths acc graph neighbour
            BlockEntryEdge entry 
                -> case entry of
                    ConditionalEntryType (Just e)
                        -> let acc1 = (prepend currentNode (PathEntry entry) . prepend currentNode (PathStmt $ Assume' e)) acc
                            in paths acc1 graph neighbour
                    _   -> paths (prepend currentNode (PathEntry entry) acc) graph neighbour
            BlockExitEdge entry 
                -> paths (prepend currentNode (PathExit entry) acc) graph neighbour
            BlockExitEntryEdge exit entry 
                -> case entry of
                    ConditionalEntryType (Just e)
                        -> let acc1 = (prepend currentNode (PathExitEntry exit entry) . prepend currentNode (PathStmt $ Assume' e)) acc
                            in paths acc1 graph neighbour
                    _   -> paths (prepend currentNode (PathExitEntry exit entry) acc) graph neighbour

-- | Prepend a path item to all program paths and rename the method calls in the
-- expression of the statement if necessary.
prepend :: Node -> PathType -> PathAccumulator -> PathAccumulator
prepend currentNode stat (history, manipulations, callStack, ps, k)
    = let (scope, name, _) = fromJust $ S.peek callStack
       in case stat of
            PathStmt s 
                -> let (manipulations1, stat1) = rename manipulations currentNode s
                       k' = case s of Empty' -> k; _ -> k - 1
                    in (history, manipulations1, callStack, map ((stat1, (scope, name)):) ps, k')
            _   -> (history, manipulations, callStack, map ((stat, (scope, name)):) ps, k)
        
-- | Rename the statement of the given node, if there are any renamings to be done.
rename :: StmtManipulations -> Node -> Stmt' -> (StmtManipulations, PathType)
rename manipulations currentNode stat 
    = let renaming           = manipulations M.!? currentNode
          (renaming1, stat1) = PathStmt <$> maybe (M.empty, stat) (runAccumulator (renameStmt stat)) renaming
          manipulations1     = M.insert currentNode renaming1 manipulations
       in (manipulations1, stat1) 
