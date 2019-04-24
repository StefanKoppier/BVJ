{-|
Module      : Linearization.Phase
Description : Module containing the linearization phase.
-}
module Linearization.Phase(
    linearizationPhase
) where

import           Data.Graph.Inductive
import qualified Data.Map              as M
import           Data.Stack            as S
import           Data.List.Split
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

--------------------------------------------------------------------------------
-- Linearization phase
--------------------------------------------------------------------------------

-- | Generates the program paths from the given control flow graph and AST.
linearizationPhase :: Phase (CompilationUnit', CFG) ProgramPaths
linearizationPhase Arguments{function, maximumDepth, verbosity} (unit, graph@CFG{cfg})
    | (Just method)     <- entryMethod
    , (Just (entry, _)) <- entryOfMethod method graph = do
        liftIO $ printInformation verbosity graph
        let history   = M.fromList [(n, 0) | (_,MethodEntryNode n) <- labNodes cfg]
        let callStack = S.singleton (method, functionName, -1)
        let acc       = (history, M.empty, callStack, [[]], maximumDepth)
        let ps        = map reverse $ paths acc graph (context cfg entry)
        liftIO $ printText ("Generated " ++ show (length ps) ++ " program path(s).")
        return ps
    | otherwise = do
        liftIO $ printInformation verbosity graph
        throwSemanticalError (UndefinedMethodReference [functionName])
    where
        functionName = maybe "main" (last . splitOn ".") function 
        entryMethod  = maybe (findMainScope unit) (findMethodScope unit . splitOn ".") function 

-- | Prints information about the linearization phase to the screen.
printInformation :: Verbosity -> CFG -> IO ()
printInformation verbosity graph = do
    printHeader "3. LINEARIZATION"
    case verbosity of
        Informative -> printPretty graph
        _           -> return ()

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
paths acc graph (_, currentNode, StatNode (Stmt' stat), neighbours)
    = concatMap (next (prepend currentNode (PathStmt stat) acc) graph . createEdge currentNode) neighbours

-- Case: a for initializer node.
paths acc graph (_, currentNode, ForInitNode forInit, neighbours)
    = concatMap (next (prepends currentNode acc stats) graph . createEdge currentNode) neighbours
    where
        stats = case forInit of
                    ForLocalVars' modifiers ty decls
                        -> [PathStmt (Decl' modifiers ty decls)]
                    ForInitExps' exps
                        -> map (PathStmt . ExpStmt') exps

-- Case: a for update node.
paths acc graph (_, currentNode, ForUpdateNode exps, neighbours)
    = concatMap (next (prepends currentNode acc stats) graph . createEdge currentNode) neighbours
    where
        stats = map (PathStmt . ExpStmt') exps

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
paths (history, manipulations, callStack, ps, k) graph (_, currentNode, CallNode scope statNode name, [edge])
    = let callNumber     = history M.! scope
          history1       = M.insert scope (callNumber + 1) history
          newName        = renameMethodName scope callNumber
          manipulations1 = insertManipulation statNode name (scope, callNumber) manipulations
          callStack1     = S.push (scope, newName, currentNode + 1) callStack
          acc1           = (history1, manipulations1, callStack1, ps, k)
       in next acc1 graph (createEdge currentNode edge)

-- Case: the entry of a method.
paths acc graph (_,currentNode, MethodEntryNode _, neighbours)
    = concatMap (next acc graph . createEdge currentNode) neighbours

-- Case: the exit of a method.
paths (history, manipulations, callStack, ps, k) graph (_,currentNode, MethodExitNode _, neighbours)
    -- Case: the exit of a method with one stack frame left, i.e. the final statement.
    | S.size callStack == 1
        = ps

    -- Case: the non-final exit of a method.
    | otherwise
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
                -> let acc1 = prepends currentNode acc (getEdgeValues [BlockEntryEdge entry]) 
                    in paths acc1 graph neighbour
            BlockExitEdge exit
                -> let acc1 = prepends currentNode acc (getEdgeValues [BlockExitEdge exit]) 
                    in paths acc1 graph neighbour
            BlockExitsEdge exits
                -> let acc1 = prepends currentNode acc (getEdgeValues (map BlockExitEdge (reverse exits)))
                    in paths acc1 graph neighbour
            BlockExitEntryEdge exit entry
                -> let acc1 = prepends currentNode acc (getEdgeValues [ BlockEntryEdge entry
                                                                      , BlockExitEdge exit])  
                    in paths acc1 graph neighbour

-- | Maps the dge values to their corresponding program path values.
getEdgeValues :: [CFGEdgeValue] -> [PathType]
getEdgeValues []
    = []
getEdgeValues (BlockEntryEdge entry : stats) 
    = PathEntry entry : conditionalAssumption entry ++ getEdgeValues stats
getEdgeValues (BlockExitEdge exit : stats)
    = PathExit exit : conditionalAssumption exit ++ getEdgeValues stats
        
-- | Generate assumption statement from the conditional edge.
conditionalAssumption :: BlockEntryType -> [PathType]
conditionalAssumption (ConditionalEntryType (Just e)) = [PathStmt $ Assume' e] 
conditionalAssumption _                               = [] 

-- | Prepend the path items to all program paths.
prepends :: Node -> PathAccumulator -> [PathType]  -> PathAccumulator
prepends currentNode = foldr (prepend currentNode)

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
