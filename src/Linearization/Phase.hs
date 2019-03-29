module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive  as G
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
        let history   = M.fromList [(n, 0) | (_,Entry n) <- G.labNodes cfg]
        let callStack = S.singleton (method, "main", -1, 0)
        let acc       = (history, M.empty, callStack, [[]], maximumDepth, 0)
        let ps        = map (clean . reverse) $ paths acc graph (G.context cfg entry)
        filteringPhase args (unit, ps)
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

-- | Placeholder subphase to allow filtering of specific program paths.
-- currently implemented as doing nothing.
filteringPhase :: Subphase (CompilationUnit', ProgramPaths) ProgramPaths
filteringPhase _ (unit, paths)
    = return paths

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- | The history of number of calls renamed.
type CallHistory = M.Map Scope Int

-- | A stack frame containing the current method, the current call name, 
-- the node the method should return to, and the previous scope depth.
type StackFrame = (Scope, String, G.Node, Int)

-- | The stack of method calls.
type CallStack = Stack StackFrame

-- | Accumulator containing the number of calls made, the pending renamings for
-- specific nodes, the call stack, the generated program paths thus far, the 
-- remaining number of statements allowed, and the current scope depth.
type PathAccumulator = (CallHistory, StmtManipulations, CallStack, ProgramPaths, Int, Int)

paths :: PathAccumulator -> CFG -> CFGContext -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths (_,_,_,_,0,_) _ (_, _, _, _:_)
    = []

-- Case: the final statement.
paths (_,_,callStack,ps,_,_)  _ (_,_,Exit _, [])
    = let (method, name, _, _) = fromJust $ S.peek callStack
          entry = (PathExit $ ETMethod name method, (method, name))
       in map (entry :) ps

-- Case: the entry of the unknown method.
paths _ _ (_,_, Entry scope, [])
    = []

-- Case: the entry of an method.
paths (history, manipulations, callStack, ps, k, _) graph@CFG{cfg} (_,_,Entry _,[(_,neighbour)])
    = let (method, name, _, _) = fromJust $ S.peek callStack
          acc1  = (history, manipulations, callStack, ps, k, 0)
          entry = (PathEntry $ ETMethod name method, (method, name))
          acc2  = prepend entry False acc1
       in paths acc2 graph (G.context cfg neighbour)
    
-- Case: the exit of an method.
paths (history, manipulations, callStack, ps, k, _) graph@CFG{cfg} (_,_,Exit _,_)
    = let (method, name, destination, s) = fromJust $ S.peek callStack
          acc1                   = (history, manipulations, S.pop callStack, ps, k, s)
          entry                  = (PathExit $ ETMethod name method, (method, name))
          acc2                   = prepend entry False acc1
       in paths acc2 graph (G.context cfg destination)

-- Case: the call of an method.
paths (history, manipulations, callStack, ps, k, s) graph@CFG{cfg} (_,node, Call method statNode name, [(_,neighbour)])
    = let callNumber     = history M.! method
          history'       = M.insert method (callNumber + 1) history
          newName        = renameMethodName method callNumber
          manipulations' = insertManipulation statNode name (method, callNumber) manipulations
          callStack'     = S.push (method, newName, node + 1, s) callStack
          acc1           = (history', manipulations', callStack', ps, k, s)
       in paths acc1 graph (G.context cfg neighbour)

-- Case: a statement.
paths acc graph (_, node, Block s, neighbours)
    = concatMap (\ (edge, neighbour) -> next acc s graph (node, neighbour, edge)) neighbours

paths (history, manipulations, callStack, ps, k, s) graph@CFG{cfg} (_, node, Catch (Catch' c _), [(_,neighbour)])
    = let (scope, callName, _, _) = fromJust $ S.peek callStack
          metaInfo                = (scope, callName)
          entries                 = [(PathEntry (ETCatch c), metaInfo), (PathExit ETTry, metaInfo)]
          acc1                    = (history, manipulations, callStack, map (entries ++) ps, k, s)
       in paths acc1 graph (G.context cfg neighbour)

next :: PathAccumulator -> CompoundStmt' -> CFG -> CFGEdge -> ProgramPaths
next (history, manipulations, callStack, ps, k, s) Try'{} graph@CFG{cfg} (node, neighbour, edge)
    = let (scope, callName, _, _) = fromJust $ S.peek callStack
          metaInfo                = (scope, callName)
          entry                   = (PathEntry ETTry, metaInfo)
          acc1                    = (history, manipulations, callStack, map (entry:) ps, k, s)
       in paths acc1 graph (G.context cfg neighbour)

next (history, manipulations, callStack, ps, k, s) stat1 graph@CFG{cfg} (node, neighbour, edge)
    = let (k', s') = (k-1, s)
          metaInfo = (scope', callName')
          acc'     = (history, manipulations', callStack, map ((stat4, metaInfo):) ps, k', s')
       in paths acc' graph (G.context cfg neighbour)
    where
        renaming                             = manipulations M.!? node
        manipulations'                       = M.insert node renaming' manipulations
        (scope', callName', _, _)            = fromJust $ S.peek callStack
        stat3 | ConditionalEdge e _ <- edge  = Assume' e
              | (Stmt' stat2)       <- stat1 = stat2
        (renaming', stat4) 
            = PathStmt <$> maybe (M.empty, stat3) (runAccumulator (renameStmt stat3)) renaming

scopeModificationOfEdge :: CFGEdgeValue -> Int
scopeModificationOfEdge (InterEdge _ s)       = s
scopeModificationOfEdge (ConditionalEdge _ s) = s
scopeModificationOfEdge (IntraEdge s)         = s

prepend :: PathStmt -> Bool -> PathAccumulator -> PathAccumulator
prepend stat decreaseK (history, manipulations', callStack, ps, k, s)
    = (history, manipulations', callStack, map (stat:) ps, k', s)
    where
        k' = if decreaseK then k-1 else k