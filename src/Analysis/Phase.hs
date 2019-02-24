module Analysis.Phase where

import           Auxiliary.Phase
import           Auxiliary.Pretty
import           Auxiliary.Arguments
import           Parsing.Syntax
import           Parsing.Pretty
import           Analysis.CFA
import           Analysis.CFG
import           Analysis.Pretty
import qualified Data.Graph.Inductive.Graph     as G
import           Data.Graph.Inductive.Query.DFS      (dfs)

--------------------------------------------------------------------------------
-- Main analysis phase
--------------------------------------------------------------------------------

analysisPhase :: Phase CompilationUnit' CFG
analysisPhase args unit = do 
    newEitherT $ printHeader "2. PROGRAM ANALYSIS"
    cfg <- controlFlowAnalysisSubphase args unit
    reachabilityAnalysisSubphase args cfg
    
--------------------------------------------------------------------------------
-- Control Flow Analysis subphase
--------------------------------------------------------------------------------
             
controlFlowAnalysisSubphase :: Subphase CompilationUnit' CFG
controlFlowAnalysisSubphase _ unit = do
    newEitherT $ printHeader "2.a control flow analysis"
    newEitherT $ printPretty unit
    return $ cfgOfCompilationUnit unit

--------------------------------------------------------------------------------
-- Reachability Analysis subphase
--------------------------------------------------------------------------------

reachabilityAnalysisSubphase :: Subphase CFG CFG
reachabilityAnalysisSubphase Arguments{method} graph@CFG{cfg} = do
    newEitherT $ printHeader "2.b reachability analysis"
    newEitherT $ printPretty graph
    case entryOfMethod method graph of
        Just (init, _) -> return CFG{cfg = reachableFrom init cfg}
        Nothing        -> semanticalError (UndefinedMethodReference method)
        
-- | Returns the subgraph containing the nodes that are
-- reachable from the starting node.
reachableFrom :: G.DynGraph ge => G.Node -> ge a b -> ge a b
reachableFrom s g = G.subgraph (dfs [s] g) g
