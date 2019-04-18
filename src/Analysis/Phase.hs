{-|
Module      : Analysis.Phase
Description : Module containing the analysis phase.

This module contains contains the analysis phase. 
Currently, it only contains the control flow anaylsis to construct to the control
flow graph. Reachability analysis is disabled.
-}
module Analysis.Phase(
      analysisPhase
    , controlFlowAnalysisSubphase
) where

import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Pretty()
import Analysis.CFA
import Analysis.CFG
import Analysis.Pretty()

--------------------------------------------------------------------------------
-- Main analysis phase
--------------------------------------------------------------------------------

-- | Performs the analysis on the compilation unit and returns the control flow
-- graph. Currently it only performs control flow analysis.
analysisPhase :: Phase CompilationUnit' CFG
analysisPhase args unit = do 
    liftIO $ printHeader "2. PROGRAM ANALYSIS"
    controlFlowAnalysisSubphase args unit
    
--------------------------------------------------------------------------------
-- Control Flow Analysis subphase
--------------------------------------------------------------------------------
             
-- | Performs the control flow analysis on the compilation unit.
controlFlowAnalysisSubphase :: Subphase CompilationUnit' CFG
controlFlowAnalysisSubphase _ unit = do
    liftIO $ printHeader "2.a control flow analysis"
    liftIO $ printPretty unit
    return (cfgOfCompilationUnit unit)

--------------------------------------------------------------------------------
-- Reachability Analysis subphase
--
-- Disabled as we can now verify any function, instead of only the main function.
-- The code is left here to be reïmplemented if wanted.
--------------------------------------------------------------------------------

{-
reachabilityAnalysisSubphase :: Subphase CFG CFG
reachabilityAnalysisSubphase Arguments{function} graph@CFG{cfg} = do
    liftIO $ printHeader "2.b reachability analysis"
    liftIO $ printPretty graph
    case entryOfMethod scope graph of
        Just (init, _) -> return CFG{cfg = reachableFrom init cfg}
        Nothing        -> throwSemanticalError (UndefinedMethodReference ["main"])
        
-- | Returns the subgraph containing the nodes that are
-- reachable from the starting node.
reachableFrom :: G.DynGraph ge => G.Node -> ge a b -> ge a b
reachableFrom s g = G.subgraph (dfs [s] g) g
-}
