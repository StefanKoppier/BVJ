module Linearization.Phase(
      ProgramPath
    , ProgramPaths
    , linearizationPhase
) where

import Data.Graph.Inductive   
import Language.Java.Syntax
import Analysis.Complete
import Control.Phase

type ProgramPath = [CFGNodeValue]

type ProgramPaths = [ProgramPath]

linearizationPhase :: Phase (Node, CFG, Int) ProgramPaths
linearizationPhase verbosity (start, cfg, n) = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printTitled "Input CFG" (prettify cfg)
    return $ paths' [[]] (context cfg start) cfg n

paths' :: ProgramPaths -> CFGContext -> CFG -> Int -> ProgramPaths
paths' acc (_,_,value,[]) cfg n =
    map (\ a -> value : a) acc
paths' acc (_,_,value,[((),neighbour)]) cfg n =
    paths' (map (\ a -> value : a) acc) (context cfg neighbour) cfg n

    --case value of
    --    NodeStmt (IfThen' e s)
    --        -> _
    --    NodeStmt (IfThenElse' e s1 s2)
    --        -> assume e       : _
    --        ++ assume (neg e) : _

assume :: Exp -> CFGNodeValue
assume = NodeStmt . Assume'

neg :: Exp -> Exp
neg = PreNot