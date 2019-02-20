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
        Just (entry,_) -> do
            let initial = M.fromList [(n,(0,0)) | (_,Entry n) <- G.labNodes cfg]
            return $ map (reverse . clean) (snd $ paths [[]] initial method graph (G.context cfg entry) maximumDepth)
        Nothing        -> 
            left $ MethodNotFound method

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- Call history, the first value represents the scope renaming, the second
-- one represents the call renaming.
type CallHistory = M.Map Name' (Int, Int)

paths :: ProgramPaths -> CallHistory -> Name' -> CFG -> CFGContext -> Int -> (CallHistory, ProgramPaths)
-- Case: the end is not reached and the maximum path length is reached.
paths _ calls _ _ (_,_,_,neighbour:neighbours) 0
    = (calls, [])

-- Case: the entry of an method.
paths acc calls scope graph@CFG{cfg} (_,_,Entry _,[(_,neighbour)]) n
    = paths acc calls scope graph (G.context cfg neighbour) n

-- Case: the exit of an method.
paths acc calls _ _ (_,_,Exit _,neighbours) n
    = (calls, acc)

-- Case: a statement.
paths acc calls scope graph@CFG{cfg} (_,_,Block s,neighbours) n
    = let intras'        = intras neighbours
          inters'        = inters neighbours
          (calls1,acc')  = interPaths acc calls graph inters' n
          (calls2,paths) = mapAccumR (next acc' scope s graph n) calls1 intras'
       in (calls2, concat paths)

next :: ProgramPaths -> Name' -> CompoundStmt' -> CFG -> Int -> CallHistory -> (CFGEdgeValue, G.Node) -> (CallHistory, ProgramPaths)
next acc scope s graph@CFG{cfg} n calls (edge,neighbour)
    = paths (map ((scope,stat):) acc) calls' scope graph (G.context cfg neighbour) (n-1)
    where
        (calls',stat) | ConditionalEdge e <- edge = renameStmt calls (Assume' e)
                      | (Stmt' s')        <- s    = renameStmt calls s'

interPaths :: ProgramPaths -> CallHistory -> CFG -> CFGAdj -> Int -> (CallHistory, ProgramPaths)
interPaths acc calls graph neighbours n
    = let (calls', paths) = mapAccumR (interPath graph n) calls neighbours
       in (calls', merge acc paths)

interPath :: CFG -> Int -> CallHistory -> (CFGEdgeValue, G.Node) -> (CallHistory, ProgramPaths)
interPath graph@CFG{cfg} n calls (InterEdge method, neighbour) 
    = let (callNumber,x) = calls M.! method
          newName        = [head method ++ "$" ++ show callNumber]
          calls1         = M.insert method (callNumber + 1,x) calls
       in paths [[]] calls1 newName graph (G.context cfg neighbour) n

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

--------------------------------------------------------------------------------
-- Renaming of method calls
--------------------------------------------------------------------------------

renameStmt :: CallHistory -> Stmt' -> (CallHistory, Stmt')
renameStmt history (Decl' ms ty vars) =
    let (history', vars') = mapAccumR renameVarDecl history vars
     in (history', Decl' ms ty vars')
renameStmt history (Assert' e err) = 
    let (history', e')    = renameExp history e
        (history'', err') = renameMaybeExp history' err
     in (history'', Assert' e' err')
renameStmt history (Assume' e) = 
    let (history', e') = renameExp history e
     in (history', Assume' e')
renameStmt history (Return' e) = 
    let (history', e') = renameMaybeExp history e
     in (history', Return' e')
renameStmt history (ExpStmt' e) =
    let (history', e') = renameExp history e
     in (history', ExpStmt' e')
renameStmt history s = (history, s)

renameVarDecl :: CallHistory -> VarDecl' -> (CallHistory, VarDecl')
renameVarDecl history (VarDecl' id init) 
    = let (history', init') = renameVarInit history init
       in (history', VarDecl' id init')

renameVarInit :: CallHistory -> VarInit' -> (CallHistory, VarInit')
renameVarInit history (InitExp' e)
    = let (history', e') = renameExp history e
       in (history', InitExp' e')
renameVarInit history (InitArray' Nothing)
    = (history, InitArray' Nothing)
renameVarInit history (InitArray' (Just is))
    = let (history', is') = mapAccumR renameVarInit history is
       in (history', InitArray' (Just is'))

renameMaybeExp :: CallHistory -> Maybe Exp' -> (CallHistory, Maybe Exp')
renameMaybeExp history Nothing = (history, Nothing)
renameMaybeExp history (Just e) 
    = let (history', e') = renameExp history e in (history', Just e')

renameExp :: CallHistory -> Exp' -> (CallHistory, Exp')
renameExp history (Lit' x)
    = (history, Lit' x)
renameExp history (ArrayCreate' ty ss u)
    = let (history', ss') = mapAccumR renameExp history ss
       in (history', ArrayCreate' ty ss' u)
renameExp history (MethodInv' (MethodCall' n args)) 
    = let (history', args') = mapAccumR renameExp history args
          (x,callNumber)    = history M.! n
          newName           = [head n ++ "$" ++ show callNumber]
          history''         = M.insert n (x,callNumber + 1) history'
       in (history'', MethodInv' (MethodCall' newName args'))
renameExp history (ArrayAccess' i is)
    = let (history', is') = mapAccumR renameExp history is
       in (history', ArrayAccess' i is')
renameExp history (ExpName' n)
    = (history, ExpName' n)
renameExp history (PostIncrement' e)
    = let (history', e') = renameExp history e in (history', PostIncrement' e')
renameExp history (PostDecrement' e)
    = let (history', e') = renameExp history e in (history', PostDecrement' e')
renameExp history (PreIncrement' e)
    = let (history', e') = renameExp history e in (history', PreIncrement' e')
renameExp history (PreDecrement' e)
    = let (history', e') = renameExp history e in (history', PreDecrement' e')
renameExp history (PrePlus' e)
    = let (history', e') = renameExp history e in (history', PrePlus' e')
renameExp history (PreMinus' e)
    = let (history', e') = renameExp history e in (history', PreMinus' e')
renameExp history (PreBitCompl' e)
    = let (history', e') = renameExp history e in (history', PreBitCompl' e')
renameExp history (PreNot' e)
    = let (history', e') = renameExp history e in (history', PreNot' e')
renameExp history (BinOp' e1 op e2)
    = let (history', e1')  = renameExp history e1
          (history'', e2') = renameExp history' e2
       in (history'', BinOp' e1' op e2')
renameExp history (Cond' g e1 e2)
    = let (history1, g')  = renameExp history g
          (history2, e1') = renameExp history1 e1
          (history3, e2') = renameExp history2 e2
       in (history3, Cond' g' e1' e2')
renameExp history (Assign' t op e)
    = let (history', e') = renameExp history e
       in (history', Assign' t op e')