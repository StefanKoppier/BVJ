module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive  as G
import qualified Data.Map              as M
import           Data.Stack            as S
import           Data.Maybe
import           Data.List
import           Analysis.CFG
import           Analysis.Pretty
import           Parsing.Syntax
import           Linearization.Path
import           Auxiliary.Phase
import           Auxiliary.Pretty

linearizationPhase :: Phase (CompilationUnit', CFG) ProgramPaths
linearizationPhase Arguments{maximumDepth,method} (_, graph@CFG{cfg}) = do
    newEitherT $ printHeader "3. LINEARIZATION"
    newEitherT $ printPretty graph
    case entryOfMethod method graph of
        Just (entry,_) -> do
            let history   = M.fromList [(n, (0,0)) | (_,Entry n) <- G.labNodes cfg]
            let callStack = S.singleton (method, -1)
            let acc       = (history, callStack, [[]])
            let ps        = paths acc graph (G.context cfg entry) maximumDepth
            return . map (reverse . clean) $ ps
        Nothing        -> 
            left $ MethodNotFound method

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- Call history, the first value represents the scope renaming, the second
-- one represents the call renaming.
type CallHistory = M.Map Name' (Int, Int)

type CallStack = Stack (Name', G.Node)

type Accumulator = (CallHistory, CallStack, ProgramPaths)

paths :: Accumulator -> CFG -> CFGContext -> Int -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths _ _ (_, _, _, _:_) 0
    = []

-- Case: the final statement.
paths (_,_,ps)  _ (_,_,Exit _, []) _
    = ps

-- Case: the entry of an method.
paths acc graph@CFG{cfg} (_,_,Entry _,[(_,neighbour)]) n
    = paths acc graph (G.context cfg neighbour) n
    
-- Case: the exit of an method.
paths (history, callStack, ps) graph@CFG{cfg} (_,_,Exit _,_) n
    = let (_,destination) = fromJust $ S.peek callStack
          acc'            = (history, S.pop callStack, ps)
       in paths acc' graph (G.context cfg destination) n

-- Case: the call of an method.
paths (history, callStack, ps) graph@CFG{cfg} (_,node,Call method,[(_,neighbour)]) n
    = let (callNumber, x)      = history M.! method
          history'             = M.insert method (callNumber + 1, x) history
          newName              = [head method ++ "$" ++ show callNumber]
          callStack'           = S.push (newName, node + 1) callStack
          acc'                 = (history', callStack', ps)
       in paths acc' graph (G.context cfg neighbour) n

-- Case: a statement.
paths acc graph (_,_,Block s,neighbours) n
    = concatMap (next acc s graph n) neighbours

next :: Accumulator -> CompoundStmt' -> CFG -> Int -> (CFGEdgeValue, G.Node) -> ProgramPaths
next (history, callStack, ps) s graph@CFG{cfg} n (edge, neighbour) 
    = let acc' = (history', callStack, map ((scope,stat):) ps)
       in paths acc' graph (G.context cfg neighbour) (n-1) 
    where
        (scope, _)                      = fromJust $ S.peek callStack
        (history',stat) 
            | ConditionalEdge e <- edge = renameStmt history (Assume' e)
            | (Stmt' s')        <- s    = renameStmt history s'

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
renameStmt history (ReturnExp' e) = 
    let (history', e') = renameExp history e
     in (history', ReturnExp' e')
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