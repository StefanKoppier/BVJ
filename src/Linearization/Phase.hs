module Linearization.Phase(
    linearizationPhase
) where

import qualified Data.Graph.Inductive  as G
import qualified Data.Map              as M
import           Data.Stack            as S
import           Data.Maybe
import           Data.List
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
            let history   = M.fromList [(n, (0,0)) | (_,Entry n) <- G.labNodes cfg]
            let callStack = S.singleton (method, scopeMember method, -1)
            let acc       = (history, callStack, [[]], maximumDepth)
            let ps        = paths acc graph (G.context cfg entry)
            return . map reverse $ ps
        Nothing        -> semanticalError (UndefinedMethodReference method)

--------------------------------------------------------------------------------
-- Program path generation
--------------------------------------------------------------------------------

-- | The history of number of calls renamed, and number of statements renamed.
type CallHistory = M.Map Scope (Int, Int)

-- | A stack frame containing the current method, the current call name, and 
-- the node the method should return to.
type StackFrame = (Scope, String, G.Node)

-- | The stack of method calls.
type CallStack = Stack StackFrame

type Accumulator = (CallHistory, CallStack, ProgramPaths, Int)

paths :: Accumulator -> CFG -> CFGContext -> ProgramPaths
-- Case: the end is not reached and the maximum path length is reached.
paths (_,_,_,0) _ (_, _, _, _:_)
    = []

-- Case: the final statement.
paths (_,_,ps,_)  _ (_,_,Exit _, [])
    = ps

-- Case: the entry of an method.
paths acc graph@CFG{cfg} (_,_,Entry _,[(_,neighbour)])
    = paths acc graph (G.context cfg neighbour)
    
-- Case: the exit of an method.
paths (history, callStack, ps, k) graph@CFG{cfg} (_,_,Exit _,_)
    = let (_, _, destination) = fromJust $ S.peek callStack
          acc'                = (history, S.pop callStack, ps, k)
       in paths acc' graph (G.context cfg destination)

-- Case: the call of an method.
paths (history, callStack, ps, k) graph@CFG{cfg} (_,node, Call method, [(_,neighbour)])
    = let (callNumber, x)      = history M.! method
          history'             = M.insert method (callNumber + 1, x) history
          newName              = renameMethodCall method callNumber
          callStack'           = S.push (method, newName, node + 1) callStack
          acc'                 = (history', callStack', ps, k)
       in paths acc' graph (G.context cfg neighbour)

-- Case: a statement.
paths acc graph (_,_,Block s,neighbours)
    = concatMap (next acc s graph) neighbours

next :: Accumulator -> CompoundStmt' -> CFG -> (CFGEdgeValue, G.Node) -> ProgramPaths
next (history, callStack, ps, k) s graph@CFG{cfg} (edge, neighbour) 
    = let pathInfo = PathStmtInfo{callName=callName', original=scope'}
          acc' = (history', callStack, map ((stat, pathInfo):) ps, k-1)
       in paths acc' graph (G.context cfg neighbour)
    where
        (scope', callName', _)          = fromJust $ S.peek callStack
        (history', stat) 
            | ConditionalEdge e <- edge = renameStmt scope' history (Assume' e)
            | (Stmt' s')        <- s    = renameStmt scope' history s'

--------------------------------------------------------------------------------
-- Renaming of method calls
--------------------------------------------------------------------------------

renameStmt :: Scope -> CallHistory -> Stmt' -> (CallHistory, Stmt')
renameStmt scope history (Decl' ms ty vars) =
    let (history', vars') = mapAccumR (renameVarDecl scope) history vars
     in (history', Decl' ms ty vars')
renameStmt scope history (Assert' e mssg) = 
    let (history', e') = renameExp scope history e
     in (history', Assert' e' mssg)
renameStmt scope history (Assume' e) = 
    let (history', e') = renameExp scope history e
     in (history', Assume' e')
renameStmt scope history (ReturnExp' e) = 
    let (history', e') = renameExp scope history e
     in (history', ReturnExp' e')
renameStmt scope history (ExpStmt' e) =
    let (history', e') = renameExp scope history e
     in (history', ExpStmt' e')
renameStmt _ history s = (history, s)

renameVarDecl :: Scope -> CallHistory -> VarDecl' -> (CallHistory, VarDecl')
renameVarDecl scope history (VarDecl' id init) 
    = let (history', init') = renameVarInit scope history init
       in (history', VarDecl' id init')

renameVarInit :: Scope -> CallHistory -> VarInit' -> (CallHistory, VarInit')
renameVarInit scope history (InitExp' e)
    = let (history', e') = renameExp scope history e
       in (history', InitExp' e')
renameVarInit _ history (InitArray' Nothing)
    = (history, InitArray' Nothing)
renameVarInit scope history (InitArray' (Just is))
    = let (history', is') = mapAccumR (renameVarInit scope) history is
       in (history', InitArray' (Just is'))

renameMaybeExp :: Scope -> CallHistory -> Maybe Exp' -> (CallHistory, Maybe Exp')
renameMaybeExp _ history Nothing = (history, Nothing)
renameMaybeExp scope history (Just e) 
    = let (history', e') = renameExp scope history e in (history', Just e')

renameExp :: Scope -> CallHistory -> Exp' -> (CallHistory, Exp')
renameExp _ history (Lit' x)
    = (history, Lit' x)
renameExp scope history (InstanceCreation' ty args)
    = let (history', args') = mapAccumR (renameExp scope) history args
          (history'', ty')  = renameClassType history' ty
       in (history'', InstanceCreation' ty' args')
renameExp scope history (ArrayCreate' ty ss u)
    = let (history', ss') = mapAccumR (renameExp scope) history ss
       in (history', ArrayCreate' ty ss' u)
renameExp scope history (MethodInv' (MethodCall' n args)) 
    = let (history', args') = mapAccumR (renameExp scope) history args
          (x, callNumber)   = history M.! scope
          newName           = renameMethodCall scope callNumber
          history''         = M.insert scope (x, callNumber + 1) history'
        -- TODO: check if changeLast newName n is correct.
       in (history'', MethodInv' (MethodCall' (changeLast newName n) args'))
renameExp scope history (ArrayAccess' i is)
    = let (history', is') = mapAccumR (renameExp scope) history is
       in (history', ArrayAccess' i is')
renameExp _ history (ExpName' n)
    = (history, ExpName' n)
renameExp scope history (PostIncrement' e)
    = let (history', e') = renameExp scope history e in (history', PostIncrement' e')
renameExp scope history (PostDecrement' e)
    = let (history', e') = renameExp scope history e in (history', PostDecrement' e')
renameExp scope history (PreIncrement' e)
    = let (history', e') = renameExp scope history e in (history', PreIncrement' e')
renameExp scope history (PreDecrement' e)
    = let (history', e') = renameExp scope history e in (history', PreDecrement' e')
renameExp scope history (PrePlus' e)
    = let (history', e') = renameExp scope history e in (history', PrePlus' e')
renameExp scope history (PreMinus' e)
    = let (history', e') = renameExp scope history e in (history', PreMinus' e')
renameExp scope history (PreBitCompl' e)
    = let (history', e') = renameExp scope history e in (history', PreBitCompl' e')
renameExp scope history (PreNot' e)
    = let (history', e') = renameExp scope history e in (history', PreNot' e')
renameExp scope history (BinOp' e1 op e2)
    = let (history', e1')  = renameExp scope history e1
          (history'', e2') = renameExp scope history' e2
       in (history'', BinOp' e1' op e2')
renameExp scope history (Cond' g e1 e2)
    = let (history1, g')  = renameExp scope history g
          (history2, e1') = renameExp scope history1 e1
          (history3, e2') = renameExp scope history2 e2
       in (history3, Cond' g' e1' e2')
renameExp scope history (Assign' t op e)
    = let (history', e') = renameExp scope history e
       in (history', Assign' t op e')

changeLast :: a -> [a] -> [a]
changeLast x [_]    = [x]
changeLast x (_:xs) = changeLast x xs 

renameClassType :: CallHistory -> ClassType' -> (CallHistory, ClassType')
renameClassType history (ClassType' [name])
    = let constructor     = Scope Nothing name name
          (x, callNumber) = history M.! constructor
          newName         = renameMethodCall constructor callNumber
          history'        = M.insert constructor (x, callNumber + 1) history
       in (history', ClassType' [newName])