module Linearization.Renaming(
      StmtManipulations
    , RenamingOperations
    , renameStmt
    , renameMethodName
    , insertManipulation
) where

import qualified Data.Map                   as M
import qualified Data.Graph.Inductive.Graph as G
import           Data.List
import           Parsing.Syntax
import           Analysis.Pretty                 ()
import           Auxiliary.Pretty 

import Debug.Trace

type StmtManipulations = M.Map G.Node RenamingOperations

type RenamingOperations = M.Map Name' [(Scope, Int)]

insertManipulation :: G.Node -> Name' -> (Scope, Int) -> StmtManipulations -> StmtManipulations
insertManipulation node name value manipulations
    | Just oldValue' <- oldValue
         = M.insert node (M.insertWith (flip (++)) name [value] oldValue') manipulations
    | Nothing <- oldValue
        = M.insert node (M.singleton name [value]) manipulations
    where
        oldValue  = manipulations M.!? node

-- TODO: add package name to newCallName.
renameMethodName :: Scope -> Int -> String
renameMethodName (Scope scopePackage scopeClass scopeMember) callNumber
    = newCallName
    where
        newCallName = scopeClass ++ "_" ++ scopeMember ++ "$" ++ show callNumber

-- TODO: add package name to newCallName.
renameMethodCall :: Name' -> Scope -> Int -> Name'
renameMethodCall name s@(Scope scopePackage scopeClass scopeMember) callNumber
    = changeLast newCallName name
    where
        newCallName = scopeClass ++ "_" ++ scopeMember ++ "$" ++ show callNumber

changeLast :: a -> [a] -> [a]
changeLast x [_]    = [x]
changeLast x (v:xs) = v : changeLast x xs 

renameStmt :: RenamingOperations -> Stmt' -> (RenamingOperations, Stmt')
renameStmt ops (Decl' ms ty vars)
    = let (ops1, vars') = mapAccumL renameVarDecl ops vars
       in (ops1, Decl' ms ty vars')
renameStmt ops Empty' 
    = (ops, Empty')
renameStmt ops (ExpStmt' e) 
    = let (ops1, e') = renameExp ops e
       in (ops1, ExpStmt' e')
renameStmt ops (Assert' e mssg)
    = let (ops1, e') = renameExp ops e 
       in (ops1, Assert' e' mssg)
renameStmt ops (Assume' e) 
    = let (ops1, e') = renameExp ops e
       in (ops1, Assume' e')
renameStmt ops s@(Break' _)
    = (ops, s)
renameStmt ops s@(Continue' _)
    = (ops, s)
renameStmt ops (Return' e) = 
    let (ops1, e') = renameMaybeExp ops e
     in (ops1, Return' e')

renameVarDecl :: RenamingOperations -> VarDecl' -> (RenamingOperations, VarDecl')
renameVarDecl ops (VarDecl' id init) 
    = let (ops1, init') = renameVarInit ops init
       in (ops1, VarDecl' id init')

renameVarInit :: RenamingOperations -> VarInit' -> (RenamingOperations, VarInit')
renameVarInit ops (InitExp' e)
    = let (ops1, e') = renameExp ops e
       in (ops1, InitExp' e')
renameVarInit ops (InitArray' inits)
    = let (ops1, inits') = renameMaybeVarInits ops inits
       in (ops1, InitArray' inits')

renameMaybeVarInits :: RenamingOperations -> MaybeVarInits' -> (RenamingOperations, MaybeVarInits')
renameMaybeVarInits ops (Just inits)
    = let (ops1, inits') = mapAccumL renameVarInit ops inits
       in (ops1, Just inits')
renameMaybeVarInits ops Nothing
    = (ops, Nothing)

renameMaybeExp :: RenamingOperations -> Maybe Exp' -> (RenamingOperations, Maybe Exp')
renameMaybeExp ops Nothing = (ops, Nothing)
renameMaybeExp ops (Just exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, Just exp')

renameExp :: RenamingOperations -> Exp' -> (RenamingOperations, Exp')
renameExp ops e@(Lit' _) = (ops, e)
renameExp ops This' = (ops, This')
renameExp ops (InstanceCreation' (ClassType' name) args)
    = let ((scope, callNumber):renames) = ops M.! name
          ops1                = M.insert name renames ops
          name'               = renameMethodCall name scope callNumber
          (ops2, args')       = mapAccumL renameExp ops1 args
       in (ops2, InstanceCreation' (ClassType' name') args')
renameExp ops (ArrayCreate' ty sizes unspecified)
    = let (ops1, sizes') = mapAccumL renameExp ops sizes
       in (ops1, ArrayCreate' ty sizes' unspecified)
renameExp ops (ArrayCreateInit' ty dimensions inits)
    = let (ops1, inits') = mapAccumL renameVarInit ops inits
       in (ops1, ArrayCreateInit' ty dimensions inits')
renameExp ops (FieldAccess' access)
    = let (ops1, access') = renameFieldAccess ops access
       in (ops1, FieldAccess' access')
renameExp ops (MethodInv' inv)
    = let (ops1, inv') = renameInvocation ops inv
       in (ops1, MethodInv' inv')
renameExp ops (ArrayAccess' name indices)
    = let (ops1, indices') = mapAccumL renameExp ops indices
       in (ops1, ArrayAccess' name indices')
renameExp ops e@(ExpName' _) = (ops, e)
renameExp ops (PostIncrement' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PostIncrement' exp')
renameExp ops (PostDecrement' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PostDecrement' exp')
renameExp ops (PreIncrement' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PreIncrement' exp')
renameExp ops (PreDecrement' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PreDecrement' exp')
renameExp ops (PrePlus' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PrePlus' exp')
renameExp ops (PreMinus' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PreMinus' exp')
renameExp ops (PreBitCompl' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PreBitCompl' exp')
renameExp ops (PreNot' exp)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PreNot' exp')
renameExp ops (BinOp' exp1 op exp2)
    = let (ops1, exp1') = renameExp ops exp1
          (ops2, exp2') = renameExp ops1 exp2
       in (ops2, BinOp' exp1' op exp2')
renameExp ops (Cond' guard exp1 exp2)
       = let (ops1, guard') = renameExp ops guard
             (ops2, exp1')  = renameExp ops1 exp1
             (ops3, exp2')  = renameExp ops2 exp2
          in (ops3, Cond' guard' exp1' exp2')
renameExp ops (Assign' lhs op exp)
    = let (ops1, lhs') = renameLhs ops lhs
          (ops2, exp') = renameExp ops1 exp
       in (ops2, Assign' lhs' op exp')

renameInvocation :: RenamingOperations -> MethodInvocation' -> (RenamingOperations, MethodInvocation')
renameInvocation ops (MethodCall' name args) 
    = let ((scope, callNumber):renames) = ops M.! name
          ops1                = M.insert name renames ops
          name'               = renameMethodCall name scope callNumber
          (ops2, args')       = mapAccumL renameExp ops1 args
       in (ops2, MethodCall' name' args')

renameInvocation ops (PrimaryMethodCall' exp name args)
    = let ((scope, callNumber):renames) = ops M.! [name]
          ops1                = M.insert [name] renames ops
          [name']             = renameMethodCall [name] scope callNumber
          (ops2, args')       = mapAccumL renameExp ops1 args
          (ops3, exp')        = renameExp ops2 exp
       in (ops3, PrimaryMethodCall' exp' name' args')

renameLhs :: RenamingOperations -> Lhs' -> (RenamingOperations, Lhs')
renameLhs ops name@(Name' _) = (ops, name)
renameLhs ops (Field' access) 
    = let (ops1, access') = renameFieldAccess ops access
       in (ops1, Field' access')
renameLhs ops (Array' (ArrayIndex' array indices))
    = let (ops1, array')   = renameExp ops array
          (ops2, indices') = mapAccumL renameExp ops1 indices
       in (ops2, Array' (ArrayIndex' array' indices'))

renameFieldAccess :: RenamingOperations -> FieldAccess' -> (RenamingOperations, FieldAccess')
renameFieldAccess ops (PrimaryFieldAccess' exp field)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PrimaryFieldAccess' exp' field)
renameFieldAccess ops f@(ClassFieldAccess' _ _)
    = (ops, f)