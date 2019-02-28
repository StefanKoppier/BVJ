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

type StmtManipulations = M.Map G.Node RenamingOperations

type RenamingOperations = M.Map Name' (Scope, Int)

insertManipulation :: G.Node -> Name' -> (Scope, Int) -> StmtManipulations -> StmtManipulations
insertManipulation node name value manipulations
    | Just oldValue' <- oldValue
        = M.insert node (M.insert name value oldValue') manipulations
    | Nothing <- oldValue
        = M.insert node (M.singleton name value) manipulations
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
renameStmt ops (Assert' e mssg)
    = let (ops1, e') = renameExp ops e 
       in (ops1, Assert' e' mssg)
renameStmt ops (Assume' e) 
    = let (ops1, e') = renameExp ops e
       in (ops1, Assume' e')
renameStmt ops (ReturnExp' e) = 
    let (ops1, e') = renameExp ops e
     in (ops1, ReturnExp' e')
renameStmt ops (ExpStmt' e) 
    = let (ops1, e') = renameExp ops e
       in (ops1, ExpStmt' e')

renameVarDecl :: RenamingOperations -> VarDecl' -> (RenamingOperations, VarDecl')
renameVarDecl ops (VarDecl' id init) 
    = let (ops1, init') = renameVarInit ops init
       in (ops1, VarDecl' id init')

renameVarInit :: RenamingOperations -> VarInit' -> (RenamingOperations, VarInit')
renameVarInit ops (InitExp' e)
    = let (ops1, e') = renameExp ops e
       in (ops1, InitExp' e')
renameVarInit ops (InitArray' Nothing)
    = (ops, InitArray' Nothing)
renameVarInit ops (InitArray' (Just is))
    = let (ops1, is') = mapAccumL renameVarInit ops is
       in (ops1, InitArray' (Just is'))

renameExp :: RenamingOperations -> Exp' -> (RenamingOperations, Exp')
renameExp ops e@(Lit' _) = (ops, e)
renameExp ops This' = (ops, This')
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
          (ops2, exp2') = renameExp ops exp2
       in (ops2, BinOp' exp1' op exp2')
renameExp ops (Cond' guard exp1 exp2)
       = let (ops1, guard') = renameExp ops guard
             (ops2, exp1') = renameExp ops exp1
             (ops3, exp2') = renameExp ops exp2
          in (ops3, Cond' guard' exp1' exp2')
renameExp ops (Assign' lhs op exp)
    = let (ops1, lhs') = renameLhs ops lhs
          (ops2, exp') = renameExp ops1 exp
       in (ops2, Assign' lhs' op exp')

renameExp ops (InstanceCreation' (ClassType' name) args)
    = let (scope, callNumber) = ops M.! name
          ops1                = M.delete name ops
          name'               = renameMethodCall name scope callNumber
          (ops2, args')       = mapAccumL renameExp ops1 args
       in (ops2, InstanceCreation' (ClassType' name') args')

renameExp ops (MethodInv' (MethodCall' name args))
    = let (scope, callNumber) = ops M.! name
          ops1                = M.delete name ops
          name'               = renameMethodCall name scope callNumber
          (ops2, args')       = mapAccumL renameExp ops1 args
       in (ops2, MethodInv' (MethodCall' name' args'))

renameLhs :: RenamingOperations -> Lhs' -> (RenamingOperations, Lhs')
renameLhs ops name@(Name' _) = (ops, name)
renameLhs ops (Field' access) 
    = let (ops1, access') = renameFieldAccess ops access
       in (ops1, Field' access')

renameFieldAccess :: RenamingOperations -> FieldAccess' -> (RenamingOperations, FieldAccess')
renameFieldAccess ops (PrimaryFieldAccess' exp field)
    = let (ops1, exp') = renameExp ops exp
       in (ops1, PrimaryFieldAccess' exp' field)
renameFieldAccess ops f@(ClassFieldAccess' _ _)
    = (ops, f)

{-
-- TODO: the package and class name should be included in the method call to
-- avoid duplicate method name generation.

renameMethodCall :: Scope -> Name' -> Int -> Name'
renameMethodCall _ name numberOfCallsMade
    = changeLast newName name
    where 
        newName  = last name ++ "$" ++ show numberOfCallsMade
    
renameMethodName :: Scope -> Int -> String
renameMethodName (Scope package className methodName) numberOfCallsMade
    = methodName ++ "$" ++ show numberOfCallsMade
        
changeLast :: a -> [a] -> [a]
changeLast x [_]    = [x]
changeLast x (v:xs) = v : changeLast x xs 
-}