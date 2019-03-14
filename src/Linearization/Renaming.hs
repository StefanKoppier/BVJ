module Linearization.Renaming(
      StmtManipulations
    , RenamingOperations
    , renameStmt
    , renameMethodName
    , insertManipulation
) where

import qualified Data.Map                   as M
import qualified Data.Graph.Inductive.Graph as G
import           Data.Accumulator
import           Parsing.Syntax
import           Analysis.Pretty                 ()
import           Auxiliary.Pretty 

import Debug.Trace

type StmtManipulations = M.Map G.Node RenamingOperations

type RenamingOperations = M.Map Name' [(Scope, Int)]

type RenamingAcc a = Accumulator RenamingOperations a

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

rename :: Name' -> RenamingAcc Name'
rename name = do
    acc <- getAccumulator
    let ((scope, callNumber):renames) = acc M.! name
    let acc' = M.insert name renames acc
    updateAccumulator (const acc') 
    return $ renameMethodCall name scope callNumber

renameStmt :: Stmt' -> RenamingAcc Stmt'
renameStmt (Decl' ms ty vars) = do
    vars' <- mapM renameVarDecl vars
    return $ Decl' ms ty vars'

renameStmt (ExpStmt' exp) = do
    exp' <- renameExp exp
    return $ ExpStmt' exp'

renameStmt (Assert' exp message) = do
    exp' <- renameExp exp
    return $ Assert' exp' message

renameStmt (Assume' exp) = do
    exp' <- renameExp exp
    return $ Assume' exp'

renameStmt (Return' exp) = do
    exp' <- renameMaybeExp exp
    return $ Return' exp'
    
renameStmt stat = return stat

renameVarDecl :: VarDecl' -> RenamingAcc VarDecl'
renameVarDecl (VarDecl' id init) = do
    init' <- renameVarInit init
    return $ VarDecl' id init'

renameMaybeExp :: Maybe Exp' -> RenamingAcc (Maybe Exp')
renameMaybeExp Nothing    = return Nothing
renameMaybeExp (Just exp) = return <$> renameExp exp 

renameExp :: Exp' -> RenamingAcc Exp'
renameExp (InstanceCreation' (ClassType' name) args) = do
    newName <- rename name
    args'   <- mapM renameExp args
    return $ InstanceCreation' (ClassType' newName) args'

renameExp (ArrayCreate' ty sizes unspecified) = do
    sizes' <- mapM renameExp sizes
    return $ ArrayCreate' ty sizes unspecified

renameExp (ArrayCreateInit' ty dimensions inits) = do
    inits' <- mapM renameVarInit inits
    return $ ArrayCreateInit' ty dimensions inits'

renameExp (FieldAccess' access) = do
    access' <- renameFieldAccess access
    return $ FieldAccess' access'

renameExp (MethodInv' invocation) = do
    invocation' <- renameInvocation invocation
    return $ MethodInv' invocation'

renameExp (ArrayAccess' ident indices) = do
    indices' <- mapM renameExp indices
    return $ ArrayAccess' ident indices'

renameExp (PostIncrement' exp) = do
    exp' <- renameExp exp
    return $ PostIncrement' exp'
    
renameExp (PostDecrement' exp) = do
    exp' <- renameExp exp
    return $ PostDecrement' exp'
    
renameExp (PreIncrement' exp) = do
    exp' <- renameExp exp
    return $ PreIncrement' exp'
    
renameExp (PreDecrement' exp) = do
    exp' <- renameExp exp
    return $ PreDecrement' exp'
    
renameExp (PrePlus' exp) = do
    exp' <- renameExp exp
    return $ PrePlus' exp'
    
renameExp (PreMinus' exp) = do
    exp' <- renameExp exp
    return $ PreMinus' exp'
    
renameExp (PreBitCompl' exp) = do
    exp' <- renameExp exp
    return $ PreBitCompl' exp'
    
renameExp (PreNot' exp) = do
    exp' <- renameExp exp
    return $ PreNot' exp'

renameExp (BinOp' exp1 op exp2) = do
    exp1' <- renameExp exp1
    exp2' <- renameExp exp2
    return $ BinOp' exp1' op exp2'
    
renameExp (Cond' guard exp1 exp2) = do
    guard' <- renameExp guard
    exp1'  <- renameExp exp1
    exp2'  <- renameExp exp2
    return $ Cond' guard' exp1' exp2'

renameExp (Assign' target op exp) = do
    target' <- renameLhs target
    exp'    <- renameExp exp
    return $ Assign' target' op exp'

renameExp exp = return exp

renameInvocation :: MethodInvocation' -> RenamingAcc MethodInvocation'
renameInvocation (MethodCall' name args) = do
    newName <- rename name
    args'   <- mapM renameExp args
    return $ MethodCall' newName args'

renameInvocation (PrimaryMethodCall' exp name args) = do
    [newName] <- rename [name]
    exp'      <- renameExp exp
    args'     <- mapM renameExp args
    return $ PrimaryMethodCall' exp' newName args'

renameVarInit :: VarInit' -> RenamingAcc VarInit'
renameVarInit (InitExp' exp) = do
    exp' <- renameExp exp
    return $ InitExp' exp'

renameVarInit (InitArray' inits) = do
    inits' <- renameMaybeVarInits inits
    return $ InitArray' inits'

renameMaybeVarInits :: Maybe VarInits' -> RenamingAcc (Maybe VarInits')
renameMaybeVarInits Nothing      = return Nothing
renameMaybeVarInits (Just inits) = return <$> mapM renameVarInit inits

renameFieldAccess :: FieldAccess' -> RenamingAcc FieldAccess'
renameFieldAccess (PrimaryFieldAccess' exp field) = do
    exp' <- renameExp exp
    return $ PrimaryFieldAccess' exp' field

renameLhs :: Lhs' -> RenamingAcc Lhs'
renameLhs (Field' access) = do
    access' <- renameFieldAccess access
    return $ Field' access'

renameLhs (Array' (ArrayIndex' array indices)) = do
    array'   <- renameExp array
    indices' <- mapM renameExp indices
    return $ Array' (ArrayIndex' array' indices')

renameLhs lhs = return lhs
