module Compilation.Compiler.Statement where

import Parsing.Syntax
import Auxiliary.Phase
import Linearization.Path
import Data.Accumulator
import Compilation.Compiler.Expression
import Compilation.CompiledUnit

--------------------------------------------------------------------------------
-- Method block building
--------------------------------------------------------------------------------

buildStmts :: (Stmt' -> MethodAccumulator CompoundStmt') -> [PathType] -> MethodAccumulator CompoundStmts'
buildStmts _ [] 
    = return []

buildStmts statBuilder (PathStmt Empty':stats) 
    = buildStmts statBuilder stats

buildStmts statBuilder (PathStmt stat:stats) = do
    stat'  <- statBuilder stat
    stats' <- buildStmts statBuilder stats
    return (stat' : stats')

buildStmts statBuilder (entry@(PathEntry (ConditionalEntryType e)):stats) = do
    let index = findIndex 0 0 entry stats
    let (blockBody, restStats) = splitAt index stats
    block <- keepOldAccumulator (Block' Nothing <$> buildStmts statBuilder blockBody)
    restStats' <- buildStmts statBuilder restStats
    return (block : restStats')
    
buildStmts statBuilder (entry@(PathEntry (BlockEntryType label)):stats) = do
    let index                  = findIndex 0 0 entry stats
    let (blockBody, restStats) = splitAt index stats
    block <- keepOldAccumulator (Block' label <$> buildStmts statBuilder blockBody)
    restStats' <- buildStmts statBuilder restStats
    return (block : restStats')

buildStmts statBuilder stats@(entry@(PathEntry TryEntryType):_) = do
    (try, restStats) <- keepOldAccumulator (buildTryCatchStmts statBuilder stats)
    restStats'       <- buildStmts statBuilder restStats
    return (try : restStats')

buildStmts statBuilder (PathExit _:stats) 
    = buildStmts statBuilder stats

buildTryCatchStmts :: (Stmt' -> MethodAccumulator CompoundStmt') -> [PathType] -> MethodAccumulator (CompoundStmt', [PathType])
buildTryCatchStmts statBuilder (entry@(PathEntry TryEntryType):stats) = do
    let index                = findIndex 0 0 entry stats
    let (tryBody, restStats) = splitAt index stats
    try              <- keepOldAccumulator (buildStmts statBuilder tryBody)
    (catches, rest1) <- keepOldAccumulator (buildCatches statBuilder (drop 1 restStats))
    (finally, rest2) <- keepOldAccumulator (buildFinally statBuilder (drop 1 rest1))
    return (Try' try catches finally, rest2)

buildCatches :: (Stmt' -> MethodAccumulator CompoundStmt') -> [PathType] -> MethodAccumulator (Catches', [PathType])
buildCatches statBuilder (ty@(PathEntry (CatchEntryType (Just e))):stats) = do
    let index                  = findIndex 0 0 ty stats
    let (catchBody, restStats) = splitAt index stats
    catchStats <- keepOldAccumulator (buildStmts statBuilder catchBody)
    let catch = Catch' e catchStats
    (catches, restStats2) <- keepOldAccumulator (buildCatches statBuilder restStats)
    return (catch:catches, restStats2)

buildCatches _ stats = return ([], stats)

buildFinally :: (Stmt' -> MethodAccumulator CompoundStmt') -> [PathType] -> MethodAccumulator (MaybeCompoundStmts', [PathType])
buildFinally statBuilder (ty@(PathEntry FinallyEntryType):stats) = do
    let index                    = findIndex  0 0 ty stats
    let (finallyBody, restStats) = splitAt index stats
    finallyStats <- keepOldAccumulator (buildStmts statBuilder finallyBody)
    let finally = Just finallyStats
    return (finally, restStats)

buildFinally _ stats = return (Nothing, stats)

findIndex :: Int -> Int -> PathType -> [PathType] -> Int
findIndex _ _ _ [] = error "no according exit block found"

findIndex x i (PathEntry ty) (PathStmt _:rest)
    = findIndex x (i+1) (PathEntry ty) rest

findIndex x i (PathEntry ty) (PathEntry entry:rest)
    | ty == entry = findIndex (x+1) (i+1) (PathEntry ty) rest 
    | otherwise   = findIndex x (i+1) (PathEntry ty) rest

findIndex x i (PathEntry ty) (PathExit exit:rest)
    | ty == exit && x == 0 = i 
    | ty == exit           = findIndex (x-1) (i+1) (PathEntry ty) rest
    | otherwise            = findIndex x (i+1) (PathEntry ty) rest
 
--------------------------------------------------------------------------------
-- Method statement building
--------------------------------------------------------------------------------

buildMethodStmt :: CompilationUnit' -> Stmt' -> MethodAccumulator CompoundStmt'
buildMethodStmt unit (Decl' modifiers ty vars)
    = Stmt' . Decl' modifiers ty <$> mapM (buildMethodDecl unit) vars
buildMethodStmt _ Empty'
    = return $ Stmt' Empty'
buildMethodStmt unit (ExpStmt' exp) = do
    locals <- getAccumulator
    return $ Stmt' (ExpStmt' (buildMethodExp unit locals exp))
buildMethodStmt unit (Assert' exp message) = do
    locals <- getAccumulator
    return $ Stmt' (Assert' (buildMethodExp unit locals exp) message)
buildMethodStmt unit (Assume' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Assume' (buildMethodExp unit locals exp))
buildMethodStmt unit (Return' (Just exp)) = do
    locals <- getAccumulator
    return $ Stmt' (Return' (Just (buildMethodExp unit locals exp)))
buildMethodStmt _ (Return' Nothing) =
    return $ Stmt' (Return' Nothing)
buildMethodStmt unit (Throw' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Throw' (buildMethodExp unit locals exp))
    
buildMethodDecl :: CompilationUnit' -> VarDecl' -> MethodAccumulator VarDecl'
buildMethodDecl unit (VarDecl' id init) 
    = VarDecl' id <$> buildMethodVarInit unit init

buildMethodVarInit :: CompilationUnit' -> VarInit' -> MethodAccumulator VarInit'
buildMethodVarInit unit (InitExp' exp) = do
    locals <- getAccumulator
    return $ InitExp' (buildMethodExp unit locals exp)
    
buildVarInit _ (InitArray' Nothing) 
    = return $ InitArray' Nothing
    
buildVarInit unit (InitArray' (Just inits)) 
    = InitArray' . Just <$> mapM (buildMethodVarInit unit) inits

--------------------------------------------------------------------------------
-- Constructor statement building
--------------------------------------------------------------------------------
 
buildConstructorStmt :: CompilationUnit' -> Stmt' -> MethodAccumulator CompoundStmt'
buildConstructorStmt unit (Decl' modifiers ty vars) = do
    vars' <- mapM (buildConstructorDecl unit) vars
    return $ Stmt' (Decl' modifiers ty vars')
buildConstructorStmt _ Empty'
    = return (Stmt' Empty')
buildConstructorStmt unit (ExpStmt' exp) = do
    locals <- getAccumulator
    return $ Stmt' (ExpStmt' (buildConstructorExp unit locals exp))
buildConstructorStmt unit (Assert' exp message) = do
    locals <- getAccumulator
    return $ Stmt' (Assert' (buildConstructorExp unit locals exp) message)
buildConstructorStmt unit (Assume' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Assume' (buildConstructorExp unit locals exp))
buildConstructorStmt _ (Return' _)
    = return $ Stmt' (Return' (Just (ExpName' ["_thisObj__"])))
buildConstructorStmt unit (Throw' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Throw' (buildConstructorExp unit locals exp))
    
buildConstructorDecl :: CompilationUnit' -> VarDecl' -> MethodAccumulator VarDecl'
buildConstructorDecl unit (VarDecl' id init) = do
    updateAccumulator (id:)
    init' <- buildConstructorVarInit unit init
    return (VarDecl' id init')

buildConstructorVarInit :: CompilationUnit' -> VarInit' -> MethodAccumulator VarInit'
buildConstructorVarInit unit (InitExp' exp) = do
    locals <- getAccumulator
    return $ InitExp' (buildConstructorExp unit locals exp)

buildConstructorVarInit _ (InitArray' Nothing) 
    = pure $ InitArray' Nothing
    
buildConstructorVarInit unit (InitArray' (Just inits)) 
    = InitArray' . Just <$> mapM (buildConstructorVarInit unit) inits