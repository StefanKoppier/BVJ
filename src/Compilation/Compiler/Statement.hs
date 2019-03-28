module Compilation.Compiler.Statement where

import Parsing.Syntax
import Auxiliary.Phase
import Linearization.Path
import Data.Accumulator
import Compilation.Compiler.Expression
import Compilation.CompiledUnit

import Debug.Trace

--------------------------------------------------------------------------------
-- Method statement building
--------------------------------------------------------------------------------

buildMethodStmt :: PathStmt -> MethodAccumulator CompoundStmt'
buildMethodStmt (PathStmt s, _) = buildMethodStmt' s
buildMethodStmt x               = trace (show x) undefined

buildMethodStmt' (Decl' modifiers ty vars)
    = Stmt' . Decl' modifiers ty <$> mapM buildMethodDecl vars
buildMethodStmt' Empty'
    = return $ Stmt' Empty'
buildMethodStmt' (ExpStmt' exp) = do
    locals <- getAccumulator
    return $ Stmt' (ExpStmt' (buildMethodExp locals exp))
buildMethodStmt' (Assert' exp message) = do
    locals <- getAccumulator
    return $ Stmt' (Assert' (buildMethodExp locals exp) message)
buildMethodStmt' (Assume' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Assume' (buildMethodExp locals exp))
buildMethodStmt' (Return' (Just exp)) = do
    locals <- getAccumulator
    return $ Stmt' (Return' (Just (buildMethodExp locals exp)))
buildMethodStmt' (Return' Nothing) =
    return $ Stmt' (Return' Nothing)
buildMethodStmt' (Throw' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Throw' (buildMethodExp locals exp))
    
buildMethodDecl :: VarDecl' -> MethodAccumulator VarDecl'
buildMethodDecl (VarDecl' id init) 
    = VarDecl' id <$> buildMethodVarInit init

buildMethodVarInit :: VarInit' -> MethodAccumulator VarInit'
buildMethodVarInit (InitExp' exp) = do
    locals <- getAccumulator
    return $ InitExp' (buildMethodExp locals exp)
    
buildVarInit (InitArray' Nothing) 
    = return $ InitArray' Nothing
    
buildVarInit (InitArray' (Just inits)) 
    = InitArray' . Just <$> mapM buildMethodVarInit inits

--------------------------------------------------------------------------------
-- Constructor statement building
--------------------------------------------------------------------------------
    
buildConstructorStmt :: PathStmt -> MethodAccumulator CompoundStmt'
buildConstructorStmt (PathStmt s, _) = buildConstructorStmt' s
buildConstructorStmt x               = trace (show x) undefined

buildConstructorStmt' :: Stmt' -> MethodAccumulator CompoundStmt'
buildConstructorStmt' (Decl' modifiers ty vars) = do
    vars' <- mapM buildConstructorDecl vars
    return $ Stmt' (Decl' modifiers ty vars')
buildConstructorStmt' Empty'
    = return (Stmt' Empty')
buildConstructorStmt' (ExpStmt' exp) = do
    locals <- getAccumulator
    return $ Stmt' (ExpStmt' (buildConstructorExp locals exp))
buildConstructorStmt' (Assert' exp message) = do
    locals <- getAccumulator
    return $ Stmt' (Assert' (buildConstructorExp locals exp) message)
buildConstructorStmt' (Assume' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Assume' (buildConstructorExp locals exp))
buildConstructorStmt' (Return' _)
    = return $ Stmt' (Return' (Just (ExpName' ["_thisObj__"])))
buildConstructorStmt' (Throw' exp) = do
    locals <- getAccumulator
    return $ Stmt' (Throw' (buildConstructorExp locals exp))
    
buildConstructorDecl :: VarDecl' -> MethodAccumulator VarDecl'
buildConstructorDecl (VarDecl' id init) = do
    updateAccumulator (id:)
    init' <- buildConstructorVarInit init
    return (VarDecl' id init')

buildConstructorVarInit :: VarInit' -> MethodAccumulator VarInit'
buildConstructorVarInit (InitExp' exp) = do
    locals <- getAccumulator
    return $ InitExp' (buildConstructorExp locals exp)

buildConstructorVarInit (InitArray' Nothing) 
    = pure $ InitArray' Nothing
    
buildConstructorVarInit (InitArray' (Just inits)) 
    = InitArray' . Just <$> mapM buildConstructorVarInit inits