{-|
Module      : Compilation.Compiler.Method
Description : Module containing the AST generation of a method.
-}
module Compilation.Compiler.Method where

import Data.Maybe
import Data.Accumulator
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Phase
import Compilation.Compiler.Statement
import Compilation.CompiledUnit

-- | Generates the method from the given program path.
buildMethod :: CompilationUnit' -> ProgramPath -> PhaseResult Decl'
buildMethod unit path = do
    let name = snd . snd . head $ path
    case method of
        MethodDecl' modifiers ty _ params _
            -> do
                let vars = [d | (FormalParam' _ _ d) <- params]
                let body = snd $ runAccumulator (buildMethodBody unit path) vars
                return (MemberDecl' (MethodDecl' modifiers ty name params body))
        ConstructorDecl' modifiers _ params _
            -> do
                let classTy = ClassType' [className (head path)]
                let ty = RefType' (ClassRefType' classTy)
                let vars = [d | (FormalParam' _ _ d) <- params]
                let body = snd $ runAccumulator (buildConstructorBody unit classTy path) vars
                return (MemberDecl' (MethodDecl' (modifiers ++ [Static']) (Just ty) name params body))
    where
        scope  = (fst . snd . head) path
        method = fromJust $ getMethod unit scope

-- | Generates the method body from the given program path.
buildMethodBody :: CompilationUnit' -> ProgramPath -> MethodAccumulator CompoundStmts'
buildMethodBody unit path = do
    let path' = map fst path
    buildStmts (buildMethodStmt unit) path'

-- | Generates the constructor body from the given program path.
buildConstructorBody :: CompilationUnit' -> ClassType' -> ProgramPath -> MethodAccumulator CompoundStmts'
buildConstructorBody unit ty path = do
    let objCreation = Stmt' $ Decl' [] (RefType' (ClassRefType' ty)) [VarDecl' (VarId' "_thisObj__") (InitExp' (InstanceCreation' ty []))]
    let path' = map fst path
    originalBody <- buildStmts (buildConstructorStmt unit) path'
    let body = objCreation : originalBody ++ [objReturn]
    return body
    where
        objReturn = Stmt' (Return' (Just (ExpName' ["_thisObj__"])))

-- | Returns the class in which the path statement is defined in.
className :: PathStmt -> String
className (_, (Scope _ name _, _))
    = name