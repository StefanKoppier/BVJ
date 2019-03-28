module Compilation.Compiler.Method where

import Data.Maybe
import Data.Accumulator
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Phase
import Linearization.Path
import Compilation.Compiler.Statement
import Compilation.CompiledUnit

import Debug.Trace

buildMethod :: CompilationUnit' -> ProgramPath -> PhaseResult Decl'
buildMethod unit path = do
    let name = snd . snd . head $ path
    case method of
        MethodDecl' modifiers ty _ params _
            -> do
                let body = snd $ runAccumulator (buildMethodBody path) []
                return (MemberDecl' (MethodDecl' modifiers ty name params body))
        ConstructorDecl' modifiers _ params _
            -> do
                let classTy = ClassType' [className (head path)]
                let ty = RefType' (ClassRefType' classTy)
                let body = snd $ runAccumulator (buildConstructorBody unit classTy path) []
                return (MemberDecl' (MethodDecl' (modifiers ++ [Static']) (Just ty) name params body))
    where
        scope  = (fst . snd . head) path
        method = fromJust $ getMethod unit scope

buildMethodBody :: ProgramPath -> MethodAccumulator CompoundStmts'
buildMethodBody path = 
    let path' = filter hasMethodEntryType path
     in mapM buildMethodStmt path'

buildConstructorBody :: CompilationUnit' -> ClassType' -> ProgramPath -> MethodAccumulator CompoundStmts'
buildConstructorBody unit ty path = do
    let path' = filter hasMethodEntryType path
    let objCreation = Stmt' $ Decl' [] (RefType' (ClassRefType' ty)) [VarDecl' (VarId' "_thisObj__") (InitExp' (InstanceCreation' ty []))]
    originalBody <- mapM buildConstructorStmt path'
    let body = objCreation : originalBody ++ [objReturn]
    return body
    where
        objReturn = Stmt' (Return' (Just (ExpName' ["_thisObj__"])))

className :: PathStmt -> String
className (_,(origin, _))
    = c
    where
        (Scope _ c _) = origin
