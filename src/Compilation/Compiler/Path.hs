module Compilation.Compiler.Path where

import Data.Maybe  
import Data.List
import Data.Function
import Language.C.Syntax.AST
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Statement
import Compilation.Compiler.Expression
import Compilation.Compiler.Array
import Compilation.Compiler.Object
import Compilation.Compiler.Naming
import Compilation.Compiler.InformationGathering
import Linearization.Path
import Parsing.Syntax
import Parsing.Utility
import Data.Accumulator

translatePath :: CompilationUnit' -> ProgramPath -> ExpAccumulator CProgram
translatePath unit@(CompilationUnit' _ decls) path = do
    let (constructedClasses, usedClasses, usedArrays) 
                         = (typesInProgram unit . map fst) path
        initialExpAcc    = (0, [])
        classStructDecls = map (createClassStructDecl unit) usedClasses 
        arrayStructDecls = map (createArrayStructDecl unit) usedArrays 
        classAllocDecls  = map (createClassAllocDecl unit) constructedClasses
        calls            = (groupBy ((==) `on` (callName . snd)) . sortOn (callName . snd)) path
    staticFieldDecls   <- createStaticFields unit
    callDecls          <- mapM (translateCall unit) calls 
    (_, arrayNewDecls) <- getAccumulator
    let declarations = classStructDecls ++ arrayStructDecls ++ arrayNewDecls ++ classAllocDecls ++ staticFieldDecls ++ callDecls
    return $ cUnit declarations

translateCall :: CompilationUnit' -> ProgramPath -> ExpAccumulator CExtDecl
translateCall unit path
    = case methodDecl of
            MethodDecl'{}      
                -> translateMethodCall unit methodDecl path
            ConstructorDecl'{} 
                -> translateConstructorCall unit methodDecl path
    where
        (PathStmtInfo _ scope _) = (snd . head) path
        methodDecl               = fromJust $ getMethod unit scope
        
translateConstructorCall :: CompilationUnit' -> MemberDecl' -> ProgramPath -> ExpAccumulator CExtDecl
translateConstructorCall unit methodDecl path = do
    let (returns, declrs) = translateType unit methodType
        name              = cIdent callName
        params            = translateParams unit methodParams
        preStats          = cVarDeclStat (cDecl returns [(cDeclr thisName [cPointer], Just (cExpInit (cCall (createObjectAllocName scopeMember) [])))])
        postStats         = cReturnStat (Just thisVar)
        localInfo         = (scopeClass, paramNames)
    stats <- translateStmts unit localInfo (map transformReturnToReturnThis path)
    let body     = cCompoundStat [preStats, cBlockStat stats, postStats]
        function = cFunction returns name (params : declrs) body
    return function
    where
        (PathStmtInfo callName scope depth) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        paramNames   = namesOfParams methodParams
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: (Stmt', PathStmtInfo) ->  (Stmt', PathStmtInfo)
transformReturnToReturnThis (Return' _, info) = (Return' (Just (ExpName' ["this"])), info)
transformReturnToReturnThis s                 = s

translateMethodCall :: CompilationUnit' -> MemberDecl' -> ProgramPath -> ExpAccumulator CExtDecl
translateMethodCall unit methodDecl path = do
    let (returns, declrs) = translateType unit methodType
        name              = cIdent callName
        params            = translateParams unit methodParams
        localInfo         = (scopeClass, paramNames)
    body <- translateStmts unit localInfo path
    let function = cFunction returns name (params : declrs) body
    return function
    where
        (PathStmtInfo callName scope _) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        methodType   = fromJust $ getReturnTypeOfMethod methodDecl
        thisTy       = RefType' . ClassRefType' . ClassType' $ [scopeClass]
        paramNames   = namesOfParams (getParams methodDecl)
        methodParams = if isStatic methodDecl
                            then getParams methodDecl
                            else thisParam thisTy : getParams methodDecl
       
{-
translateCall :: CompilationUnit' -> ExpAccumulator -> ProgramPath -> (ExpAccumulator, CExtDecl)
translateCall unit expAcc path
    = case methodDecl of
            MethodDecl'{}      
                -> translateMethodCall unit methodDecl expAcc path
            ConstructorDecl'{} 
                -> translateConstructorCall unit methodDecl expAcc path
    where
        (PathStmtInfo _ scope _) = (snd . head) path
        methodDecl = fromJust $ getMethod unit scope
        
translateConstructorCall :: CompilationUnit' -> MemberDecl' -> ExpAccumulator -> ProgramPath -> (ExpAccumulator, CExtDecl)
translateConstructorCall unit methodDecl expAcc path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          preStats          = cVarDeclStat (cDecl returns [(cDeclr thisName [cPointer], Just (cExpInit (cCall (createObjectAllocName scopeMember) [])))])
          postStats         = cReturnStat (Just thisVar)
          localInfo         = (scopeClass, paramNames)
          (expAcc1, stats)  = translateStmts unit localInfo expAcc (map transformReturnToReturnThis path)
          body              = cCompoundStat [preStats, cBlockStat stats, postStats]
          function          = cFunction returns name (params : declrs) body
       in (expAcc1, function)
    where
        (PathStmtInfo callName scope depth) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        paramNames   = namesOfParams methodParams
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: (Stmt', PathStmtInfo) ->  (Stmt', PathStmtInfo)
transformReturnToReturnThis (Return' _, info) = (Return' (Just (ExpName' ["this"])), info)
transformReturnToReturnThis s                 = s

translateMethodCall :: CompilationUnit' -> MemberDecl' -> ExpAccumulator -> ProgramPath -> (ExpAccumulator, CExtDecl)
translateMethodCall unit methodDecl expAcc path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          localInfo         = (scopeClass, paramNames)
          (expAcc1, body)   = translateStmts unit localInfo expAcc path
          function          = cFunction returns name (params : declrs) body
       in (expAcc1, function)
    where
        (PathStmtInfo callName scope _) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        methodType   = fromJust $ getReturnTypeOfMethod methodDecl
        thisTy       = RefType' . ClassRefType' . ClassType' $ [scopeClass]
        paramNames   = namesOfParams (getParams methodDecl)
        methodParams = if isStatic methodDecl
                            then getParams methodDecl
                            else thisParam thisTy : getParams methodDecl
-}
translateParams :: CompilationUnit' -> FormalParams' -> CDerivedDeclr
translateParams unit = cParams . map (translateParam unit)

translateParam :: CompilationUnit' -> FormalParam' -> CDecl
translateParam unit (FormalParam' _ ty (VarId' name)) 
    = let (cTy, declrs) = translateType unit (Just ty)
          var           = (cDeclr (cIdent name) declrs, Nothing)
       in cDecl cTy [var]
       
thisParam :: Type' -> FormalParam'
thisParam ty = FormalParam' [] ty (VarId' "this")
