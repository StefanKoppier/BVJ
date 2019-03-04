module Compilation.Compiler.Path where

import Data.Maybe                     (fromJust)
import Data.List                      (groupBy, sortOn, mapAccumL)
import Data.Function                  (on)
import Language.C.Syntax.AST
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Statement
import Compilation.Compiler.Array
import Compilation.Compiler.Object
import Compilation.Compiler.Naming
import Linearization.Path
import Parsing.Syntax
import Parsing.Utility

translatePath :: CompilationUnit' -> ProgramPath -> CProgram
translatePath unit@(CompilationUnit' _ decls) path
    = let classes      = concatMap (translateClass unit) classDecls
          calls        = map (translateCall unit) . groupBy ((==) `on` (callName . snd)) . sortOn (callName . snd) $ path
          declarations = createDefaultDeclarations ++ classes ++ calls
       in cUnit declarations
    where
        classDecls = [c | ClassTypeDecl'(c) <- decls]

translateCall :: CompilationUnit' -> ProgramPath -> CExtDecl
translateCall unit path
    = case methodDecl of
            MethodDecl'{}      -> translateMethodCall unit callName methodDecl path
            ConstructorDecl'{} -> translateConstructorCall unit callName methodDecl path
    where
        (PathStmtInfo callName scope@(Scope _ _ scopeMember)) = snd . head $ path
        methodName = scopeMember
        methodDecl = fromJust $ getMethod unit scope
        
translateConstructorCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateConstructorCall unit callName methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          preStats          = cVarDeclStat (cDecl returns [(cDeclr thisName [cPointer], Just (cExpInit (cCall (createObjectAllocName methodName) [])))])
          postStats         = cReturnStat (Just thisVar)
          localInfo         = (scopeClass, paramNames)
          stats             = cBlockStat $ translateStmts unit localInfo (map (transformReturnToReturnThis . fst) path)
          body              = cCompoundStat [preStats, stats, postStats]
       in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope@(Scope _ scopeClass scopeMember)) = snd . head $ path
        methodName   = scopeMember
        paramNames   = namesOfParams methodParams
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: Stmt' -> Stmt'
transformReturnToReturnThis Return' = ReturnExp' (ExpName' ["this"])
transformReturnToReturnThis s       = s

translateMethodCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateMethodCall unit callName methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          localInfo         = (scopeClass, paramNames)
          body              = translateStmts unit localInfo (map fst path)
       in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        methodType   = fromJust $ getReturnTypeOfMethod methodDecl
        thisTy       = RefType' . ClassRefType' . ClassType' $ [scopeClass]
        paramNames   = namesOfParams (getParams methodDecl)
        methodParams = if isStatic methodDecl
                            then getParams methodDecl
                            else thisParam thisTy : getParams methodDecl

translateParams :: CompilationUnit' -> FormalParams' -> CDerivedDeclr
translateParams unit params 
    = cParams (map (translateParam unit) params)

translateParam :: CompilationUnit' -> FormalParam' -> CDecl
translateParam unit (FormalParam' _ ty' (VarId' name)) 
    = let (ty, declrs) = translateType unit (Just ty')
          var          = (cDeclr (cIdent name) declrs, Nothing)
       in cDecl ty [var]
       
thisParam :: Type' -> FormalParam'
thisParam ty = FormalParam' [] ty (VarId' "this")
