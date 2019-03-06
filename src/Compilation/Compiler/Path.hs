module Compilation.Compiler.Path where

import Data.Maybe                     (fromJust)
import qualified Data.Set as S
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
import Compilation.Compiler.InformationGathering
import Linearization.Path
import Parsing.Syntax
import Parsing.Utility

import Debug.Trace

translatePath :: CompilationUnit' -> ProgramPath -> CProgram
translatePath unit@(CompilationUnit' _ decls) path
    = let (constructedClasses, usedClasses, constructedArrays, usedArrays) 
            = (typesInProgram unit . map fst) path
          classStructDecls  = map (createClassStructDecl unit) usedClasses 
          arrayStructDecls  = map (createArrayStructDecl unit) usedArrays 
          classAllocDecls   = map (createClassAllocDecl unit) constructedClasses
          arrayAllocDecls   = map (createArrayAllocDecl unit) constructedArrays
          staticFieldDecls  = createStaticFields unit
          callDecls         = map (translateCall unit) . groupBy ((==) `on` (callName . snd)) . sortOn (callName . snd) $ path
          declarations      = classStructDecls ++ arrayStructDecls ++ classAllocDecls ++ arrayAllocDecls ++ staticFieldDecls ++ callDecls
       in cUnit declarations

translateCall :: CompilationUnit' -> ProgramPath -> CExtDecl
translateCall unit path
    = case methodDecl of
            MethodDecl'{}      -> translateMethodCall unit methodDecl path
            ConstructorDecl'{} -> translateConstructorCall unit methodDecl path
    where
        (PathStmtInfo _ scope _)= (snd . head) path
        methodDecl = fromJust $ getMethod unit scope
        
translateConstructorCall :: CompilationUnit' -> MemberDecl' -> ProgramPath -> CExtDecl
translateConstructorCall unit methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          preStats          = cVarDeclStat (cDecl returns [(cDeclr thisName [cPointer], Just (cExpInit (cCall (createObjectAllocName scopeMember) [])))])
          postStats         = cReturnStat (Just thisVar)
          localInfo         = (scopeClass, paramNames)
          stats             = cBlockStat $ translateStmts unit localInfo (map transformReturnToReturnThis path)
          body              = cCompoundStat [preStats, stats, postStats]
       in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope depth) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        paramNames   = namesOfParams methodParams
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: (Stmt', PathStmtInfo) ->  (Stmt', PathStmtInfo)
transformReturnToReturnThis (Return', info) = (ReturnExp' (ExpName' ["this"]), info)
transformReturnToReturnThis s               = s

translateMethodCall :: CompilationUnit' -> MemberDecl' -> ProgramPath -> CExtDecl
translateMethodCall unit methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          localInfo         = (scopeClass, paramNames)
          body              = translateStmts unit localInfo path
       in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope _) = snd . head $ path
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
