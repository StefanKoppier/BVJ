module Compilation.Compiler.Object where

import Language.C.Syntax.AST
import Data.Maybe
import Compilation.Utility
import Compilation.Compiler.Expression
import Compilation.Compiler.Statement
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Parsing.Utility

import Debug.Trace

--------------------------------------------------------------------------------
-- Struct and allocator creation of classes.
--------------------------------------------------------------------------------

createClassStructDecl :: CompilationUnit' -> ClassType' -> CExtDecl
createClassStructDecl unit (ClassType' [name'])
    = translateClassStruct unit class'
    where 
        class' = fromJust $ findClass name' unit

createClassAllocDecl :: CompilationUnit' -> ClassType' -> CExtDecl
createClassAllocDecl unit (ClassType' [name'])
    = translateStructAllocator unit class'
    where 
        class' = fromJust $ findClass name' unit

translateClassStruct :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateClassStruct unit classDecl@(ClassDecl' _ name' _)
    = let fieldDecls    = concatMap (translateField unit) nonStaticFields'
          structDecl    = cStruct (cIdent name') fieldDecls
       in structDecl
    where
        fields'          = getFields classDecl
        nonStaticFields' = filter (not . isStatic) fields'

translateStructAllocator :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateStructAllocator unit classDecl@(ClassDecl' _ name' _)
    = let (ty, declrs) = translateRefType unit (ClassRefType' (ClassType' [name']))
          name         = createObjectAllocName name'
          alloc        = cVarDeclStat (cDecl ty [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType ty []))))])
          inits        = concatMap (translateFieldInitializer unit classDecl) nonStaticFields'
          return       = cReturnStat (Just thisVar)
          body         = cCompoundStat ([alloc] ++ inits ++ [return])
        in cFunction ty name (cParams [] : declrs) body
    where
        classFields'     = getFields classDecl
        nonStaticFields' = filter (not . isStatic) classFields'

translateField :: CompilationUnit' -> MemberDecl' -> [CDecl]
translateField unit (FieldDecl' _ ty' vars')
    = let ty = translateType unit (Just ty')
       in map (translateFieldDecl unit ty) vars'

translateFieldDecl :: CompilationUnit' -> (CTypeSpec, [CDerivedDeclr]) -> VarDecl' -> CDecl
translateFieldDecl _ (ty, declrs) (VarDecl' (VarId' name') _)
    = cDecl ty [(cDeclr (cIdent name') declrs, Nothing)]

translateFieldInitializer :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CBlockItem]
translateFieldInitializer unit classDecl (FieldDecl' _ _ decls)
    = map (translateFieldDeclInitializer unit classDecl) decls

translateFieldDeclInitializer :: CompilationUnit' -> ClassDecl' -> VarDecl' -> CBlockItem
translateFieldDeclInitializer unit (ClassDecl' _ className' _) (VarDecl' (VarId' name') (InitExp' exp'))
    = let localInfo  = (className', [])
          exp        = translateExp unit localInfo exp'
          name       = cIdent name'
          assignment = cAssign CAssignOp (cMember thisVar name) exp
       in cExprStat assignment
    
translateFieldDeclInitializer unit _ (VarDecl' (VarId' name') (InitArray' Nothing'))
    = undefined
    
--------------------------------------------------------------------------------
-- Creation of static fields of classes.
--------------------------------------------------------------------------------

createStaticFields :: CompilationUnit' -> [CExtDecl]
createStaticFields unit@(CompilationUnit' _ decls)
    = concatMap (translateStaticFields unit) classDecls
    where
        classDecls = [c | ClassTypeDecl'(c) <- decls]

translateStaticFields :: CompilationUnit' -> ClassDecl' -> [CExtDecl]
translateStaticFields unit classDecl@(ClassDecl' modifiers name body)
    = concatMap (translateStaticField unit classDecl) staticFields
    where
        fields       = getFields classDecl
        staticFields = filter isStatic fields

translateStaticField :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CExtDecl]
translateStaticField unit classDecl (FieldDecl' _ ty' vars')
    = map (translateStaticFieldDecl unit ty' classDecl) vars'

translateStaticFieldDecl :: CompilationUnit' -> Type' -> ClassDecl' -> VarDecl' -> CExtDecl
translateStaticFieldDecl unit ty' (ClassDecl' _ name _) (VarDecl' (VarId' id) init')
    = let (ty, declrs) = translateType unit (Just ty')
          renamed      = VarDecl' (VarId' (createStaticFieldName name id)) init'
          localInfo    = (name, [])
          decl         = translateVarDecl unit localInfo declrs renamed
       in cDeclExt (cDecl ty [decl])