module Compilation.Compiler.Object where

import Language.C.Syntax.AST
import Data.Maybe
import Data.List
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
createClassStructDecl unit (ClassType' [name])
    = translateClassStruct unit (fromJust $ findClass name unit)

createClassAllocDecl :: CompilationUnit' -> ClassType' -> CExtDecl
createClassAllocDecl unit (ClassType' [name])
    = translateStructAllocator unit (fromJust $ findClass name unit)

translateClassStruct :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateClassStruct unit classDecl@(ClassDecl' _ name _)
    = let fieldDecls = concatMap (translateField unit) nonStaticFields
          structDecl = cStruct (cIdent name) fieldDecls
       in structDecl
    where
        fields          = getFields classDecl
        nonStaticFields = filter (not . isStatic) fields

translateStructAllocator :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateStructAllocator unit classDecl@(ClassDecl' _ name _)
    = let (cTy, declrs) = translateRefType unit (ClassRefType' (ClassType' [name]))
          cName         = createObjectAllocName name
          alloc         = cVarDeclStat (cDecl cTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType cTy []))))])
          inits         = concatMap (translateFieldInitializer unit classDecl) nonStaticFields
          return        = cReturnStat (Just thisVar)
          body          = cCompoundStat ([alloc] ++ inits ++ [return])
        in cFunction cTy cName (cParams [] : declrs) body
    where
        classFields     = getFields classDecl
        nonStaticFields = filter (not . isStatic) classFields

translateField :: CompilationUnit' -> MemberDecl' -> [CDecl]
translateField unit (FieldDecl' _ ty' vars')
    = let ty = translateType unit (Just ty')
       in map (translateFieldDecl unit ty) vars'

translateFieldDecl :: CompilationUnit' -> (CTypeSpec, [CDerivedDeclr]) -> VarDecl' -> CDecl
translateFieldDecl _ (ty, declrs) (VarDecl' (VarId' name) _)
    = cDecl ty [(cDeclr (cIdent name) declrs, Nothing)]

translateFieldInitializer :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CBlockItem]
translateFieldInitializer unit classDecl (FieldDecl' _ _ decls)
    = map (translateFieldDeclInitializer unit classDecl) decls

translateFieldDeclInitializer :: CompilationUnit' -> ClassDecl' -> VarDecl' -> CBlockItem
translateFieldDeclInitializer unit (ClassDecl' _ className _) (VarDecl' (VarId' name) (InitExp' exp))
    = let localInfo     = (className, [])
          expAcc        = (0, [])
          (decls, cExp) = translateExp unit localInfo expAcc exp
          cName         = cIdent name
          assignment    = cAssign CAssignOp (cMember thisVar cName) cExp
       in cExprStat assignment
    
translateFieldDeclInitializer _ _ (VarDecl' (VarId' _) (InitArray' Nothing))
    = trace "translateFieldDeclInitializer" undefined
    
--------------------------------------------------------------------------------
-- Creation of static fields of classes.
--------------------------------------------------------------------------------

createStaticFields :: CompilationUnit' -> ExpAccumulator -> (ExpAccumulator, [CExtDecl])
createStaticFields unit@(CompilationUnit' _ decls) expAcc
    = let (expAcc1, fields) = mapAccumL (translateStaticFields unit) expAcc classDecls
       in (expAcc1, concat fields)
    where
        classDecls = [c | ClassTypeDecl'(c) <- decls]

translateStaticFields :: CompilationUnit' -> ExpAccumulator -> ClassDecl' -> (ExpAccumulator, [CExtDecl])
translateStaticFields unit expAcc classDecl@(ClassDecl' modifiers name body)
    = let (expAcc1, fields) = mapAccumL (translateStaticField unit classDecl) expAcc staticFields
       in (expAcc1, concat fields)
    where
        fields       = getFields classDecl
        staticFields = filter isStatic fields

translateStaticField :: CompilationUnit' -> ClassDecl' -> ExpAccumulator -> MemberDecl' -> (ExpAccumulator, [CExtDecl])
translateStaticField unit classDecl expAcc (FieldDecl' _ ty' vars')
    = mapAccumL (translateStaticFieldDecl unit ty' classDecl) expAcc vars'

translateStaticFieldDecl :: CompilationUnit' -> Type' -> ClassDecl' -> ExpAccumulator -> VarDecl' -> (ExpAccumulator, CExtDecl)
translateStaticFieldDecl unit ty' (ClassDecl' _ name _) expAcc (VarDecl' (VarId' id) init')
    = let (ty, declrs) = translateType unit (Just ty')
          renamed      = VarDecl' (VarId' (createStaticFieldName name id)) init'
          acc          = ((name, []), expAcc)
          (acc1, decl) = translateVarDecl unit declrs acc renamed
          expAcc1      = snd acc1
       in (expAcc1, cDeclExt (cDecl ty [decl]))