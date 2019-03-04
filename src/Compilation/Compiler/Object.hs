module Compilation.Compiler.Object where

import Language.C.Syntax.AST
import Compilation.Utility
import Compilation.Compiler.Expression
import Compilation.Compiler.Statement
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Parsing.Utility

translateClass :: CompilationUnit' -> ClassDecl' -> [CExtDecl]
translateClass unit classDecl@(ClassDecl' modifiers name body)
    = let fields       = concatMap (translateField unit) nonStaticFields'
          struct       = cStruct (cIdent name) fields
          allocator    = translateStructAllocator unit classDecl
          staticFields = concatMap (translateStaticField unit classDecl) staticFields'
       in struct : allocator : staticFields
    where
        classFields'     = getFields classDecl
        nonStaticFields' = filter (not . isStatic) classFields'
        staticFields'    = filter isStatic classFields'

translateField :: CompilationUnit' -> MemberDecl' -> [CDecl]
translateField unit (FieldDecl' _ ty' vars')
    = let ty    = translateType unit (Just ty')
          decls = map (translateFieldDecl unit ty) vars'
      in decls

translateFieldDecl :: CompilationUnit' -> (CTypeSpec, [CDerivedDeclr]) -> VarDecl' -> CDecl
translateFieldDecl _ (ty, declrs) (VarDecl' (VarId' name') _)
    = cDecl ty [(cDeclr (cIdent name') declrs, Nothing)]

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
