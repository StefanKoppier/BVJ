module Compilation.Compiler.Array where

import Data.List
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Linearization.Path

import Debug.Trace

createArrayStructDecl :: CompilationUnit' -> Type' -> CExtDecl
createArrayStructDecl unit ty'
    = let name         = cIdent (nameOfType ty' ++ "_Array")
          (ty, dDeclr) = translateType unit (Just ty')
       in cStruct name [ cDecl ty       [(cDeclr arrayElementsName (cPointer : dDeclr), Nothing)]
                       , cDecl cIntType [(cDeclr arrayLengthName []                   , Nothing)] ]

createArrayAllocDecl :: CompilationUnit' -> Type' -> CExtDecl
createArrayAllocDecl unit ty'
    = let name       = cIdent ("allocator_" ++ nameOfType ty' ++ "_Array")
          returnTy   = cStructType (cIdent (nameOfType ty' ++ "_Array"))
          params     = cParams [cDecl cIntType [(cDeclr arrayLengthName [], Nothing)]]
          declrs     = [params, cPointer]
          alloc      = cVarDeclStat (cDecl returnTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType returnTy []))))])
          (elemType, elemDDeclr) = translateType unit (Just ty')
          calloc     = cCalloc arrayLengthVar (cSizeofType elemType elemDDeclr)
          init       = cCompoundStat
                        [ cExprStat (cAssign CAssignOp (cMember thisVar arrayLengthName)   arrayLengthVar)
                        , cExprStat (cAssign CAssignOp (cMember thisVar arrayElementsName) calloc) ] 
          return     = cReturnStat (Just thisVar)
          body       = cCompoundStat [alloc, cBlockStat init, return]
       in cFunction returnTy name declrs body