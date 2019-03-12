module Compilation.Compiler.Array where

import Language.C.Syntax.AST
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax

createArrayStructDecl :: CompilationUnit' -> Type' -> CExtDecl
createArrayStructDecl unit ty
    = let name          = cIdent (nameOfType ty ++ "_Array")
          (cTy, dDeclr) = translateType unit (Just ty)
       in cStruct name [ cDecl cTy       [(cDeclr arrayElementsName (cPointer : dDeclr), Nothing)]
                       , cDecl cIntType  [(cDeclr arrayLengthName   []                 , Nothing)] ]