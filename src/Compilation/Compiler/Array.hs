module Compilation.Compiler.Array where

import Data.List
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Ident
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Compilation.CProgram
import Parsing.Syntax
import Linearization.Path

import Debug.Trace

createArrayStructDecl :: CompilationUnit' -> Type' -> CExtDecl
createArrayStructDecl unit ty'
    = let name         = cIdent (nameOfType ty' ++ "_Array")
          (ty, dDeclr) = translateType unit (Just ty')
       in cStruct name [ cDecl ty       [(cDeclr arrayElementsName (cPointer : dDeclr), Nothing)]
                       , cDecl cIntType [(cDeclr arrayLengthName []                   , Nothing)] ]