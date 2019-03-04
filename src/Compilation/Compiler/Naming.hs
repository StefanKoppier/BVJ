module Compilation.Compiler.Naming where

import Language.C.Data.Ident
import Language.C.Syntax.AST
import Compilation.Utility

thisName :: Ident
thisName = cIdent "this"

thisVar :: CExpr
thisVar = cVar thisName

arrayLengthName :: Ident
arrayLengthName = cIdent "length"

arrayLengthVar :: CExpr
arrayLengthVar = cVar arrayLengthName

arrayElementsName :: Ident
arrayElementsName = cIdent "elements"

arrayElementsVar :: CExpr
arrayElementsVar = cVar arrayElementsName

createStructName :: String -> String -> Ident
createStructName className fieldName
    = cIdent (className ++ "_" ++ fieldName)

createStaticFieldName :: String -> String -> String
createStaticFieldName className fieldName
    = className ++ "_" ++ fieldName

createObjectAllocName :: String -> Ident
createObjectAllocName className
    = cIdent ("allocator_" ++ className)