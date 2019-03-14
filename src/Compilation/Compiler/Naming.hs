module Compilation.Compiler.Naming where

import Language.C.Data.Ident
import Language.C.Syntax.AST
import Compilation.Utility
import Parsing.Syntax

thisName :: Ident
thisName = cIdent "this"

thisVar :: CExpr
thisVar = cVar thisName

arrayLengthName :: Ident
arrayLengthName = cIdent "length"

arrayLengthVar :: CExpr
arrayLengthVar = cVar arrayLengthName

createArrayTypeName :: Int -> Type' -> Ident
createArrayTypeName dimensions ty
    = cIdent (nameOfType ty ++ concat (replicate dimensions "_Array"))

nameOfType :: Type' -> String
nameOfType (RefType' (ClassRefType' (ClassType' [name])))
    = name
nameOfType (RefType' (ArrayType' ty))
    = nameOfType ty ++ "_Array"
nameOfType (PrimType' ty)
    = case ty of 
        BooleanT' -> "Boolean"; ByteT'   -> "Byte"  
        ShortT'   -> "Short"  ; IntT'    -> "Int"   
        LongT'    -> "Long"   ; CharT'   -> "Char"
        FloatT'   -> "Float"  ; DoubleT' -> "Double"

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

createArrayMethodName :: Int -> Ident
createArrayMethodName i = cIdent ("new_Array$" ++ show i)