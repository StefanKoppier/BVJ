module Compilation.Compiler.Type where

import Language.C.Syntax.AST
import Compilation.Utility
import Parsing.Syntax
import Debug.Trace

translateType :: CompilationUnit' -> Maybe Type' -> (CTypeSpec, [CDerivedDeclr])
translateType _ Nothing = (cVoidType, [])

translateType _ (Just (PrimType' ty'))
    = (ty, [])
    where
        ty = translatePrimType ty'

translateType unit (Just (RefType' ty))
    = translateRefType unit ty
    
translatePrimType :: PrimType' -> CTypeSpec
translatePrimType ty
    = case ty of
        BooleanT' -> cBoolType ; ByteT'   -> cByteType  
        ShortT'   -> cShortType; IntT'    -> cIntType   
        LongT'    -> cLongType ; CharT'   -> cCharType
        FloatT'   -> cFloatType; DoubleT' -> cDoubleType

translateRefType :: CompilationUnit' -> RefType' -> (CTypeSpec, [CDerivedDeclr])
translateRefType _ (ClassRefType' (ClassType' [name']))
    = let name = cIdent name'
       in (cStructType name, [cPointer])

translateRefType unit ty@(ArrayType' _)
    = let name = cIdent (nameOfType (RefType' ty))
       in (cStructType name, [cPointer])

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