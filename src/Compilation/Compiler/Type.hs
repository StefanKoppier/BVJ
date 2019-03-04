module Compilation.Compiler.Type where

import Language.C.Syntax.AST
import Compilation.Utility
import Parsing.Syntax

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

-- TODO: assuming PrimType now, no multi-dimensional arrays or objects.
translateRefType unit (ArrayType' (PrimType' ty'))
    = (ty, [cPointer])
    where
        ty = case ty' of
                BooleanT' -> cBooleanArrayType; ByteT'   -> cByteArrayType  
                ShortT'   -> cShortArrayType  ; IntT'    -> cIntArrayType   
                LongT'    -> cLongArrayType   ; CharT'   -> cCharArrayType
                FloatT'   -> cFloatArrayType  ; DoubleT' -> cDoubleArrayType
