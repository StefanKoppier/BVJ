module Compilation.Compiler.Type where

import Language.C.Syntax.AST
import Compilation.Utility
import Compilation.Compiler.Naming
import Parsing.Syntax

translateType :: CompilationUnit' -> Maybe Type' -> (CTypeSpec, [CDerivedDeclr])
translateType _ Nothing = (cVoidType, [])

translateType _ (Just (PrimType' ty))
    = (translatePrimType ty, [])

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
translateRefType _ (ClassRefType' (ClassType' [name]))
    = (cStructType (cIdent name), [cPointer])

translateRefType _ ty@ArrayType'{}
    = (cStructType (cIdent (nameOfType (RefType' ty))), [cPointer])