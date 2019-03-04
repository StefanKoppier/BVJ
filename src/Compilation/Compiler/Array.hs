module Compilation.Compiler.Array where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax

createDefaultDeclarations :: [CExtDecl]
createDefaultDeclarations 
    = let arrayTys = [ ("Boolean", Just BooleanT'), ("Byte"  , Just ByteT'  )
                     , ("Short"  , Just ShortT'  ), ("Int"   , Just IntT'   )
                     , ("Long"   , Just LongT'   ), ("Char"  , Just CharT'  )
                     , ("Float"  , Just FloatT'  ), ("Double", Just DoubleT')
                     , ("Ref"    , Nothing       ) ] 
          arrayStructs    = map (uncurry createArrayDeclaration) arrayTys
          arrayAllocators = map (uncurry createArrayAllocator) arrayTys
       in arrayStructs ++ arrayAllocators

createArrayDeclaration :: String -> Maybe PrimType' -> CExtDecl
createArrayDeclaration name Nothing
    = cStruct (cIdent (name ++ "_Array"))
        [ cDecl cVoidType [(cDeclr arrayElementsName [cPointer, cPointer], Nothing)]
        , cDecl cIntType  [(cDeclr arrayLengthName   []                  , Nothing)] ]
        
createArrayDeclaration name (Just ty')
    = let ty = translatePrimType ty'
       in cStruct (cIdent (name ++ "_Array"))
            [ cDecl ty       [(cDeclr arrayElementsName [cPointer], Nothing)]
            , cDecl cIntType [(cDeclr arrayLengthName   []        , Nothing)] ]

createArrayAllocator :: String -> Maybe PrimType' -> CExtDecl
createArrayAllocator name' ty' 
    = let tyName     = name' ++ "_Array"
          name       = cIdent ("allocator_" ++ tyName)
          ty         = cStructType (cIdent tyName)
          params     = cParams [cDecl cIntType [(cDeclr arrayLengthName [], Nothing)]]
          declrs     = [params, cPointer]
          alloc      = cVarDeclStat (cDecl ty [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType ty []))))])
          (elemType, elemDDeclr) = maybe (cVoidType, [cPointer]) (\ ty -> (translatePrimType ty, [])) ty'
          calloc     = cCalloc arrayLengthVar (cSizeofType elemType elemDDeclr)
          init       = cCompoundStat
                        [ cExprStat (cAssign CAssignOp (cMember thisVar arrayLengthName)   arrayLengthVar)
                        , cExprStat (cAssign CAssignOp (cMember thisVar arrayElementsName) calloc) ] 
          return     = cReturnStat (Just thisVar)
          body       = cCompoundStat [alloc, cBlockStat init, return]
       in cFunction ty name declrs body
