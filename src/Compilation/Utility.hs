module Compilation.Utility where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Syntax.Constants

--------------------------------------------------------------------------------
-- Translation unit, functions and structs
--------------------------------------------------------------------------------

-- | Construct a C translation unit from a list of C declarations.
cUnit :: [CExtDecl] -> CTranslUnit
cUnit declarations = CTranslUnit declarations noInfo

-- | Construct a C global scoped variable.
cDeclExt :: CDecl -> CExtDecl
cDeclExt = CDeclExt

-- | Construct a C function from a type, name, derivied declarators and statement.
cFunction :: CTypeSpec -> Ident -> [CDerivedDeclr] -> CStat -> CExtDecl
cFunction returns name dDeclr body
    = let declr = CDeclr (Just name) dDeclr Nothing [] noInfo
       in CFDefExt (CFunDef [CTypeSpec returns] declr [] body noInfo)

-- | Create a C declaration from a type and a list of declarators and initializers.
cDecl :: CTypeSpec -> [(CDeclr, Maybe CInit)] -> CDecl
cDecl ty vars 
    = let decls = map (\ (d, i) -> (Just d, i, Nothing)) vars
       in CDecl [CTypeSpec ty] decls noInfo

-- | Create a C declarator from an identifier and derived declarators.
cDeclr :: Ident -> [CDerivedDeclr] -> CDeclr
cDeclr name dDeclr = CDeclr (Just name) dDeclr Nothing [] noInfo

-- | Create a C declarator representing the parameters of a function.
cParams :: [CDecl] -> CDerivedDeclr
cParams params = CFunDeclr (Right (params, False)) [] noInfo

-- | Create a C struct declaration.
cStruct :: Ident -> [CDecl] -> CExtDecl
cStruct name fields 
    = let struct = CStruct CStructTag (Just name) (Just fields) [] noInfo
          ty     = CTypeSpec (CSUType struct noInfo)
       in CDeclExt (CDecl [ty] [] noInfo)

-- | Create a C enum declaration.
cEnum :: Ident -> [(Ident, Maybe CExpr)] -> CExtDecl
cEnum name members
    = let enum = CEnum (Just name) (Just members) [] noInfo
          ty   = CTypeSpec (CEnumType enum noInfo)
       in CDeclExt (CDecl [ty] [] noInfo)
    
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The C void type.
cVoidType :: CTypeSpec
cVoidType = CVoidType noInfo

-- | The C boolean type.
cBoolType :: CTypeSpec 
cBoolType = CTypeDef (cIdent "_Bool") noInfo

-- | The C byte type.
cByteType :: CTypeSpec
cByteType = CTypeDef (cIdent "__int8") noInfo

-- | The C short type.
cShortType :: CTypeSpec
cShortType = CTypeDef (cIdent "__int16") noInfo

-- | The C int type.
cIntType :: CTypeSpec
cIntType = CTypeDef (cIdent "__int32") noInfo

-- | The C long type.
cLongType :: CTypeSpec
cLongType = CTypeDef (cIdent "__int64") noInfo

-- | The C char type.
cCharType :: CTypeSpec
cCharType = CCharType noInfo

-- | The C float type.
cFloatType :: CTypeSpec
cFloatType = CFloatType noInfo

-- | The C double type.
cDoubleType :: CTypeSpec
cDoubleType = CDoubleType noInfo

-- | Create a C struct type of the given name.
cStructType :: Ident -> CTypeSpec
cStructType name
    = CSUType (CStruct CStructTag (Just name) Nothing [] noInfo) noInfo

-- | Create a C enum type of the given name.
cEnumType :: Ident -> CTypeSpec
cEnumType name
    = CEnumType (CEnum (Just name) Nothing [] noInfo) noInfo

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

-- | Construct a C compound statement from a list of statements.
cCompoundStat :: [CBlockItem] -> CStat
cCompoundStat stats = CCompound [] stats noInfo

-- | Construct a C block statement from a statement.
cBlockStat :: CStat -> CBlockItem
cBlockStat = CBlockStmt

-- | Create a C variable declaration statement.
cVarDeclStat :: CDecl -> CBlockItem
cVarDeclStat = CBlockDecl

-- | Create a C expression statement.
cExprStat :: CExpr -> CBlockItem
cExprStat expression 
    = CBlockStmt (CExpr (Just expression) noInfo)

-- | Create a C for statement with an init, guard, update and body.
cForStat :: CDecl -> CExpr -> CExpr -> CStat -> CBlockItem
cForStat init guard update body
    = CBlockStmt (CFor (Right init) (Just guard) (Just update) body noInfo)

-- | Create a C empty (;) statement.
cEmptyStat :: CBlockItem
cEmptyStat = CBlockStmt (CExpr Nothing noInfo)

-- | Create a C assert statement.
cAssertStat :: CExpr -> CString -> CBlockItem
cAssertStat expression message 
    = let methodName = cIdent "__CPROVER_assert"
          arguments  = [expression, cConst (cStringConst message)]
       in cExprStat (cCall methodName arguments)

-- | Create a C assume statement.
cAssumeStat :: CExpr -> CBlockItem
cAssumeStat expression 
    = cExprStat (cCall (cIdent "__CPROVER_assume") [expression])

-- | Create a C return statement.
cReturnStat :: Maybe CExpr -> CBlockItem
cReturnStat expression = CBlockStmt (CReturn expression noInfo)

-- | Create a C array intializer.
cArrayInit :: [CInit] -> CInit
cArrayInit inits = CInitList (map ([],) inits) noInfo

-- | Create a C expression initializer.
cExpInit :: CExpr -> CInit
cExpInit expression = CInitExpr expression noInfo

-- | Create a C zero init value.
cExpInitZero :: CInit
cExpInitZero = cExpInit (cConst (cIntConst (cInteger 0)))

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | Create a C variable access from a name.
cVar :: Ident -> CExpr
cVar name = CVar name noInfo

-- | Create a C variable assignment.
cAssign :: CAssignOp -> CExpr -> CExpr -> CExpr
cAssign op lhs exp
    = CAssign op lhs exp noInfo

-- | Create a C array index.
cIndex :: CExpr -> CExpr -> CExpr
cIndex array index
    = CIndex array index noInfo

-- | Create a C method invocation from a name and list of arguments.
cCall :: Ident -> [CExpr] -> CExpr
cCall name arguments
    = CCall (cVar name) arguments noInfo

-- | Create a C unary operator from an operator and expression.
cUnary :: CUnaryOp -> CExpr -> CExpr
cUnary op expression
    = CUnary op expression noInfo

-- | Create a C binary operator from an operator and two expressions.
cBinary :: CBinaryOp -> CExpr -> CExpr -> CExpr
cBinary op expression1 expression2
    = CBinary op expression1 expression2 noInfo

-- | Create a C conditional from a guard and two expressions.
cCond :: CExpr -> CExpr -> CExpr -> CExpr
cCond guard expression1 expression2
    = CCond guard (Just expression1) expression2 noInfo

-- | Create a C member access (->) from an expression and a name.
cMember :: CExpr -> Ident -> CExpr
cMember expression member
    = CMember expression member True noInfo

-- | Create a C member access (->) from an expression and a variable.
cMemberVar :: CExpr -> CExpr -> CExpr
cMemberVar expression (CVar name _)
    = cMember expression name

-- | Create a C sizeof from a type.
cSizeofType :: CTypeSpec -> [CDerivedDeclr] -> CExpr
cSizeofType ty []
    = CSizeofType (cDecl ty []) noInfo
cSizeofType ty dDeclr
    = let declr = (CDeclr Nothing dDeclr Nothing [] noInfo, Nothing)
       in CSizeofType (cDecl ty [declr]) noInfo

-- | Create a C expression from a constant.
cConst :: CConstant NodeInfo -> CExpr
cConst = CConst

-- | Create a C constant from a string.
cStringConst :: CString -> CConstant NodeInfo
cStringConst string = CStrConst string noInfo
    
-- | Create a C constant from a float.
cFloatConst :: CFloat -> CConstant NodeInfo
cFloatConst float = CFloatConst float noInfo
    
-- | Create a C constant from an int.
cIntConst :: CInteger -> CConstant NodeInfo
cIntConst int = CIntConst int noInfo
    
-- | Create a C constant from a char.
cCharConst :: CChar -> CConstant NodeInfo
cCharConst int = CCharConst int noInfo

-- | The C NULL constant.
cNull :: CExpr
cNull = cVar (cIdent "NULL")

--------------------------------------------------------------------------------
-- Auxiliary
--------------------------------------------------------------------------------

-- | Create a C identifier from a string.
cIdent :: String -> Ident
cIdent string = Ident string 0 noInfo

-- | Create a C malloc statement with a given size.
cMalloc :: CExpr -> CExpr
cMalloc size = cCall (cIdent "malloc") [size]
    
-- | Create a C calloc statement with a given size.
cCalloc :: CExpr -> CExpr -> CExpr
cCalloc amount size = cCall (cIdent "calloc") [amount, size]

-- | Create a C pointer derived declarator.
cPointer :: CDerivedDeclr
cPointer = CPtrDeclr [] noInfo

-- | Create a C array derived declarator of given size.
cArray :: CExpr -> CDerivedDeclr
cArray size = CArrDeclr [] (CArrSize False size) noInfo

noInfo :: NodeInfo
noInfo = OnlyPos nopos (nopos, 0)
