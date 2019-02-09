module Translation.Phase(
      translationPhase
    , Program
    , Programs
) where

import Language.C.Data.Position
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident
import Control.Phase
import Linearization.Phase
import Linearization.Path
import Analysis.Syntax

type Program = CTranslUnit

type Programs = [Program]

translationPhase :: Phase ProgramPaths Programs
translationPhase _ paths = do
    newEitherT $ printHeader "4. TRANSLATION"
    return $ map translate paths

translate :: ProgramPath -> Program
translate path = CTranslUnit [
    CFDefExt (CFunDef [CTypeSpec (CVoidType noNodeInfo)] (CDeclr (Just (ident "main")) [CFunDeclr (Right ([], False)) [] noNodeInfo] Nothing [] noNodeInfo) [] (translatePath path) noNodeInfo)] noNodeInfo

translatePath :: ProgramPath -> CStat
translatePath path = CCompound [] (map translateStmt path) noNodeInfo

translateStmt :: Stmt' -> CBlockItem
translateStmt (Decl' _ ty [VarDecl' (VarId' name) init])
    = let init' = Just $ translateVarInit init
       in CBlockDecl (CDecl [translateType ty] [(Just (CDeclr (Just (ident name)) [] Nothing [] noNodeInfo), init', Nothing)] noNodeInfo)

translateStmt (ExpStmt' exp)
    = CBlockStmt (CExpr (Just $ translateExp exp) noNodeInfo)

translateStmt (Assert' exp error)
    = let error'  = maybe (CConst $ CStrConst (cString "") noNodeInfo) translateExp error
          exp'    = translateExp exp
          assert' = CExpr (Just (CCall (CVar (ident "__CPROVER_assert") noNodeInfo) [exp', error'] noNodeInfo)) noNodeInfo
       in CBlockStmt assert'

translateStmt (Assume' exp)
    = let exp'    = [translateExp exp]
          assume' = CExpr (Just (CCall (CVar (ident "__CPROVER_assume") noNodeInfo) exp' noNodeInfo)) noNodeInfo
       in CBlockStmt assume'

translateVarInit :: VarInit' -> CInit
translateVarInit (InitExp' e) = CInitExpr (translateExp e) noNodeInfo

translateType :: Type' -> CDeclarationSpecifier NodeInfo
translateType (PrimType' BooleanT') = CTypeSpec (CTypeDef (ident "__Bool") noNodeInfo)
translateType (PrimType' ByteT')    = CTypeSpec (CTypeDef (ident "__int8") noNodeInfo)
translateType (PrimType' ShortT')   = CTypeSpec (CTypeDef (ident "__int16") noNodeInfo)
translateType (PrimType' IntT')     = CTypeSpec (CTypeDef (ident "__int32") noNodeInfo)
translateType (PrimType' LongT')    = CTypeSpec (CTypeDef (ident "__int64") noNodeInfo)
translateType (PrimType' CharT')    = CTypeSpec (CCharType noNodeInfo)
translateType (PrimType' FloatT')   = CTypeSpec (CFloatType noNodeInfo)
translateType (PrimType' DoubleT')  = CTypeSpec (CDoubleType noNodeInfo)

translateExp :: Exp' -> CExpr
translateExp (Lit' Null')       = CVar (ident "NULL") noNodeInfo
translateExp (Lit' l)           = CConst $ translateLiteral l
translateExp (ExpName' [name])  = CVar (ident name) noNodeInfo
translateExp (PostIncrement' e) = CUnary CPostIncOp (translateExp e) noNodeInfo
translateExp (PostDecrement' e) = CUnary CPostDecOp (translateExp e) noNodeInfo
translateExp (PreIncrement' e)  = CUnary CPreIncOp (translateExp e) noNodeInfo
translateExp (PreDecrement' e)  = CUnary CPreDecOp (translateExp e) noNodeInfo
translateExp (PrePlus' e)       = CUnary CPlusOp (translateExp e) noNodeInfo
translateExp (PreMinus' e)      = CUnary CMinOp (translateExp e) noNodeInfo
translateExp (PreBitCompl' e)   = CUnary CCompOp (translateExp e) noNodeInfo
translateExp (PreNot' e)        = CUnary CNegOp (translateExp e) noNodeInfo
translateExp (BinOp' e1 op e2)  = CBinary (translateOp op) (translateExp e1) (translateExp e2) noNodeInfo
translateExp (Cond' g e1 e2)    = CCond (translateExp g) (Just $ translateExp e1) (translateExp e2) noNodeInfo
translateExp (Assign' lhs op e) = CAssign (translateAssignOp op) (translateLhs lhs) (translateExp e) noNodeInfo

translateLhs :: Lhs' -> CExpr
translateLhs (Name' [name]) = CVar (ident name) noNodeInfo

translateOp :: Op' -> CBinaryOp
translateOp Mult'    = CMulOp
translateOp Div'     = CDivOp
translateOp Rem'     = CRmdOp
translateOp Add'     = CAddOp
translateOp Sub'     = CSubOp
translateOp LShift'  = CShlOp
translateOp RShift'  = CShrOp
translateOp RRShift' = CShrOp
translateOp LThan'   = CLeOp
translateOp GThan'   = CGrOp
translateOp LThanE'  = CLeqOp
translateOp GThanE'  = CGeqOp
translateOp Equal'   = CEqOp
translateOp NotEq'   = CNeqOp
translateOp And'     = CAndOp
translateOp Or'      = COrOp
translateOp Xor'     = CXorOp
translateOp CAnd'    = CLndOp
translateOp COr'     = CLorOp

translateAssignOp :: AssignOp' -> CAssignOp
translateAssignOp EqualA'   = CAssignOp
translateAssignOp MultA'    = CMulAssOp
translateAssignOp DivA'     = CDivAssOp
translateAssignOp RemA'     = CRmdAssOp
translateAssignOp AddA'     = CAddAssOp
translateAssignOp SubA'     = CSubAssOp
translateAssignOp LShiftA'  = CShlAssOp
translateAssignOp RShiftA'  = CShrAssOp
translateAssignOp RRShiftA' = CShrAssOp
translateAssignOp AndA'     = CAndAssOp
translateAssignOp XorA'     = CXorAssOp
translateAssignOp OrA'      = COrAssOp

translateLiteral :: Literal' -> CConst
translateLiteral (Int'     x)     = CIntConst (cInteger x) noNodeInfo
translateLiteral (Float'   x)     = CFloatConst (cFloat x) noNodeInfo
translateLiteral (Double'  x)     = CFloatConst (cFloat x) noNodeInfo
translateLiteral (Boolean' True)  = CIntConst (cInteger 1) noNodeInfo
translateLiteral (Boolean' False) = CIntConst (cInteger 0) noNodeInfo
translateLiteral (Char'    x)     = CCharConst (cChar x) noNodeInfo
translateLiteral (String'  x)     = CStrConst (cString x) noNodeInfo

ident :: String -> Ident
ident x = Ident x 0 noNodeInfo

noNodeInfo :: NodeInfo
noNodeInfo = OnlyPos nopos (nopos, 0)