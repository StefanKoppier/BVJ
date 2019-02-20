module Translation.Phase(
      translationPhase
) where

import Data.Maybe                  (fromJust)
import Data.List                   (groupBy)
import Language.C.Data.Position
import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Language.C.Data.Node
import Language.C.Data.Ident
import Auxiliary.Phase
import Auxiliary.Pretty
import Linearization.Path
import Translation.Program
import Translation.Pretty
import Parsing.Syntax
import Parsing.Utility

translationPhase :: Phase (CompilationUnit', ProgramPaths) Programs
translationPhase _ (unit, paths) = do
    newEitherT $ printHeader "4. TRANSLATION"
    newEitherT $ printPretty paths
    return $ map (translate unit) paths

translate :: CompilationUnit' -> ProgramPath -> Program
translate unit path = CTranslUnit calls noNodeInfo
    where
        calls = (map (translateCall unit) . groupBy (\ x y -> fst x == fst y)) path

translateCall :: CompilationUnit' -> ProgramPath -> CExtDecl
translateCall unit path 
    = CFDefExt $ CFunDef [returnType] (CDeclr name [params] Nothing [] noNodeInfo) [] body noNodeInfo
    where
        callName   = (head . fst . head) path
        methodName = stripCallName callName
        name       = Just (ident callName)
        body       = translatePath path
        method     = (fromJust . findMethod methodName . fromJust . findClass "Main") unit
        returnType = (CTypeSpec . translateMaybeType . fromJust . getReturnTypeOfMethod) method
        params     = (translateParams . fromJust . getParamsOfMethod) method

translateParams :: [FormalParam'] -> CDerivedDeclr
translateParams ps = CFunDeclr (Right (map translateParam ps, False)) [] noNodeInfo

translateParam :: FormalParam' -> CDeclaration NodeInfo
translateParam (FormalParam' _ ty (VarId' name)) 
    = CDecl [ty'] [(name', Nothing, Nothing)] noNodeInfo
    where
        ty'   = CTypeSpec $ translateType ty
        name' :: Maybe (CDeclarator NodeInfo)
        name' = Just $ CDeclr (Just $ ident name) [] Nothing [] noNodeInfo

translatePath :: ProgramPath -> CStat
translatePath path = CCompound [] (map (translateStmt . snd) path) noNodeInfo

translateStmt :: Stmt' -> CBlockItem
translateStmt (Decl' _ (RefType' (ArrayType' ty)) [VarDecl' (VarId' name) init])
    = let init' = translateVarInit init
          ty'   = [CTypeSpec $ translateType ty]
          name' = Just $ ident name
          size' = [CArrDeclr [] (CNoArrSize False) noNodeInfo]
          declr = [(Just (CDeclr name' size' Nothing [] noNodeInfo), init', Nothing)]
       in CBlockDecl (CDecl ty' declr noNodeInfo)

translateStmt (Decl' _ ty ds)
    = let ty' = [CTypeSpec $ translateType ty]
          ds' = map translateVarDecl ds
       in CBlockDecl (CDecl ty' ds' noNodeInfo)
   
translateStmt (ExpStmt' exp)
    = CBlockStmt (CExpr (Just $ translateExp exp) noNodeInfo)

translateStmt (Assert' exp err)
    = let err'    = maybe (CConst $ CStrConst (cString "") noNodeInfo) translateExp err
          exp'    = translateExp exp
          assert' = CExpr (Just (CCall (CVar (ident "__CPROVER_assert") noNodeInfo) [exp', err'] noNodeInfo)) noNodeInfo
       in CBlockStmt assert'

translateStmt (Assume' exp)
    = let exp'    = [translateExp exp]
          assume' = CExpr (Just (CCall (CVar (ident "__CPROVER_assume") noNodeInfo) exp' noNodeInfo)) noNodeInfo
       in CBlockStmt assume'
       
translateStmt (Return' exp)
    = let exp'    = translateMaybeExp exp
       in CBlockStmt (CReturn exp' noNodeInfo)

translateVarDecl :: VarDecl' -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
translateVarDecl (VarDecl' (VarId' name) init) 
    = ( Just $ CDeclr (Just $ ident name) [] Nothing [] noNodeInfo
      , translateVarInit init
      , Nothing)

translateVarInit :: VarInit' -> Maybe CInit
translateVarInit (InitExp' e)           = Just $ CInitExpr (translateExp e) noNodeInfo
translateVarInit (InitArray' (Just es)) = Just $ CInitList (map (([],) . fromJust . translateVarInit) es) noNodeInfo
translateVarInit (InitArray' Nothing)   = Nothing

translateMaybeType :: Maybe Type' -> CTypeSpec
translateMaybeType (Just ty) = translateType ty
translateMaybeType Nothing   = CVoidType noNodeInfo

translateType :: Type' -> CTypeSpec
translateType (PrimType' BooleanT') = CTypeDef (ident "__Bool") noNodeInfo
translateType (PrimType' ByteT')    = CTypeDef (ident "__int8") noNodeInfo
translateType (PrimType' ShortT')   = CTypeDef (ident "__int16") noNodeInfo
translateType (PrimType' IntT')     = CTypeDef (ident "__int32") noNodeInfo
translateType (PrimType' LongT')    = CTypeDef (ident "__int64") noNodeInfo
translateType (PrimType' CharT')    = CCharType noNodeInfo
translateType (PrimType' FloatT')   = CFloatType noNodeInfo
translateType (PrimType' DoubleT')  = CDoubleType noNodeInfo
translateType (RefType' ty)         = translateRefType ty

translateRefType :: RefType' -> CTypeSpec
translateRefType (ArrayType' ty) = translateType ty

translateMaybeExp :: Maybe Exp' -> Maybe CExpr
translateMaybeExp (Just e) = Just (translateExp e)
translateMaybeExp Nothing  = Nothing

translateExp :: Exp' -> CExpr
translateExp (Lit' Null')        = CVar (ident "NULL") noNodeInfo
translateExp (Lit' l)            = CConst $ translateLiteral l
translateExp (ArrayAccess' n es) = translateArrayIndex n (reverse es)
translateExp (ExpName' [name])   = CVar (ident name) noNodeInfo
translateExp (PostIncrement' e)  = CUnary CPostIncOp (translateExp e) noNodeInfo
translateExp (PostDecrement' e)  = CUnary CPostDecOp (translateExp e) noNodeInfo
translateExp (PreIncrement' e)   = CUnary CPreIncOp (translateExp e) noNodeInfo
translateExp (PreDecrement' e)   = CUnary CPreDecOp (translateExp e) noNodeInfo
translateExp (PrePlus' e)        = CUnary CPlusOp (translateExp e) noNodeInfo
translateExp (PreMinus' e)       = CUnary CMinOp (translateExp e) noNodeInfo
translateExp (PreBitCompl' e)    = CUnary CCompOp (translateExp e) noNodeInfo
translateExp (PreNot' e)         = CUnary CNegOp (translateExp e) noNodeInfo
translateExp (BinOp' e1 op e2)   = CBinary (translateOp op) (translateExp e1) (translateExp e2) noNodeInfo
translateExp (Cond' g e1 e2)     = CCond (translateExp g) (Just $ translateExp e1) (translateExp e2) noNodeInfo
translateExp (Assign' lhs op e)  = CAssign (translateAssignOp op) (translateLhs lhs) (translateExp e) noNodeInfo
translateExp (MethodInv' (MethodCall' name args))      
    = CCall (CVar ((ident . head) name) noNodeInfo) (map translateExp args) noNodeInfo

translateArrayIndex :: String -> [Exp'] -> CExpr
translateArrayIndex n [e]    = CIndex (CVar (ident n) noNodeInfo) (translateExp e) noNodeInfo
translateArrayIndex n (e:es) = CIndex (translateArrayIndex n es) (translateExp e) noNodeInfo

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

stripCallName :: String -> String
stripCallName []        = []
stripCallName (x:xs)
            | x == '$'  = []
            | otherwise = x : stripCallName xs