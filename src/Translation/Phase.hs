module Translation.Phase(
      translationPhase
) where

import Data.Maybe                  (fromJust)
import Data.List                   (groupBy, sortOn)
import Data.Function               (on)
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
import Translation.Utility

import Debug.Trace

translationPhase :: Phase (CompilationUnit', ProgramPaths) [CTranslUnit]
translationPhase _ (unit, paths) = do
    newEitherT $ printHeader "4. TRANSLATION"
    newEitherT $ printPretty paths
    return $ map (translatePath unit) paths

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

translatePath :: CompilationUnit' -> ProgramPath -> CTranslUnit
translatePath unit@(CompilationUnit' _ decls) path
    = let classes      = concatMap (translateClass unit) classDecls
          calls        = map (translateCall unit) . groupBy ((==) `on` (callName . snd)) . sortOn (callName . snd) $ path
          declarations = classes ++ calls
       in cUnit declarations
    where
        classDecls :: [ClassDecl']
        classDecls = [c | ClassTypeDecl'(c) <- decls]

translateCall :: CompilationUnit' -> ProgramPath -> CExtDecl
translateCall unit path
    = case methodDecl of
         MethodDecl'{}      -> translateMethodCall unit callName methodDecl path
         ConstructorDecl'{} -> translateConstructorCall unit callName methodDecl path
    where
        (PathStmtInfo callName scope) = snd . head $ path
        methodName = scopeMember scope
        methodDecl = fromJust $ getMethod unit methodName

translateConstructorCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateConstructorCall unit callName methodDecl path
    = let returns = translateType unit methodType
          name    = cIdent callName
          params  = translateParams unit methodParams
          stats   =  cVarDeclStat (cDecl returns [(cDeclr cThisIdent [cPointer], Just (cExpInit (cCall (cIdent ("allocator_" ++ methodName)) [])))])
                  :  map (translateStmt unit . transformReturnToReturnThis . fst) path
                  ++ [cReturnStat (Just cThis)]
          body    = cCompoundStat stats
       in cFunction returns name [params, cPointer] body
    where
        (PathStmtInfo callName scope) = snd . head $ path
        methodName = scopeMember scope
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: Stmt' -> Stmt'
transformReturnToReturnThis Return' = ReturnExp' (ExpName' ["this"])
transformReturnToReturnThis s       = s

translateMethodCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateMethodCall unit callName methodDecl path
    = let returns = translateType unit methodType
          name    = cIdent callName
          params  = translateParams unit methodParams
          body    = cCompoundStat (map (translateStmt unit . fst) path)
          declr   = params : [cPointer | isRefType methodType]
      in cFunction returns name declr body
    where
        (PathStmtInfo callName scope) = snd . head $ path
        methodName   = scopeMember scope
        className    = scopeClass scope
        methodType   = fromJust $ getReturnTypeOfMethod methodDecl
        thisTy       = RefType' . ClassRefType' . ClassType' $ [className]
        methodParams = if isStatic methodDecl
                            then getParams methodDecl
                            else thisParam thisTy : getParams methodDecl

translateParams :: CompilationUnit' -> FormalParams' -> CDerivedDeclr
translateParams unit params 
    = cParams (map (translateParam unit) params)

translateParam :: CompilationUnit' -> FormalParam' -> CDecl
translateParam unit (FormalParam' _ ty' (VarId' name)) 
    = let ty     = translateType unit (Just ty')
          declrs = [cPointer | isRefType (Just ty')]
          var    = (cDeclr (cIdent name) declrs, Nothing)
       in cDecl ty [var]

translateClass :: CompilationUnit' -> ClassDecl' -> [CExtDecl]
translateClass unit classDecl@(ClassDecl' modifiers name body)
    = let fields       = concatMap (translateField unit) nonStaticFields'
          struct       = cStruct (cIdent name) fields
          allocator    = translateStructAllocator unit classDecl
          staticFields = concatMap (translateStaticField unit classDecl) staticFields'
       in struct : allocator : staticFields
    where
        classFields'     = getFields classDecl
        nonStaticFields' = filter (not . isStatic) classFields'
        staticFields'    = filter isStatic classFields'

translateField :: CompilationUnit' -> MemberDecl' -> [CDecl]
translateField unit (FieldDecl' _ ty' vars')
    = let ty    = translateType unit (Just ty')
          decls = map (translateFieldDecl unit ty) vars'
       in decls

translateFieldDecl :: CompilationUnit' -> CTypeSpec -> VarDecl' -> CDecl
translateFieldDecl _ ty (VarDecl' (VarId' name') _)
    = cDecl ty [(cDeclr (cIdent name') [], Nothing)]

translateStaticField :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CExtDecl]
translateStaticField unit classDecl (FieldDecl' _ ty' vars')
    = map (translateStaticFieldDecl unit ty' classDecl) vars'
       
translateStaticFieldDecl :: CompilationUnit' -> Type' -> ClassDecl' -> VarDecl' -> CExtDecl
translateStaticFieldDecl unit ty' (ClassDecl' _ name _) (VarDecl' (VarId' id) init')
    = let ty      = translateType unit (Just ty')
          renamed = VarDecl' (VarId' (name ++ "_" ++ id)) init'
          decl    = translateVarDecl unit [cPointer | isRefType (Just ty')] renamed
       in cDeclExt (cDecl ty [decl])

translateStructAllocator :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateStructAllocator unit classDecl@(ClassDecl' _ name' _)
    = let ty     = translateRefType unit (ClassRefType' (ClassType' [name']))
          name   = cIdent ("allocator_" ++ name')
          alloc  = cVarDeclStat (cDecl ty [(cDeclr cThisIdent [cPointer], Just (cExpInit (cMalloc (cSizeofType ty))))])
          inits  = concatMap (translateFieldInitializer unit) nonStaticFields'
          return = cReturnStat (Just cThis)
          body   = cCompoundStat ([alloc] ++ inits ++ [return])
       in cFunction ty name [cParams [], cPointer] body
    where
        classFields'     = getFields classDecl
        nonStaticFields' = filter (not . isStatic) classFields'

translateFieldInitializer :: CompilationUnit' -> MemberDecl' -> [CBlockItem]
translateFieldInitializer unit (FieldDecl' _ _ decls)
    = map (translateFieldDeclInitializer unit) decls

translateFieldDeclInitializer :: CompilationUnit' -> VarDecl' -> CBlockItem
translateFieldDeclInitializer  unit (VarDecl' (VarId' name') (InitExp' exp'))
    = let exp        = translateExp unit exp'
          name       = cIdent name'
          assignment = cAssign CAssignOp (cMember cThis name) exp
       in cExprStat assignment
    
translateFieldDeclInitializer  unit (VarDecl' (VarId' name') (InitArray' Nothing))
    = trace ("array without initializer not supported") undefined

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

translateStmt :: CompilationUnit' -> Stmt' -> CBlockItem
translateStmt unit (Decl' _ ty' vars') 
    = let ty   = translateType unit (Just ty')
          vars = map (translateVarDecl unit [cPointer | isRefType (Just ty')]) vars'
       in cVarDeclStat (cDecl ty vars)

translateStmt _ Empty'
    = cEmptyStat

translateStmt unit (ExpStmt' exp')
    = let exp = translateExp unit exp'
       in cExprStat exp

translateStmt unit (Assert' exp' error')
    = let exp   = translateExp unit exp'
          error = cString error'
       in cAssertStat exp error 

translateStmt unit (Assume' exp')
    = let exp = translateExp unit exp'
       in cAssumeStat exp

translateStmt _ (Break' _)
    = cEmptyStat

translateStmt _ (Continue' _)
    = cEmptyStat

translateStmt unit (ReturnExp' exp')
    = let exp = translateExp unit exp' 
       in cReturnStat (Just exp)

translateStmt _ Return'
    = cReturnStat Nothing

translateVarDecl :: CompilationUnit' -> [CDerivedDeclr] -> VarDecl' -> (CDeclr, Maybe CInit)
translateVarDecl unit declrs (VarDecl' (VarId' name') init')
    = let name = cIdent name'
          init = translateVarInit unit init'
       in (cDeclr name declrs, init)

translateVarInit :: CompilationUnit' -> VarInit' -> Maybe CInit
translateVarInit unit (InitExp' exp')
    = let exp = translateExp unit exp'
       in Just $ cExpInit exp

translateVarInit _ (InitArray' Nothing)
    = Nothing

translateVarInit unit (InitArray' (Just inits'))
    = cArrayInit <$> mapM (translateVarInit unit) inits'

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

translateType :: CompilationUnit' -> Maybe Type' -> CTypeSpec
translateType _ Nothing = cVoidType

translateType _ (Just (PrimType' ty))
    = case ty of
        BooleanT' -> cBoolType ; ByteT'   -> cByteType  ; ShortT' -> cShortType
        IntT'     -> cIntType  ; LongT'   -> cLongType  ; CharT'  -> cCharType
        FloatT'   -> cFloatType; DoubleT' -> cDoubleType

translateType unit (Just (RefType' ty))
    = translateRefType unit ty
    
translateRefType :: CompilationUnit' -> RefType' -> CTypeSpec
translateRefType _ (ClassRefType' (ClassType' [name']))
    = let name = cIdent name'
       in cStructType name
        
--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

translateExp :: CompilationUnit' -> Exp' -> CExpr
translateExp _ (Lit' lit') 
    = case lit' of
        Int'     value -> cConst (cIntConst    (cInteger value))
        Float'   value -> cConst (cFloatConst  (cFloat value))
        Double'  value -> cConst (cFloatConst  (cFloat value))
        Boolean' True  -> cConst (cIntConst    (cInteger 1))
        Boolean' False -> cConst (cIntConst    (cInteger 0))
        Char'    value -> cConst (cCharConst   (cChar value))
        String'  value -> cConst (cStringConst (cString value))
        Null'          -> cNull

translateExp _ This'
    = cThis

translateExp unit (InstanceCreation' (ClassType' name') args') 
    = let name = cIdent (head name')
          args = map (translateExp unit) args'
       in cCall name args

translateExp unit (ArrayCreate' _ _ _) 
    = trace "array creation in expression unsupported." undefined

translateExp unit (MethodInv' (MethodCall' name' args')) 
    = let name = cIdent (head name')
          args = map (translateExp unit) args'
       in cCall name args

translateExp unit (ArrayAccess' name' [index'])
    = let name  = cIdent name'
          index = translateExp unit index'
       in cIndex name index

translateExp unit (ExpName' [name'])
    = let name = cIdent name'
       in cVar name

translateExp unit (PostIncrement' exp')
    = let exp = translateExp unit exp'
       in cUnary CPostIncOp exp

translateExp unit (PostDecrement' exp')
    = let exp = translateExp unit exp'
       in cUnary CPreDecOp exp

translateExp unit (PreIncrement' exp')
    = let exp = translateExp unit exp'
       in cUnary CPreIncOp exp

translateExp unit (PreDecrement' exp')
    = let exp = translateExp unit exp'
       in cUnary CPostDecOp exp

translateExp unit (PrePlus' exp')
    = let exp = translateExp unit exp'
       in cUnary CPlusOp exp

translateExp unit (PreMinus' exp')
    = let exp = translateExp unit exp'
       in cUnary CMinOp exp

translateExp unit (PreBitCompl' exp')
    = let exp = translateExp unit exp'
       in cUnary CCompOp exp

translateExp unit (PreNot' exp')
    = let exp = translateExp unit exp'
       in cUnary CNegOp exp

translateExp unit (BinOp' exp1' op' exp2')
    = let op   = case op' of
                    Mult'   -> CMulOp; Div'     -> CDivOp; Rem'    -> CRmdOp
                    Add'    -> CAddOp; Sub'     -> CSubOp; LShift' -> CShlOp
                    RShift' -> CShrOp; RRShift' -> CShrOp; LThan'  -> CLeOp
                    GThan'  -> CGrOp ; LThanE'  -> CLeqOp; GThanE' -> CGeqOp
                    Equal'  -> CEqOp ; NotEq'   -> CNeqOp; And'    -> CAndOp
                    Or'     -> COrOp ; Xor'     -> CXorOp; CAnd'   -> CLndOp
                    COr'    -> CLorOp
          exp1 = translateExp unit exp1'
          exp2 = translateExp unit exp2'
       in cBinary op exp1 exp2 

translateExp unit (Cond' guard' exp1' exp2')
    = let guard = translateExp unit guard'
          exp1  = translateExp unit exp1'
          exp2  = translateExp unit exp2'
       in cCond guard exp1 exp2 

translateExp unit (Assign' lhs' op' exp') 
    = let op  = case op' of 
                    EqualA'   -> CAssignOp; MultA'   -> CMulAssOp
                    DivA'     -> CDivAssOp; RemA'    -> CRmdAssOp
                    AddA'     -> CAddAssOp; SubA'    -> CSubAssOp
                    LShiftA'  -> CShlAssOp; RShiftA' -> CShrAssOp
                    RRShiftA' -> CShrAssOp; AndA'    -> CAndAssOp
                    XorA'     -> CXorAssOp; OrA'     -> COrAssOp
          lhs = translateLhs unit lhs'
          exp = translateExp unit exp'
       in cAssign op lhs exp

translateLhs :: CompilationUnit' -> Lhs' -> CExpr
translateLhs _ (Name' [name'])
    = cVar (cIdent name')

translateLhs unit (Field' (PrimaryFieldAccess' exp' field'))
    = let exp   = translateExp unit exp'
          field = cIdent field'
       in cMember exp field

{-translateLhs unit (Field' (ClassFieldAccess' [ty'] field'))
    = cVar (cIdent (ty' ++ "_" ++ field'))

translateLhs _ (Field' (ClassFieldAccess' ty' field'))
    = trace (show ty' ++ "---" ++ show field') undefined-}

--------------------------------------------------------------------------------
-- Auxiliary
--------------------------------------------------------------------------------

thisParam :: Type' -> FormalParam'
thisParam ty = FormalParam' [] ty (VarId' "this")
