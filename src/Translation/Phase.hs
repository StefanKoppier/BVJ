module Translation.Phase(
      translationPhase
) where

import Data.Maybe                  (fromJust)
import Data.List                   (groupBy, sortOn, mapAccumL)
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
        (PathStmtInfo callName scope@(Scope _ _ scopeMember)) = snd . head $ path
        methodName = scopeMember
        methodDecl = fromJust $ getMethod unit scope

translateConstructorCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateConstructorCall unit callName methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          preStats          = cVarDeclStat (cDecl returns [(cDeclr cThisIdent [cPointer], Just (cExpInit (cCall (cIdent ("allocator_" ++ methodName)) [])))])
          postStats         = cReturnStat (Just cThis)
          localInfo         = (scopeClass, paramNames)
          stats             = cBlockStat $ translateStmts unit localInfo (map (transformReturnToReturnThis . fst) path)
          body              = cCompoundStat [preStats, stats, postStats]
       in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope@(Scope _ scopeClass scopeMember)) = snd . head $ path
        methodName   = scopeMember
        paramNames   = namesOfParams methodParams
        methodParams = getParams methodDecl
        methodType   = fromJust (getReturnTypeOfMethod methodDecl)

transformReturnToReturnThis :: Stmt' -> Stmt'
transformReturnToReturnThis Return' = ReturnExp' (ExpName' ["this"])
transformReturnToReturnThis s       = s

translateMethodCall :: CompilationUnit' -> String -> MemberDecl' -> ProgramPath -> CExtDecl
translateMethodCall unit callName methodDecl path
    = let (returns, declrs) = translateType unit methodType
          name              = cIdent callName
          params            = translateParams unit methodParams
          localInfo         = (scopeClass, paramNames)
          body              = translateStmts unit localInfo (map fst path)
      in cFunction returns name (params : declrs) body
    where
        (PathStmtInfo callName scope) = snd . head $ path
        (Scope _ scopeClass scopeMember) = scope
        methodType   = fromJust $ getReturnTypeOfMethod methodDecl
        thisTy       = RefType' . ClassRefType' . ClassType' $ [scopeClass]
        paramNames   = namesOfParams (getParams methodDecl)
        methodParams = if isStatic methodDecl
                            then getParams methodDecl
                            else thisParam thisTy : getParams methodDecl

translateParams :: CompilationUnit' -> FormalParams' -> CDerivedDeclr
translateParams unit params 
    = cParams (map (translateParam unit) params)

translateParam :: CompilationUnit' -> FormalParam' -> CDecl
translateParam unit (FormalParam' _ ty' (VarId' name)) 
    = let (ty, declrs) = translateType unit (Just ty')
          var          = (cDeclr (cIdent name) declrs, Nothing)
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

translateFieldDecl :: CompilationUnit' -> (CTypeSpec, [CDerivedDeclr]) -> VarDecl' -> CDecl
translateFieldDecl _ (ty, declrs) (VarDecl' (VarId' name') _)
    = cDecl ty [(cDeclr (cIdent name') declrs, Nothing)]

translateStaticField :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CExtDecl]
translateStaticField unit classDecl (FieldDecl' _ ty' vars')
    = map (translateStaticFieldDecl unit ty' classDecl) vars'
       
translateStaticFieldDecl :: CompilationUnit' -> Type' -> ClassDecl' -> VarDecl' -> CExtDecl
translateStaticFieldDecl unit ty' (ClassDecl' _ name _) (VarDecl' (VarId' id) init')
    = let (ty, declrs) = translateType unit (Just ty')
          renamed      = VarDecl' (VarId' (name ++ "_" ++ id)) init'
          localInfo    = (name, [])
          decl         = translateVarDecl unit localInfo declrs renamed
       in cDeclExt (cDecl ty [decl])

translateStructAllocator :: CompilationUnit' -> ClassDecl' -> CExtDecl
translateStructAllocator unit classDecl@(ClassDecl' _ name' _)
    = let (ty, declrs) = translateRefType unit (ClassRefType' (ClassType' [name']))
          name         = cIdent ("allocator_" ++ name')
          alloc        = cVarDeclStat (cDecl ty [(cDeclr cThisIdent [cPointer], Just (cExpInit (cMalloc (cSizeofType ty))))])
          inits        = concatMap (translateFieldInitializer unit classDecl) nonStaticFields'
          return       = cReturnStat (Just cThis)
          body         = cCompoundStat ([alloc] ++ inits ++ [return])
       in cFunction ty name (cParams [] : declrs) body
    where
        classFields'     = getFields classDecl
        nonStaticFields' = filter (not . isStatic) classFields'

translateFieldInitializer :: CompilationUnit' -> ClassDecl' -> MemberDecl' -> [CBlockItem]
translateFieldInitializer unit classDecl (FieldDecl' _ _ decls)
    = map (translateFieldDeclInitializer unit classDecl) decls

translateFieldDeclInitializer :: CompilationUnit' -> ClassDecl' -> VarDecl' -> CBlockItem
translateFieldDeclInitializer unit (ClassDecl' _ className' _) (VarDecl' (VarId' name') (InitExp' exp'))
    = let localInfo  = (className', [])
          exp        = translateExp unit localInfo exp'
          name       = cIdent name'
          assignment = cAssign CAssignOp (cMember cThis name) exp
       in cExprStat assignment
    
translateFieldDeclInitializer unit _ (VarDecl' (VarId' name') (InitArray' Nothing))
    = trace ("array without initializer not supported") undefined

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

-- | Local information containing the current class and the local variables declared.
type LocalInformation = (String, [String])

translateStmts :: CompilationUnit' -> LocalInformation -> [Stmt'] -> CStat
translateStmts unit locals 
    = cCompoundStat . snd . mapAccumL (translateStmtAcc unit) locals

translateStmtAcc :: CompilationUnit' -> LocalInformation -> Stmt' -> (LocalInformation, CBlockItem)
translateStmtAcc unit (className, locals) (Decl' _ ty' vars') 
    = let (ty, declrs) = translateType unit (Just ty')
          vars         = map (translateVarDecl unit (className, locals) declrs) vars'
       in ((className, newLocals ++ locals), cVarDeclStat (cDecl ty vars))
    where
        newLocals = namesOfDecls vars'

translateStmtAcc _ locals Empty'
    = (locals, cEmptyStat)

translateStmtAcc unit locals (ExpStmt' exp')
    = let exp = translateExp unit locals exp'
       in (locals, cExprStat exp)

translateStmtAcc unit locals (Assert' exp' error')
    = let exp   = translateExp unit locals exp'
          error = cString error'
       in (locals, cAssertStat exp error)

translateStmtAcc unit locals (Assume' exp')
    = let exp = translateExp unit locals exp'
       in (locals, cAssumeStat exp)

translateStmtAcc _ locals (Break' _)
    = (locals, cEmptyStat)

translateStmtAcc _ locals (Continue' _)
    = (locals, cEmptyStat)

translateStmtAcc unit locals (ReturnExp' exp')
    = let exp = translateExp unit locals exp' 
       in (locals, cReturnStat (Just exp))

translateStmtAcc _ locals Return'
    = (locals, cReturnStat Nothing)

translateVarDecl :: CompilationUnit' -> LocalInformation -> [CDerivedDeclr] -> VarDecl' -> (CDeclr, Maybe CInit)
translateVarDecl unit locals declrs (VarDecl' (VarId' name') init')
    = let name = cIdent name'
          init = translateVarInit unit locals init'
       in (cDeclr name declrs, init)

translateVarInit :: CompilationUnit' -> LocalInformation -> VarInit' -> Maybe CInit
translateVarInit unit locals (InitExp' exp')
    = let exp = translateExp unit locals exp'
       in Just $ cExpInit exp

translateVarInit _ _ (InitArray' Nothing)
    = Nothing

translateVarInit unit locals (InitArray' (Just inits'))
    = cArrayInit <$> mapM (translateVarInit unit locals) inits'

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

translateType :: CompilationUnit' -> Maybe Type' -> (CTypeSpec, [CDerivedDeclr])
translateType _ Nothing = (cVoidType, [])

translateType _ (Just (PrimType' ty'))
    = (ty, [])
    where
        ty = case ty' of
                BooleanT' -> cBoolType ; ByteT'   -> cByteType  
                ShortT'   -> cShortType; IntT'    -> cIntType   
                LongT'    -> cLongType ; CharT'   -> cCharType
                FloatT'   -> cFloatType; DoubleT' -> cDoubleType

translateType unit (Just (RefType' ty))
    = translateRefType unit ty
    
translateRefType :: CompilationUnit' -> RefType' -> (CTypeSpec, [CDerivedDeclr])
translateRefType _ (ClassRefType' (ClassType' [name']))
    = let name = cIdent name'
       in (cStructType name, [cPointer])
        
--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

translateExp :: CompilationUnit' -> LocalInformation -> Exp' -> CExpr
translateExp _ _ (Lit' lit') 
    = case lit' of
        Int'     value -> cConst (cIntConst    (cInteger value))
        Float'   value -> cConst (cFloatConst  (cFloat value))
        Double'  value -> cConst (cFloatConst  (cFloat value))
        Boolean' True  -> cConst (cIntConst    (cInteger 1))
        Boolean' False -> cConst (cIntConst    (cInteger 0))
        Char'    value -> cConst (cCharConst   (cChar value))
        String'  value -> cConst (cStringConst (cString value))
        Null'          -> cNull

translateExp _ _ This'
    = cThis

translateExp unit locals (InstanceCreation' (ClassType' name') args') 
    = let name = cIdent (head name')
          args = map (translateExp unit locals) args'
       in cCall name args

translateExp unit _ (ArrayCreate' _ _ _) 
    = trace "array creation in expression unsupported." undefined

translateExp unit locals (FieldAccess' access)
    = translateFieldAccess unit locals access

translateExp unit locals (MethodInv' (MethodCall' (pre':[name']) args'))
    | (Just _) <- findClass pre' unit
        = undefined
    | otherwise
        = let name    = cIdent name'
              thisArg = cVar (cIdent pre')
              args    = thisArg : map (translateExp unit locals) args'
           in cCall name args 

translateExp unit locals (MethodInv' (MethodCall' [name'] args')) 
    = let name = cIdent name'
          args = map (translateExp unit locals) args'
       in cCall name args

translateExp unit locals (MethodInv' (PrimaryMethodCall' exp' name' args'))
    = let exp  = translateExp unit locals exp'
          args = exp : map (translateExp unit locals) args'
          name = cIdent name'
       in cCall name args

translateExp unit locals (ArrayAccess' name' [index'])
    = let name  = cIdent name'
          index = translateExp unit locals index'
       in cIndex name index

translateExp unit locals (ExpName' name')
    = translateExpName unit locals name'

translateExp unit locals (PostIncrement' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CPostIncOp exp

translateExp unit locals (PostDecrement' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CPreDecOp exp

translateExp unit locals (PreIncrement' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CPreIncOp exp

translateExp unit locals (PreDecrement' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CPostDecOp exp

translateExp unit locals (PrePlus' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CPlusOp exp

translateExp unit locals (PreMinus' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CMinOp exp

translateExp unit locals (PreBitCompl' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CCompOp exp

translateExp unit locals (PreNot' exp')
    = let exp = translateExp unit locals exp'
       in cUnary CNegOp exp

translateExp unit locals (BinOp' exp1' op' exp2')
    = let op   = case op' of
                    Mult'   -> CMulOp; Div'     -> CDivOp; Rem'    -> CRmdOp
                    Add'    -> CAddOp; Sub'     -> CSubOp; LShift' -> CShlOp
                    RShift' -> CShrOp; RRShift' -> CShrOp; LThan'  -> CLeOp
                    GThan'  -> CGrOp ; LThanE'  -> CLeqOp; GThanE' -> CGeqOp
                    Equal'  -> CEqOp ; NotEq'   -> CNeqOp; And'    -> CAndOp
                    Or'     -> COrOp ; Xor'     -> CXorOp; CAnd'   -> CLndOp
                    COr'    -> CLorOp
          exp1 = translateExp unit locals exp1'
          exp2 = translateExp unit locals exp2'
       in cBinary op exp1 exp2 

translateExp unit locals (Cond' guard' exp1' exp2')
    = let guard = translateExp unit locals guard'
          exp1  = translateExp unit locals exp1'
          exp2  = translateExp unit locals exp2'
       in cCond guard exp1 exp2 

translateExp unit locals (Assign' lhs' op' exp') 
    = let op  = case op' of 
                    EqualA'   -> CAssignOp; MultA'   -> CMulAssOp
                    DivA'     -> CDivAssOp; RemA'    -> CRmdAssOp
                    AddA'     -> CAddAssOp; SubA'    -> CSubAssOp
                    LShiftA'  -> CShlAssOp; RShiftA' -> CShrAssOp
                    RRShiftA' -> CShrAssOp; AndA'    -> CAndAssOp
                    XorA'     -> CXorAssOp; OrA'     -> COrAssOp
          lhs = translateLhs unit locals lhs'
          exp = translateExp unit locals exp'
       in cAssign op lhs exp

translateExpName :: CompilationUnit' -> LocalInformation -> Name' -> CExpr
translateExpName unit (className, locals) names'
    -- Case: the name is a local variable.
    | head names' `elem` locals 
        = let names = map (cVar . cIdent) names'
           in foldl1 cMemberVar names

    -- Case: the name is a field of the class.
    | Just thisClass <- findClass className unit
    , containsNonStaticFieldWithName thisClass (head names')
        = let names = map (cVar . cIdent) names'
           in foldl cMemberVar cThis names

    -- Case: the name is a class, thus the tail must be a static member.
    | Just classDecl <- findClass (head names') unit
        = translateFieldName unit classDecl (tail names')

    -- Case: the name is a field of this class.
    | Just classDecl <- findClass className unit 
        = translateFieldName unit classDecl names'

translateFieldName :: CompilationUnit' -> ClassDecl' -> Name' -> CExpr
translateFieldName unit (ClassDecl' ms name' _) (field':names')
    = let field = cVar (cIdent (name' ++ "_" ++ field'))
          names = fmap (cVar . cIdent) names'
       in foldl cMemberVar field names

translateLhs :: CompilationUnit' -> LocalInformation -> Lhs' -> CExpr
translateLhs _ _ (Name' [name'])
    = cVar (cIdent name')

translateLhs unit locals (Field' access')
    = translateFieldAccess unit locals access'

translateFieldAccess :: CompilationUnit' -> LocalInformation -> FieldAccess' -> CExpr
translateFieldAccess unit locals (PrimaryFieldAccess' exp' field')
    = cMember (translateExp unit locals exp') (cIdent field')

--------------------------------------------------------------------------------
-- Auxiliary
--------------------------------------------------------------------------------

thisParam :: Type' -> FormalParam'
thisParam ty = FormalParam' [] ty (VarId' "this")
