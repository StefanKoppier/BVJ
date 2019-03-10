{-
module Compilation.Compiler.Expression where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Parsing.Utility

import Debug.Trace

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
    = thisVar

translateExp unit locals (InstanceCreation' (ClassType' name') args') 
    = let name = cIdent (head name')
          args = map (translateExp unit locals) args'
       in cCall name args

translateExp unit locals (ArrayCreate' ty' sizes' _)
    = let dimensions = length sizes' 
          name       = cIdent ("allocator_" ++ nameOfType ty' ++ concat (replicate dimensions "_Array")concat (replicate dimensions "_Array"))
          size'      = translateExp unit locals (head sizes')
      in cCall name [size']

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
          array = cMember (cVar name) arrayElementsName
       in cIndex array index

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

translateMaybeExp :: CompilationUnit' -> LocalInformation -> Maybe Exp' -> Maybe CExpr
translateMaybeExp unit locals 
    = fmap (translateExp unit locals)

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
           in foldl cMemberVar thisVar names

    -- Case: the name is a class, thus the tail must be a static member.
    | Just classDecl <- findClass (head names') unit
        = translateFieldName unit classDecl (tail names')

    -- Case: the name is a field of this class.
    | Just classDecl <- findClass className unit 
        = translateFieldName unit classDecl names'

translateFieldName :: CompilationUnit' -> ClassDecl' -> Name' -> CExpr
translateFieldName unit (ClassDecl' ms name' _) (field':names')
    = let field = cVar (createStructName name' field')
          names = fmap (cVar . cIdent) names'
       in foldl cMemberVar field names

translateLhs :: CompilationUnit' -> LocalInformation -> Lhs' -> CExpr
translateLhs _ _ (Name' [name'])
    = cVar (cIdent name')

translateLhs unit locals (Field' access')
    = translateFieldAccess unit locals access'

translateLhs unit locals (Array' (ArrayIndex' array' [index']))
    = let array = translateExp unit locals array'
          index = translateExp unit locals index'
       in cIndex (cMember array arrayElementsName) index

translateFieldAccess :: CompilationUnit' -> LocalInformation -> FieldAccess' -> CExpr
translateFieldAccess unit locals (PrimaryFieldAccess' exp' field')
    = cMember (translateExp unit locals exp') (cIdent field')
-}

module Compilation.Compiler.Expression where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Parsing.Utility

import Debug.Trace

type ExpTranslationResult a = ([CExtDecl], a)

translateExp :: CompilationUnit' -> LocalInformation -> Exp' -> ExpTranslationResult CExpr
translateExp _ _ (Lit' lit') 
    = ([], lit)
    where
        lit = case lit' of
                Int'     value -> cConst (cIntConst    (cInteger value))
                Float'   value -> cConst (cFloatConst  (cFloat value))
                Double'  value -> cConst (cFloatConst  (cFloat value))
                Boolean' True  -> cConst (cIntConst    (cInteger 1))
                Boolean' False -> cConst (cIntConst    (cInteger 0))
                Char'    value -> cConst (cCharConst   (cChar value))
                String'  value -> cConst (cStringConst (cString value))
                Null'          -> cNull

translateExp _ _ This'
    = ([], thisVar)

translateExp unit locals (InstanceCreation' (ClassType' name') args') 
    = let name    = cIdent (head name')
          results = map (translateExp unit locals) args'
          args    = map snd results
          decls   = concatMap fst results
       in (decls, cCall name args)

translateExp unit locals exp@(ArrayCreate' ty' sizes' _)
    = let call = createArrayConstructorDecl unit locals exp
          name = cIdent ("new_Array") -- + unique identifier
       in ([call], cCall name [])

{-
translateExp unit locals (ArrayCreateInit' ty' dimensions' inits')
    = undefined
-}

translateExp unit locals (FieldAccess' access)
    = translateFieldAccess unit locals access

translateExp unit locals (MethodInv' (MethodCall' (pre':[name']) args'))
    | (Just _) <- findClass pre' unit
        = undefined
    | otherwise
        = let name    = cIdent name'
              thisArg = cVar (cIdent pre')
              results = ([], thisArg) : map (translateExp unit locals) args'
              args    = map snd results
              decls   = concatMap fst results
           in (decls, cCall name args)

translateExp unit locals (MethodInv' (MethodCall' [name'] args')) 
    = let name    = cIdent name'
          results = map (translateExp unit locals) args'
          args    = map snd results
          decls   = concatMap fst results
       in (decls, cCall name args)

translateExp unit locals (MethodInv' (PrimaryMethodCall' exp' name' args'))
    = let exp     = translateExp unit locals exp'
          results = exp : map (translateExp unit locals) args'
          args    = map snd results
          decls   = concatMap fst results
          name    = cIdent name'
       in (decls, cCall name args)

translateExp unit locals (ArrayAccess' name' [index'])
    = let name           = cIdent name'
          (decls, index) = translateExp unit locals index'
          array          = cMember (cVar name) arrayElementsName
       in (decls, cIndex array index)

translateExp unit locals (ArrayAccess' name' indices')
    = let name    = cIdent name'
          results = map (translateExp unit locals) indices'
          decls   = concatMap fst results
          indices = map snd results
          array   = cVar name
          access  = foldl (\ acc index -> cIndex (cMember acc arrayElementsName) index) array indices
         in (decls, access)

translateExp unit locals (ExpName' name')
    = translateExpName unit locals name'

translateExp unit locals (PostIncrement' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CPostIncOp exp)

translateExp unit locals (PostDecrement' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CPreDecOp exp)

translateExp unit locals (PreIncrement' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CPreIncOp exp)

translateExp unit locals (PreDecrement' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CPostDecOp exp)

translateExp unit locals (PrePlus' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CPlusOp exp)

translateExp unit locals (PreMinus' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CMinOp exp)

translateExp unit locals (PreBitCompl' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CCompOp exp)

translateExp unit locals (PreNot' exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cUnary CNegOp exp)

translateExp unit locals (BinOp' exp1' op' exp2')
    = let op   = case op' of
                    Mult'   -> CMulOp; Div'     -> CDivOp; Rem'    -> CRmdOp
                    Add'    -> CAddOp; Sub'     -> CSubOp; LShift' -> CShlOp
                    RShift' -> CShrOp; RRShift' -> CShrOp; LThan'  -> CLeOp
                    GThan'  -> CGrOp ; LThanE'  -> CLeqOp; GThanE' -> CGeqOp
                    Equal'  -> CEqOp ; NotEq'   -> CNeqOp; And'    -> CAndOp
                    Or'     -> COrOp ; Xor'     -> CXorOp; CAnd'   -> CLndOp
                    COr'    -> CLorOp
          (decls1, exp1) = translateExp unit locals exp1'
          (decls2, exp2) = translateExp unit locals exp2'
       in (decls1 ++ decls2, cBinary op exp1 exp2)

translateExp unit locals (Cond' guard' exp1' exp2')
    = let (decls1, guard) = translateExp unit locals guard'
          (decls2, exp1)  = translateExp unit locals exp1'
          (decls3, exp2)  = translateExp unit locals exp2'
       in (decls1 ++ decls2 ++ decls3, cCond guard exp1 exp2)

translateExp unit locals (Assign' lhs' op' exp') 
    = let op  = case op' of 
                    EqualA'   -> CAssignOp; MultA'   -> CMulAssOp
                    DivA'     -> CDivAssOp; RemA'    -> CRmdAssOp
                    AddA'     -> CAddAssOp; SubA'    -> CSubAssOp
                    LShiftA'  -> CShlAssOp; RShiftA' -> CShrAssOp
                    RRShiftA' -> CShrAssOp; AndA'    -> CAndAssOp
                    XorA'     -> CXorAssOp; OrA'     -> COrAssOp
          (decls1, lhs) = translateLhs unit locals lhs'
          (decls2, exp) = translateExp unit locals exp'
       in (decls1 ++ decls2, cAssign op lhs exp)

translateExp unit locals e
    = trace (show e) undefined

translateMaybeExp :: CompilationUnit' -> LocalInformation -> Maybe Exp' -> ExpTranslationResult (Maybe CExpr)
translateMaybeExp unit locals Nothing
    = ([], Nothing)
translateMaybeExp unit locals (Just exp')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, Just exp)

translateExpName :: CompilationUnit' -> LocalInformation -> Name' -> ExpTranslationResult CExpr
translateExpName unit (className, locals) names'
    -- Case: the name is a local variable.
    | head names' `elem` locals 
        = let names = map (cVar . cIdent) names'
           in ([], foldl1 cMemberVar names)

    -- Case: the name is a field of the class.
    | Just thisClass <- findClass className unit
    , containsNonStaticFieldWithName thisClass (head names')
        = let names = map (cVar . cIdent) names'
           in ([], foldl cMemberVar thisVar names)

    -- Case: the name is a class, thus the tail must be a static member.
    | Just classDecl <- findClass (head names') unit
        = translateFieldName unit classDecl (tail names')

    -- Case: the name is a field of this class.
    | Just classDecl <- findClass className unit 
        = translateFieldName unit classDecl names'

translateFieldName :: CompilationUnit' -> ClassDecl' -> Name' -> ExpTranslationResult CExpr
translateFieldName unit (ClassDecl' ms name' _) (field':names')
    = let field = cVar (createStructName name' field')
          names = fmap (cVar . cIdent) names'
       in ([], foldl cMemberVar field names)

translateLhs :: CompilationUnit' -> LocalInformation -> Lhs' -> ExpTranslationResult CExpr
translateLhs _ _ (Name' [name'])
    = ([], cVar (cIdent name'))

translateLhs unit locals (Field' access')
    = translateFieldAccess unit locals access'

translateLhs unit locals (Array' (ArrayIndex' array' [index']))
    = let (decls1, array) = translateExp unit locals array'
          (decls2, index) = translateExp unit locals index'
       in (decls1 ++ decls2, cIndex (cMember array arrayElementsName) index)

translateLhs unit locals (Array' (ArrayIndex' array' indices'))
    = let (decls1, array) = translateExp unit locals array'
          results         = map (translateExp unit locals) indices'
          decls2          = concatMap fst results
          indices         = map snd results
          access          = foldl ( \ acc index -> cIndex (cMember acc arrayElementsName) index) array indices
       in (decls1 ++ decls2, access)

translateFieldAccess :: CompilationUnit' -> LocalInformation -> FieldAccess' -> ExpTranslationResult CExpr
translateFieldAccess unit locals (PrimaryFieldAccess' exp' field')
    = let (decls, exp) = translateExp unit locals exp'
       in (decls, cMember exp (cIdent field'))

--------------------------------------------------------------------------------
-- Array new creation
--------------------------------------------------------------------------------

createArrayConstructorDecl :: CompilationUnit' -> LocalInformation -> Exp' -> CExtDecl
createArrayConstructorDecl unit locals exp@(ArrayCreate' ty' sizes' _)
    = let name       = cIdent ("new_Array") -- ++ a unique number
          nameOfTy   = createArrayTypeName dimensions ty'
          returnTy   = cStructType nameOfTy
          body       = createArrayConstructorBody unit locals exp
          params     = cParams []
       in cFunction returnTy name [params, cPointer] body   
    where
        dimensions = length sizes'

createArrayConstructorBody :: CompilationUnit' -> LocalInformation -> Exp' -> CStat
createArrayConstructorBody unit locals exp@(ArrayCreate' ty' sizes' _) 
    = let nameOfTy  = createArrayTypeName dimensions ty'
          thisTy    = cStructType nameOfTy
          thisValue = cVarDeclStat (cDecl thisTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType thisTy []))))])
          init      = createInitForDimension unit locals 0 thisVar ty' sizes'
          return    = cReturnStat (Just thisVar)
       in cCompoundStat [thisValue, cBlockStat init, return]
    where
        dimensions = length sizes'

-- TODO: add decls to return type
createInitForDimension :: CompilationUnit' -> LocalInformation -> Int -> CExpr -> Type' -> [Exp'] -> CStat
createInitForDimension unit locals depth expr ty [size]
    = let (typeOfElem, dDeclr) = translateType unit (Just ty)
          sizeOfElem           = cSizeofType typeOfElem dDeclr
          (decls, sizeExpr)    = translateExp unit locals size
          setElems             = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength            = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
       in cCompoundStat [setLength, setElems]
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName

createInitForDimension unit locals depth expr ty (size:sizes)
    = let nameOfTy          = createArrayTypeName dimensions ty
          typeOfElem        = cStructType nameOfTy
          sizeOfElem        = cSizeofType typeOfElem [cPointer]
          (decls, sizeExpr) = translateExp unit locals size
          setElems          = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength         = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
          nextIdent         = cIdent ("i" ++ show depth)
          nextVar           = cVar nextIdent
          nextExpr          = cIndex elemsMember nextVar
          allocArray        = cExprStat $ cAssign CAssignOp nextExpr (cMalloc (cSizeofType typeOfElem []))
          forInit           = cDecl cIntType [(cDeclr nextIdent [], Just cExpInitZero)]
          forGuard          = cBinary CLeOp nextVar sizeExpr
          forUpdate         = cUnary CPreIncOp nextVar
          forBody           = cCompoundStat [allocArray, cBlockStat $ createInitForDimension unit locals (depth + 1) nextExpr ty sizes]
          setInner          = cForStat forInit forGuard forUpdate forBody
       in cCompoundStat [setLength, setElems, setInner]
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName
        dimensions   = length sizes
