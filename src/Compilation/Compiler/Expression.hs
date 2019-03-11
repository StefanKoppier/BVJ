module Compilation.Compiler.Expression where

import Data.List
import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Type
import Compilation.Compiler.Naming
import Parsing.Syntax
import Parsing.Utility

import Debug.Trace

type ExpTranslationResult a = (ExpAccumulator, a)

type ExpAccumulator = (Int, [CExtDecl])

updateAcc :: [CExtDecl] -> ExpAccumulator -> (ExpAccumulator, Int)
updateAcc newDecls (x, decls) = ((x + 1, newDecls ++ decls), x)

translateExp :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Exp' -> ExpTranslationResult CExpr
translateExp _ _ acc (Lit' lit') 
    = (acc, lit)
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

translateExp _ _ acc This'
    = (acc, thisVar)

translateExp unit locals acc (InstanceCreation' (ClassType' name') args') 
    = let name         = cIdent (head name')
          (acc1, args) = mapAccumL (translateExp unit locals) acc args'
       in (acc1, cCall name args)

translateExp unit locals acc exp@ArrayCreate'{}
    = let (acc1, i)    = updateAcc [call] acc
          (acc2, call) = createArrayNewDecl unit locals i acc1 exp
          name         = cIdent ("new_Array$" ++ show i)
       in (acc2, cCall name [])

translateExp unit locals acc exp@ArrayCreateInit'{}
    = let (acc1, i)    = updateAcc [call] acc
          (acc2, call) = createArrayNewDecl unit locals i acc1 exp
          name         = cIdent ("new_Array$" ++ show i)
       in (acc2, cCall name [])

translateExp unit locals acc (FieldAccess' access)
    = translateFieldAccess unit locals acc access

translateExp unit locals acc (MethodInv' (MethodCall' (pre':[name']) args'))
    | (Just _) <- findClass pre' unit
        = undefined
    | otherwise
        = let name         = cIdent name'
              thisArg      = cVar (cIdent pre')
              (acc1, args) = mapAccumL (translateExp unit locals) acc args'
           in (acc1, cCall name (thisArg : args))

translateExp unit locals acc (MethodInv' (MethodCall' [name'] args')) 
    = let name         = cIdent name'
          (acc1, args) = mapAccumL (translateExp unit locals) acc args'
       in (acc1, cCall name args)

translateExp unit locals acc (MethodInv' (PrimaryMethodCall' exp' name' args'))
    = let (acc1, exp)  = translateExp unit locals acc exp'
          (acc2, args) = mapAccumL (translateExp unit locals) acc1 args'
          name         = cIdent name'
       in (acc2, cCall name (exp : args))

translateExp unit locals acc (ArrayAccess' name' [index'])
    = let name          = cIdent name'
          (acc1, index) = translateExp unit locals acc index'
          array         = cMember (cVar name) arrayElementsName
       in (acc1, cIndex array index)

translateExp unit locals acc (ArrayAccess' name' indices')
    = let name            = cIdent name'
          (acc1, indices) = mapAccumL (translateExp unit locals) acc indices'
          array           = cVar name
          access          = foldl (\ acc index -> cIndex (cMember acc arrayElementsName) index) array indices
         in (acc1, access)

translateExp unit locals acc (ExpName' name')
    = translateExpName unit locals acc name'

translateExp unit locals acc (PostIncrement' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CPostIncOp exp)

translateExp unit locals acc (PostDecrement' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CPreDecOp exp)

translateExp unit locals acc (PreIncrement' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CPreIncOp exp)

translateExp unit locals acc (PreDecrement' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CPostDecOp exp)

translateExp unit locals acc (PrePlus' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CPlusOp exp)

translateExp unit locals acc (PreMinus' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CMinOp exp)

translateExp unit locals acc (PreBitCompl' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CCompOp exp)

translateExp unit locals acc (PreNot' exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cUnary CNegOp exp)

translateExp unit locals acc (BinOp' exp1' op' exp2')
    = let (acc1, exp1) = translateExp unit locals acc exp1'
          (acc2, exp2) = translateExp unit locals acc1 exp2'
       in (acc2, cBinary op exp1 exp2)
    where
        op = case op' of
                Mult'   -> CMulOp; Div'     -> CDivOp; Rem'    -> CRmdOp
                Add'    -> CAddOp; Sub'     -> CSubOp; LShift' -> CShlOp
                RShift' -> CShrOp; RRShift' -> CShrOp; LThan'  -> CLeOp
                GThan'  -> CGrOp ; LThanE'  -> CLeqOp; GThanE' -> CGeqOp
                Equal'  -> CEqOp ; NotEq'   -> CNeqOp; And'    -> CAndOp
                Or'     -> COrOp ; Xor'     -> CXorOp; CAnd'   -> CLndOp
                COr'    -> CLorOp

translateExp unit locals acc (Cond' guard' exp1' exp2')
    = let (acc1, guard) = translateExp unit locals acc guard'
          (acc2, exp1)  = translateExp unit locals acc1 exp1'
          (acc3, exp2)  = translateExp unit locals acc2 exp2'
       in (acc3, cCond guard exp1 exp2)

translateExp unit locals acc (Assign' lhs' op' exp') 
    = let (acc1, lhs) = translateLhs unit locals acc lhs'
          (acc2, exp) = translateExp unit locals acc1 exp'
       in (acc2, cAssign op lhs exp)
    where
        op = case op' of 
                EqualA'   -> CAssignOp; MultA'   -> CMulAssOp
                DivA'     -> CDivAssOp; RemA'    -> CRmdAssOp
                AddA'     -> CAddAssOp; SubA'    -> CSubAssOp
                LShiftA'  -> CShlAssOp; RShiftA' -> CShrAssOp
                RRShiftA' -> CShrAssOp; AndA'    -> CAndAssOp
                XorA'     -> CXorAssOp; OrA'     -> COrAssOp

translateExp unit locals acc e
    = trace (show e) undefined

translateMaybeExp :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Maybe Exp' -> ExpTranslationResult (Maybe CExpr)
translateMaybeExp unit locals acc Nothing
    = (acc, Nothing)
translateMaybeExp unit locals acc (Just exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, Just exp)

translateExpName :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Name' -> ExpTranslationResult CExpr
translateExpName unit (className, locals) acc names'
    -- Case: the name is a local variable.
    | head names' `elem` locals 
        = let names = map (cVar . cIdent) names'
           in (acc, foldl1 cMemberVar names)

    -- Case: the name is a field of the class.
    | Just thisClass <- findClass className unit
    , containsNonStaticFieldWithName thisClass (head names')
        = let names = map (cVar . cIdent) names'
           in (acc, foldl cMemberVar thisVar names)

    -- Case: the name is a class, thus the tail must be a static member.
    | Just classDecl <- findClass (head names') unit
        = translateFieldName unit classDecl acc (tail names')

    -- Case: the name is a field of this class.
    | Just classDecl <- findClass className unit 
        = translateFieldName unit classDecl acc names'

translateFieldName :: CompilationUnit' -> ClassDecl' -> ExpAccumulator -> Name' -> ExpTranslationResult CExpr
translateFieldName unit (ClassDecl' ms name' _) acc (field':names')
    = let field = cVar (createStructName name' field')
          names = fmap (cVar . cIdent) names'
       in (acc, foldl cMemberVar field names)

translateLhs :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Lhs' -> ExpTranslationResult CExpr
translateLhs _ _ acc (Name' [name'])
    = (acc, cVar (cIdent name'))

translateLhs unit locals acc (Field' access')
    = translateFieldAccess unit locals acc access'

{-
translateLhs unit locals acc (Array' (ArrayIndex' array' [index']))
    = let (acc1, array) = translateExp unit acc array'
          (acc2, index) = translateExp unit acc1 index'
       in (acc2, cIndex (cMember array arrayElementsName) index)
-}

translateLhs unit locals acc (Array' (ArrayIndex' array' indices'))
    = let (acc1, array)   = translateExp unit locals acc array'
          (acc2, indices) = mapAccumL (translateExp unit locals) acc1 indices'
          access          = foldl ( \ acc index -> cIndex (cMember acc arrayElementsName) index) array indices
       in (acc2, access)

translateFieldAccess :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> FieldAccess' -> ExpTranslationResult CExpr
translateFieldAccess unit locals acc (PrimaryFieldAccess' exp' field')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, cMember exp (cIdent field'))

--------------------------------------------------------------------------------
-- Array new creation
--------------------------------------------------------------------------------

createArrayNewDecl :: CompilationUnit' -> LocalInformation -> Int -> ExpAccumulator -> Exp' -> ExpTranslationResult CExtDecl
createArrayNewDecl unit locals i acc exp@(ArrayCreate' ty' sizes' _)
    = let name         = cIdent ("new_Array$" ++ show i)
          nameOfTy     = createArrayTypeName dimensions ty'
          returnTy     = cStructType nameOfTy
          (acc1, body) = createArrayConstructorBody unit locals acc exp
          params       = cParams []
       in (acc1, cFunction returnTy name [params, cPointer] body)
    where
        dimensions = length sizes'

createArrayNewDecl unit locals i acc exp@(ArrayCreateInit' ty' dimensions _)
    = let name         = cIdent ("new_Array$" ++ show i)
          nameOfTy     = createArrayTypeName dimensions ty'
          returnTy     = cStructType nameOfTy
          (acc1, body) = createArrayConstructorBody unit locals acc exp
          params       = cParams []
       in (acc1, cFunction returnTy name [params, cPointer] body)

createArrayConstructorBody :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Exp' -> ExpTranslationResult CStat
createArrayConstructorBody unit locals acc exp@(ArrayCreate' ty' sizes' _) 
    = let nameOfTy     = createArrayTypeName dimensions ty'
          thisTy       = cStructType nameOfTy
          thisValue    = cVarDeclStat (cDecl thisTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType thisTy []))))])
          (acc1, init) = createInitForDimension unit locals 0 thisVar ty' sizes' acc Nothing
          return       = cReturnStat (Just thisVar)
       in (acc1, cCompoundStat [thisValue, cBlockStat init, return])
    where
        dimensions = length sizes'

createArrayConstructorBody unit locals acc exp@(ArrayCreateInit' ty' dimensions inits')
    =  let nameOfTy     = createArrayTypeName dimensions ty'
           thisTy       = cStructType nameOfTy
           thisValue    = cVarDeclStat (cDecl thisTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType thisTy []))))])
           (acc1, init) = createInitForDimension unit locals 0 thisVar ty' sizes' acc (Just inits')
           return       = cReturnStat (Just thisVar)
        in (acc1, cCompoundStat [thisValue, cBlockStat init, return])
    where
        sizes' = sizesOfVarInit inits'

sizesOfVarInit :: VarInits' -> [Exp']
sizesOfVarInit []                           = []
sizesOfVarInit (InitExp' _:xs)              = [Lit' (Int' (fromIntegral (1 + length xs)))]
sizesOfVarInit (InitArray' (Just inits):xs) = Lit' (Int' (fromIntegral (1 + length xs))) : sizesOfVarInit inits 

createInitForDimension :: CompilationUnit' -> LocalInformation -> Int -> CExpr -> Type' -> [Exp'] -> ExpAccumulator -> Maybe VarInits' -> ExpTranslationResult CStat
createInitForDimension unit locals depth expr ty [size] acc Nothing
    = let (typeOfElem, dDeclr) = translateType unit (Just ty)
          sizeOfElem           = cSizeofType typeOfElem dDeclr
          (acc1, sizeExpr)     = translateExp unit locals acc size
          setElems             = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength            = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
       in (acc1, cCompoundStat [setLength, setElems])
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName
    
createInitForDimension unit locals depth expr ty (size:sizes) acc inits
    = let nameOfTy          = createArrayTypeName dimensions ty
          typeOfElem        = cStructType nameOfTy
          sizeOfElem        = cSizeofType typeOfElem [cPointer]
          (acc1, sizeExpr) = translateExp unit locals acc size
          setElems          = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength         = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
          nextIdent         = cIdent ("i" ++ show depth)
          nextVar           = cVar nextIdent
          nextExpr          = cIndex elemsMember nextVar
          allocArray        = cExprStat $ cAssign CAssignOp nextExpr (cMalloc (cSizeofType typeOfElem []))
          forInit           = cDecl cIntType [(cDeclr nextIdent [], Just cExpInitZero)]
          forGuard          = cBinary CLeOp nextVar sizeExpr
          forUpdate         = cUnary CPreIncOp nextVar
          (acc2, nextDim)   = createInitForDimension unit locals (depth + 1) nextExpr ty sizes acc1 inits
          forBody           = cCompoundStat [allocArray, cBlockStat nextDim]
          setInner          = cForStat forInit forGuard forUpdate forBody
       in (acc2, cCompoundStat [setLength, setElems, setInner])
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName
        dimensions   = length sizes