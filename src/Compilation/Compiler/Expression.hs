module Compilation.Compiler.Expression where

import Data.List
import Data.Maybe
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
translateExp _ _ acc (Lit' lit) 
    = (acc, lit')
    where
        lit' = case lit of
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

translateExp unit locals acc (InstanceCreation' (ClassType' name) args) 
    = let cName         = cIdent (head name)
          (acc1, cArgs) = mapAccumL (translateExp unit locals) acc args
       in (acc1, cCall cName cArgs)

translateExp unit locals acc exp@ArrayCreate'{}
    = let (acc1, i)    = updateAcc [call] acc
          (acc2, call) = createArrayNewDecl unit locals i acc1 exp
          name         = createArrayMethodName i
       in (acc2, cCall name [])

translateExp unit locals acc exp@ArrayCreateInit'{}
    = let (acc1, i)    = updateAcc [call] acc
          (acc2, call) = createArrayNewDecl unit locals i acc1 exp
          name         = createArrayMethodName i
       in (acc2, cCall name [])

translateExp unit locals acc (FieldAccess' access)
    = translateFieldAccess unit locals acc access

translateExp unit locals acc (MethodInv' (MethodCall' (pre:[name]) args))
    | (Just _) <- findClass pre unit
        = trace "translateExp" undefined
    | otherwise
        = let cName         = cIdent name
              thisArg       = cVar (cIdent pre)
              (acc1, cArgs) = mapAccumL (translateExp unit locals) acc args
           in (acc1, cCall cName (thisArg : cArgs))

translateExp unit locals acc (MethodInv' (MethodCall' [name] args)) 
    = let cName         = cIdent name
          (acc1, cArgs) = mapAccumL (translateExp unit locals) acc args
       in (acc1, cCall cName cArgs)

translateExp unit locals acc (MethodInv' (PrimaryMethodCall' exp name args))
    = let (acc1, cExp)  = translateExp unit locals acc exp
          (acc2, cArgs) = mapAccumL (translateExp unit locals) acc1 args
          cName         = cIdent name
       in (acc2, cCall cName (cExp : cArgs))

translateExp unit locals acc (ArrayAccess' name indices)
    = let cName            = cIdent name
          (acc1, cIndices) = mapAccumL (translateExp unit locals) acc indices
          cArray           = cVar cName
          cAccess          = foldl (\ acc -> cIndex (cMember acc arrayElementsName)) cArray cIndices
       in (acc1, cAccess)

translateExp unit locals acc (ExpName' name)
    = translateExpName unit locals acc name

translateExp unit locals acc (PostIncrement' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CPostIncOp cExp)

translateExp unit locals acc (PostDecrement' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CPreDecOp cExp)

translateExp unit locals acc (PreIncrement' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CPreIncOp cExp)

translateExp unit locals acc (PreDecrement' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CPostDecOp cExp)

translateExp unit locals acc (PrePlus' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CPlusOp cExp)

translateExp unit locals acc (PreMinus' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CMinOp cExp)

translateExp unit locals acc (PreBitCompl' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CCompOp cExp)

translateExp unit locals acc (PreNot' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cUnary CNegOp cExp)

translateExp unit locals acc (BinOp' exp1 op exp2)
    = let (acc1, cExp1) = translateExp unit locals acc exp1
          (acc2, cExp2) = translateExp unit locals acc1 exp2
       in (acc2, cBinary cOp cExp1 cExp2)
    where
        cOp = case op of
                Mult'   -> CMulOp; Div'     -> CDivOp; Rem'    -> CRmdOp
                Add'    -> CAddOp; Sub'     -> CSubOp; LShift' -> CShlOp
                RShift' -> CShrOp; RRShift' -> CShrOp; LThan'  -> CLeOp
                GThan'  -> CGrOp ; LThanE'  -> CLeqOp; GThanE' -> CGeqOp
                Equal'  -> CEqOp ; NotEq'   -> CNeqOp; And'    -> CAndOp
                Or'     -> COrOp ; Xor'     -> CXorOp; CAnd'   -> CLndOp
                COr'    -> CLorOp

translateExp unit locals acc (Cond' guard exp1 exp2)
    = let (acc1, cGuard) = translateExp unit locals acc guard
          (acc2, cExp1)  = translateExp unit locals acc1 exp1
          (acc3, cExp2)  = translateExp unit locals acc2 exp2
       in (acc3, cCond cGuard cExp1 cExp2)

translateExp unit locals acc (Assign' lhs op exp) 
    = let (acc1, cLhs) = translateLhs unit locals acc lhs
          (acc2, cExp) = translateExp unit locals acc1 exp
       in (acc2, cAssign cOp cLhs cExp)
    where
        cOp = case op of 
                EqualA'   -> CAssignOp; MultA'   -> CMulAssOp
                DivA'     -> CDivAssOp; RemA'    -> CRmdAssOp
                AddA'     -> CAddAssOp; SubA'    -> CSubAssOp
                LShiftA'  -> CShlAssOp; RShiftA' -> CShrAssOp
                RRShiftA' -> CShrAssOp; AndA'    -> CAndAssOp
                XorA'     -> CXorAssOp; OrA'     -> COrAssOp

translateExp unit locals acc e
    = trace "translateExp missing case" undefined

translateMaybeExp :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Maybe Exp' -> ExpTranslationResult (Maybe CExpr)
translateMaybeExp unit locals acc Nothing
    = (acc, Nothing)
translateMaybeExp unit locals acc (Just exp')
    = let (acc1, exp) = translateExp unit locals acc exp'
       in (acc1, Just exp)

translateExpName :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Name' -> ExpTranslationResult CExpr
translateExpName unit (className, locals) acc names
    -- Case: the name is a local variable.
    | head names `elem` locals 
        = let cNames = map (cVar . cIdent) names
           in (acc, foldl1 cMemberVar cNames)

    -- Case: the name is a field of the class.
    | Just thisClass <- findClass className unit
    , containsNonStaticFieldWithName thisClass (head names)
        = let cNames = map (cVar . cIdent) names
           in (acc, foldl cMemberVar thisVar cNames)

    -- Case: the name is a class, thus the tail must be a static member.
    | Just classDecl <- findClass (head names) unit
        = translateFieldName unit classDecl acc (tail names)

    -- Case: the name is a field of this class.
    | Just classDecl <- findClass className unit 
        = translateFieldName unit classDecl acc names

translateFieldName :: CompilationUnit' -> ClassDecl' -> ExpAccumulator -> Name' -> ExpTranslationResult CExpr
translateFieldName unit (ClassDecl' _ name _) acc (field:names)
    = let cField = cVar (createStructName name field)
          cNames = fmap (cVar . cIdent) names
       in (acc, foldl cMemberVar cField cNames)

translateLhs :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Lhs' -> ExpTranslationResult CExpr
translateLhs _ _ acc (Name' [name])
    = (acc, cVar (cIdent name))

translateLhs unit locals acc (Field' access)
    = translateFieldAccess unit locals acc access

translateLhs unit locals acc (Array' (ArrayIndex' array indices))
    = let (acc1, cArray)   = translateExp unit locals acc array
          (acc2, cIndices) = mapAccumL (translateExp unit locals) acc1 indices
          cAccess          = foldl ( \ acc -> cIndex (cMember acc arrayElementsName)) cArray cIndices
       in (acc2, cAccess)

translateFieldAccess :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> FieldAccess' -> ExpTranslationResult CExpr
translateFieldAccess unit locals acc (PrimaryFieldAccess' exp field)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, cMember cExp (cIdent field))

-- TODO: maybe an unsafe fromJust.
translateVarInits :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> VarInits' -> (ExpAccumulator, CInit)
translateVarInits unit locals acc inits
    = let (acc1, cInits) = mapAccumL (translateVarInit unit locals) acc inits
       in (acc1, cArrayInit (map fromJust cInits))

translateVarInit :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> VarInit' -> (ExpAccumulator, Maybe CInit)
translateVarInit unit locals acc (InitExp' exp)
    = let (acc1, cExp) = translateExp unit locals acc exp
       in (acc1, Just $ cExpInit cExp)

translateVarInit _ _ acc (InitArray' Nothing)
    = (acc, Nothing)

translateVarInit unit locals expAcc (InitArray' (Just inits))
    = let (acc1, cInits) = translateVarInits unit locals expAcc inits
       in (acc1, Just cInits)

--------------------------------------------------------------------------------
-- Array new creation
--------------------------------------------------------------------------------

createArrayNewDecl :: CompilationUnit' -> LocalInformation -> Int -> ExpAccumulator -> Exp' -> ExpTranslationResult CExtDecl
createArrayNewDecl unit locals i acc exp
    = let name         = createArrayMethodName i
          nameOfTy     = createArrayTypeName dimensions ty
          returnTy     = cStructType nameOfTy
          (acc1, body) = createArrayConstructorBody unit locals acc exp
          params       = cParams []
       in (acc1, cFunction returnTy name [params, cPointer] body)
    where
        (ty, dimensions)
            | (ArrayCreateInit' ty' dimensions' _) <- exp
                = (ty', dimensions')
            | (ArrayCreate' ty' sizes' _) <- exp
                = (ty', length sizes')

createArrayConstructorBody :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> Exp' -> ExpTranslationResult CStat
createArrayConstructorBody unit locals acc (ArrayCreate' ty sizes _) 
    = let nameOfTy     = createArrayTypeName dimensions ty
          thisTy       = cStructType nameOfTy
          thisValue    = cVarDeclStat (cDecl thisTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType thisTy []))))])
          (acc1, init) = createInitForDimension unit locals 0 thisVar ty sizes acc Nothing
          return       = cReturnStat (Just thisVar)
       in (acc1, cCompoundStat [thisValue, cBlockStat init, return])
    where
        dimensions = length sizes

createArrayConstructorBody unit locals acc (ArrayCreateInit' ty dimensions inits)
    =  let nameOfTy          = createArrayTypeName dimensions ty
           thisTy            = cStructType nameOfTy
           cSizes            = map (snd . translateExp unit ("", []) (0, [])) sizes
           (acc1, initValue) = createArrayInitValue unit locals ty cSizes acc inits
           thisValue         = cVarDeclStat (cDecl thisTy [(cDeclr thisName [cPointer], Just (cExpInit (cMalloc (cSizeofType thisTy []))))])
           (acc2, init)      = createInitForDimension unit locals 0 thisVar ty sizes acc1 (Just inits)
           return            = cReturnStat (Just thisVar)
        in (acc2, cCompoundStat [initValue, thisValue, cBlockStat init, return])
    where
        sizes = sizesOfVarInit inits

createArrayInitValue :: CompilationUnit' -> LocalInformation -> Type' -> [CExpr] -> ExpAccumulator -> VarInits' -> ExpTranslationResult CBlockItem
createArrayInitValue unit locals ty sizes acc inits
    = let (cTy, tydDeclr) = translateType unit (Just ty)
          dDeclrs         = map cArray sizes ++ tydDeclr
          declr           = cDeclr (cIdent "initial") dDeclrs
          (acc1, init) = translateVarInits unit locals acc inits
       in (acc1, cVarDeclStat (cDecl cTy [(declr, Just init)]))

createInitForDimension :: CompilationUnit' -> LocalInformation -> Int -> CExpr -> Type' -> [Exp'] -> ExpAccumulator -> Maybe VarInits' -> ExpTranslationResult CStat
createInitForDimension unit locals depth expr ty [size] acc inits
    = let (typeOfElem, dDeclr) = translateType unit (Just ty)
          sizeOfElem           = cSizeofType typeOfElem dDeclr
          (acc1, sizeExpr)     = translateExp unit locals acc size
          setElems             = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength            = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
          setInitial           = maybe [] (const [createElemAssignment expr sizeExpr depth]) inits
       in (acc1, cCompoundStat (setLength : setElems : setInitial))
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName
    
createInitForDimension unit locals depth expr ty (size:sizes) acc inits
    = let nameOfTy         = createArrayTypeName dimensions ty
          typeOfElem       = cStructType nameOfTy
          sizeOfElem       = cSizeofType typeOfElem [cPointer]
          (acc1, sizeExpr) = translateExp unit locals acc size
          setElems         = cExprStat $ cAssign CAssignOp elemsMember (cCalloc sizeExpr sizeOfElem)
          setLength        = cExprStat $ cAssign CAssignOp lengthMember sizeExpr
          nextIdent        = cIdent ("i" ++ show depth)
          nextVar          = cVar nextIdent
          nextExpr         = cIndex elemsMember nextVar
          allocArray       = cExprStat $ cAssign CAssignOp nextExpr (cMalloc (cSizeofType typeOfElem []))
          forInit          = cDecl cIntType [(cDeclr nextIdent [], Just cExpInitZero)]
          forGuard         = cBinary CLeOp nextVar sizeExpr
          forUpdate        = cUnary CPreIncOp nextVar
          (acc2, nextDim)  = createInitForDimension unit locals (depth + 1) nextExpr ty sizes acc1 inits
          forBody          = cCompoundStat [allocArray, cBlockStat nextDim]
          setInner         = cForStat forInit forGuard forUpdate forBody
       in (acc2, cCompoundStat [setLength, setElems, setInner])
    where
        lengthMember = cMember expr arrayLengthName
        elemsMember  = cMember expr arrayElementsName
        dimensions   = length sizes

createElemAssignment :: CExpr -> CExpr -> Int -> CBlockItem
createElemAssignment expr size depth
    = let forVarIdent          = cIdent ("i" ++ show depth)
          forVar               = cVar forVarIdent
          forInit              = cDecl cIntType [(cDeclr forVarIdent [], Just cExpInitZero)]
          forGuard             = cBinary CLeOp forVar size
          forUpdate            = cUnary CPreIncOp forVar
          initialVar           = cVar (cIdent "initial")
          indices              = map (\ i -> cVar (cIdent ("i" ++ show i))) [0..depth]
          initValue            = foldl cIndex initialVar indices
          initTarget           = cIndex (cMember expr arrayElementsName) forVar
          (CBlockStmt forBody) = cExprStat (cAssign CAssignOp initTarget initValue)
       in cForStat forInit forGuard forUpdate forBody