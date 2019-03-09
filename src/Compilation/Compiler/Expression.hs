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
          name       = cIdent ("allocator_" ++ nameOfType ty' ++ concat (replicate dimensions "_Array"))
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
