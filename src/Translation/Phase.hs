module Translation.Phase(
      translationPhase
    , Program
    , Programs
) where

import Language.Java.Syntax
--import Language.C.Syntax.AST
import Control.Phase
import Linearization.Phase
import Linearization.Path
import Analysis.Complete
import Fold

type Program = String

type Programs = [Program]

translationPhase :: Phase ProgramPaths Programs
translationPhase verbosity paths = do
    newEitherT $ printHeader "4. TRANSLATION"
    return $ map translate paths

translate :: ProgramPath -> Program
translate path = "void main(int argc, char **argv) {\n" 
               ++ concatMap buildStmt path 
               ++ "\n}"

buildStmt :: Stmt' -> String
buildStmt (VarDecl' _ ty vs) = buildType ty ++ " " ++ buildVarDecls vs ++ ";\n" --CCompound [] [CBlockDecl (CDecl [buildType ty]) ()] ()
buildStmt Empty'             = ""
buildStmt (ExpStmt' e)       = buildExp e ++ ";\n"
buildStmt (Assert' e error)  = "__CPROVER_assert(" ++ buildExp e ++ ", " ++ buildMaybeExp error ++ ");\n"
buildStmt (Assume' e)        = "__CPROVER_assume(" ++ buildExp e ++ ");\n"
buildStmt (Break' label)     = ""
buildStmt (Continue' label)  = ""
buildStmt (Return' (Just e)) = "return " ++ buildExp e ++ ";\n"
buildStmt (Return' Nothing)  = "return;\n"

buildType :: Type -> String
buildType (PrimType BooleanT) = "_Bool"
buildType (PrimType ByteT)    = "__int8"
buildType (PrimType ShortT)   = "__int16"
buildType (PrimType IntT)     = "__int32"
buildType (PrimType LongT)    = "__int64"
buildType (PrimType FloatT)   = "float"
buildType (PrimType DoubleT)  = "double"

buildVarDecls :: [VarDecl] -> String
buildVarDecls [VarDecl (VarId (Ident name)) Nothing]            = name 
buildVarDecls [VarDecl (VarId (Ident name)) (Just (InitExp e))] = name ++ " = " ++ buildExp e

buildMaybeExp :: Maybe Exp -> String
buildMaybeExp (Just e) = buildExp e
buildMaybeExp Nothing  = "\"\""

buildExp :: Exp -> String
buildExp = foldExp alg
    where
        alg = defExpAlgebra {
             lit = \case Int x         -> show x
                         Float x       -> show x
                         Double x      -> show x
                         Boolean True  -> "1"
                         Boolean False -> "0"
                         String x      -> show x
                         Null          -> "NULL"
            , expName       = \ (Name [Ident x]) -> x
            , postIncrement = \ e -> e ++ "++"
            , postDecrement = \ e -> e ++ "--"
            , preIncrement  = \ e -> "++" ++ e
            , preDecrement  = \ e -> "--" ++ e
            , prePlus       = \ e -> "+" ++ e
            , preMinus      = \ e -> "-" ++ e
            , preBitCompl   = \ e -> "~" ++ e
            , preNot        = \ e -> "!" ++ e
            , binOp         = \ e1 op e2 -> e1 ++ buildOp op ++ e2
            , assign        = \ lhs op e -> buildLhs lhs ++ buildAssignOp op ++ e
         }

buildOp :: Op -> String
buildOp Mult    = "*" 
buildOp Div     = "/" 
buildOp Rem     = "%" 
buildOp Add     = "+"
buildOp Sub     = "-"
buildOp LShift  = "<<"
buildOp RShift  = ">>"
buildOp RRShift = error "TODO"
buildOp LThan   = "<"
buildOp GThan   = ">"
buildOp LThanE  = ">="
buildOp GThanE  = "<="
buildOp Equal   = "=="
buildOp NotEq   = "!="
buildOp And     = "&&"
buildOp Or      = "||"
buildOp Xor     = "^"
buildOp CAnd    = "&"
buildOp COr     = "|"

buildLhs :: Lhs -> String
buildLhs (NameLhs (Name [Ident var])) = var

buildAssignOp :: AssignOp -> String
buildAssignOp EqualA   = "="
buildAssignOp MultA    = "*="
buildAssignOp DivA     = "/="
buildAssignOp RemA     = "%="
buildAssignOp AddA     = "+="
buildAssignOp SubA     = "-="
buildAssignOp LShiftA  = "<<="
buildAssignOp RShiftA  = ">>="
buildAssignOp RRShiftA = error "TODO"
buildAssignOp AndA     = "&="
buildAssignOp XorA     = "^="
buildAssignOp OrA      = "|="