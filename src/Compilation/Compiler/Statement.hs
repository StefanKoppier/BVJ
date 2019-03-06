module Compilation.Compiler.Statement where

import Data.List                       (mapAccumL)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Expression
import Compilation.Compiler.Type
import Parsing.Syntax
import Parsing.Utility
import Linearization.Path

import Debug.Trace

translateStmts :: CompilationUnit' -> LocalInformation -> [(Stmt', PathStmtInfo)] -> CStat
translateStmts unit locals stats
    = let stats' = map fst stats
       in (cCompoundStat . snd . mapAccumL (translateStmtAcc unit) locals) stats'

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
    = undefined --cArrayInit <$> mapM (translateVarInit unit locals) inits'
