module Compilation.Compiler.Statement where

import Data.List                       (mapAccumL, groupBy, span)
import Data.Function                   (on)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Expression
import Compilation.Compiler.Type
import Parsing.Syntax
import Parsing.Utility
import Linearization.Path

import Auxiliary.Pretty
import Parsing.Pretty
import Debug.Trace

data ScopedStmt 
    = SameScope Stmt'
    | NewScope  [ScopedStmt]

translateStmts :: CompilationUnit' -> LocalInformation -> [(Stmt', PathStmtInfo)] -> CStat
translateStmts unit locals stats
    = let scoped = createScopedStmts 0 (map (\ (s, i) -> (s, depth i)) stats)
       in cCompoundStat (translateScopedStmts unit locals scoped)

createScopedStmts :: Int -> [(Stmt', Int)] -> [ScopedStmt]
createScopedStmts _ [] = []
createScopedStmts x ((stat, y):stats)
    | x == y = SameScope stat : createScopedStmts x stats
    | x < y  = let (hd, tl) = span (\ (s, x') -> x' == y) stats
                in NewScope (SameScope stat : createScopedStmts y hd)
                   : createScopedStmts x tl
    | x > y  = SameScope stat : createScopedStmts y stats

translateScopedStmts :: CompilationUnit' -> LocalInformation -> [ScopedStmt] -> [CBlockItem]
translateScopedStmts _ _ [] = []

translateScopedStmts unit locals' (SameScope stat':stats)
    = let (locals, stat) = translateStmtAcc unit locals' stat'
       in stat : translateScopedStmts unit locals stats

translateScopedStmts unit locals' (NewScope stat':stats)
    = let stat = translateScopedStmts unit locals' stat'
       in cBlockStat (cCompoundStat stat) : translateScopedStmts unit locals' stats

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
