module Compilation.Compiler.Statement where

import Data.List                       (mapAccumL, groupBy, span)
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

translateStmts :: CompilationUnit' -> LocalInformation -> ExpAccumulator -> [(Stmt', PathStmtInfo)] -> (ExpAccumulator, CStat)
translateStmts unit locals expAcc stats
    = let scoped          = createScopedStmts 0 (map (\ (s, i) -> (s, depth i)) stats)
          initialAcc      = (locals, expAcc)
          (expAcc1, stat) = translateScopedStmts unit initialAcc scoped
       in (expAcc1, cCompoundStat stat)

createScopedStmts :: Int -> [(Stmt', Int)] -> [ScopedStmt]
createScopedStmts _ [] = []
createScopedStmts x ((stat, y):stats)
    | x == y = SameScope stat : createScopedStmts x stats
    | x < y  = let (hd, tl) = span (\ (_, x') -> x' >= y) stats
                in NewScope (SameScope stat : createScopedStmts y hd)
                    : createScopedStmts x tl
    | x > y  = SameScope stat : createScopedStmts y stats

type Accumulator = (LocalInformation, ExpAccumulator)

translateScopedStmts :: CompilationUnit' -> Accumulator -> [ScopedStmt] -> (ExpAccumulator, [CBlockItem])
translateScopedStmts _ (_, expAcc) [] = (expAcc, [])

translateScopedStmts unit acc' (SameScope stat':stats')
    = let (acc1 , stat)   = translateStmtAcc unit acc' stat'
          (expAcc, stats) = translateScopedStmts unit acc1 stats'
       in (expAcc, stat : stats)

translateScopedStmts unit (locals', expAcc') (NewScope stat':stats')
    = let (expAcc1, stat)  = translateScopedStmts unit (locals', expAcc') stat'
          (expAcc2, stats) = translateScopedStmts unit (locals', expAcc1) stats'
       in (expAcc2, cBlockStat (cCompoundStat stat) : stats)

translateStmtAcc :: CompilationUnit' -> Accumulator -> Stmt' -> (Accumulator, CBlockItem)
translateStmtAcc unit acc'@((className, localVars'), _) (Decl' _ ty' vars') 
    = let (ty, declrs)         = translateType unit (Just ty')
          ((_, expAcc1), vars) = mapAccumL (translateVarDecl unit declrs) acc' vars'
          acc1                 = ((className, newLocals ++ localVars'), expAcc1)
       in (acc1, cVarDeclStat (cDecl ty vars))
    where
        newLocals = namesOfDecls vars'

translateStmtAcc _ acc' Empty'
    = (acc', cEmptyStat)

translateStmtAcc unit (locals', expAcc') (ExpStmt' exp')
    = let (expAcc1, exp) = translateExp unit locals' expAcc' exp'
       in ((locals', expAcc1), cExprStat exp)

translateStmtAcc unit (locals', expAcc') (Assert' exp' error')
    = let (expAcc1, exp) = translateExp unit locals' expAcc' exp'
          error          = cString error'
       in ((locals', expAcc1), cAssertStat exp error)

translateStmtAcc unit (locals', expAcc') (Assume' exp')
    = let (expAcc1, exp) = translateExp unit locals' expAcc' exp'
       in ((locals', expAcc1), cAssumeStat exp)

translateStmtAcc _ acc' (Break' _)
    = (acc', cEmptyStat)

translateStmtAcc _ acc' (Continue' _)
    = (acc', cEmptyStat)

translateStmtAcc unit (locals', expAcc') (Return' exp')
    = let (expAcc1, exp) = translateMaybeExp unit locals' expAcc' exp'
       in ((locals', expAcc1), cReturnStat exp)

translateVarDecl :: CompilationUnit' -> [CDerivedDeclr] -> Accumulator -> VarDecl' -> (Accumulator, (CDeclr, Maybe CInit))
translateVarDecl unit declrs (locals', expAcc') (VarDecl' (VarId' name') init')
    = let name            = cIdent name'
          (expAcc1, init) = translateVarInit unit locals' expAcc' init'
       in ((locals', expAcc1), (cDeclr name declrs, init))
