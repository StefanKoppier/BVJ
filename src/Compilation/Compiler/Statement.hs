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

data ScopedStmt 
    = SameScope Stmt'
    | NewScope  [ScopedStmt]

type StatAccumulator = (LocalInformation, ExpAccumulator)

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

translateScopedStmts :: CompilationUnit' -> StatAccumulator -> [ScopedStmt] -> (ExpAccumulator, [CBlockItem])
translateScopedStmts _ (_, expAcc) [] = (expAcc, [])

translateScopedStmts unit acc (SameScope stat:stats)
    = let (acc1 , cStat)   = translateStmtAcc unit acc stat
          (expAcc, cStats) = translateScopedStmts unit acc1 stats
       in (expAcc, cStat : cStats)

translateScopedStmts unit (locals, expAcc) (NewScope stat:stats)
    = let (expAcc1, cStat)  = translateScopedStmts unit (locals, expAcc) stat
          (expAcc2, cStats) = translateScopedStmts unit (locals, expAcc1) stats
       in (expAcc2, cBlockStat (cCompoundStat cStat) : cStats)

translateStmtAcc :: CompilationUnit' -> StatAccumulator -> Stmt' -> (StatAccumulator, CBlockItem)
translateStmtAcc unit acc@((className, localVars), _) (Decl' _ ty vars) 
    = let (cTy, declrs)         = translateType unit (Just ty)
          ((_, expAcc1), cVars) = mapAccumL (translateVarDecl unit declrs) acc vars
          acc1                  = ((className, newLocals ++ localVars), expAcc1)
       in (acc1, cVarDeclStat (cDecl cTy cVars))
    where
        newLocals = namesOfDecls vars

translateStmtAcc _ acc' Empty'
    = (acc', cEmptyStat)

translateStmtAcc unit (locals, expAcc) (ExpStmt' exp)
    = let (expAcc1, cExp) = translateExp unit locals expAcc exp
       in ((locals, expAcc1), cExprStat cExp)

translateStmtAcc unit (locals, expAcc) (Assert' exp error)
    = let (expAcc1, cExp) = translateExp unit locals expAcc exp
          cError          = cString error
       in ((locals, expAcc1), cAssertStat cExp cError)

translateStmtAcc unit (locals, expAcc) (Assume' exp)
    = let (expAcc1, cExp) = translateExp unit locals expAcc exp
       in ((locals, expAcc1), cAssumeStat cExp)

translateStmtAcc _ acc (Break' _)
    = (acc, cEmptyStat)

translateStmtAcc _ acc (Continue' _)
    = (acc, cEmptyStat)

translateStmtAcc unit (locals, expAcc) (Return' exp)
    = let (expAcc1, cExp) = translateMaybeExp unit locals expAcc exp
       in ((locals, expAcc1), cReturnStat cExp)

translateVarDecl :: CompilationUnit' -> [CDerivedDeclr] -> StatAccumulator -> VarDecl' -> (StatAccumulator, (CDeclr, Maybe CInit))
translateVarDecl unit declrs (locals, expAcc) (VarDecl' (VarId' name) init)
    = let cName            = cIdent name
          (expAcc1, cInit) = translateVarInit unit locals expAcc init
       in ((locals, expAcc1), (cDeclr cName declrs, cInit))
