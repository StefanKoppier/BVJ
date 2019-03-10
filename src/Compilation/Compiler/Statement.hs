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

translateStmts :: CompilationUnit' -> LocalInformation -> [(Stmt', PathStmtInfo)] -> ([CExtDecl], CStat)
translateStmts unit locals stats
    = let scoped        = createScopedStmts 0 (map (\ (s, i) -> (s, depth i)) stats)
          (decls, stat) = translateScopedStmts unit (locals, []) scoped
       in (decls, cCompoundStat stat)

createScopedStmts :: Int -> [(Stmt', Int)] -> [ScopedStmt]
createScopedStmts _ [] = []
createScopedStmts x ((stat, y):stats)
    | x == y = SameScope stat : createScopedStmts x stats
    | x < y  = let (hd, tl) = span (\ (s, x') -> x' >= y) stats
                in NewScope (SameScope stat : createScopedStmts y hd)
                    : createScopedStmts x tl
    | x > y  = SameScope stat : createScopedStmts y stats

type Accumulator = (LocalInformation, [CExtDecl])

translateScopedStmts :: CompilationUnit' -> Accumulator -> [ScopedStmt] -> ([CExtDecl], [CBlockItem])
translateScopedStmts _ (_, decls) [] = (decls, [])

translateScopedStmts unit acc' (SameScope stat':stats')
    = let (acc1 , stat)  = translateStmtAcc unit acc' stat'
          (decls, stats) = translateScopedStmts unit acc1 stats'
       in (decls, stat : stats)

translateScopedStmts unit acc' (NewScope stat':stats')
    = let (decls1, stat)  = translateScopedStmts unit acc' stat'
          (decls2, stats) = translateScopedStmts unit acc' stats'
       in (decls1 ++ decls2, cBlockStat (cCompoundStat stat) : stats)

{-
translateScopedStmts :: CompilationUnit' -> LocalInformation -> [ScopedStmt] -> [CBlockItem]
translateScopedStmts _ _ [] = []

translateScopedStmts unit locals' (SameScope stat':stats)
    = let (locals, stat) = translateStmtAcc unit locals' stat'
       in stat : translateScopedStmts unit locals stats

translateScopedStmts unit locals' (NewScope stat':stats)
    = let stat = translateScopedStmts unit locals' stat'
       in cBlockStat (cCompoundStat stat) : translateScopedStmts unit locals' stats
-}

translateStmtAcc :: CompilationUnit' -> Accumulator -> Stmt' -> (Accumulator, CBlockItem)
translateStmtAcc unit acc'@((className, localVars'), decls') (Decl' _ ty' vars') 
    = let (ty, declrs) = translateType unit (Just ty')
          results      = map (translateVarDecl unit acc' declrs) vars'
          decls        = concatMap (snd . fst) results
          vars         = map snd results
          acc          = ((className, newLocals ++ localVars'), decls' ++ decls)
       in (acc, cVarDeclStat (cDecl ty vars))
    where
        newLocals = namesOfDecls vars'

translateStmtAcc _ acc' Empty'
    = (acc', cEmptyStat)

translateStmtAcc unit (locals', decls') (ExpStmt' exp')
    = let (decls, exp) = translateExp unit locals' exp'
       in ((locals', decls' ++ decls), cExprStat exp)

translateStmtAcc unit (locals', decls') (Assert' exp' error')
    = let (decls, exp) = translateExp unit locals' exp'
          error        = cString error'
       in ((locals', decls' ++ decls), cAssertStat exp error)

translateStmtAcc unit (locals', decls') (Assume' exp')
    = let (decls, exp) = translateExp unit locals' exp'
       in ((locals', decls' ++ decls), cAssumeStat exp)

translateStmtAcc _ acc' (Break' _)
    = (acc', cEmptyStat)

translateStmtAcc _ acc' (Continue' _)
    = (acc', cEmptyStat)

translateStmtAcc unit (locals', decls') (Return' exp')
    = let (decls, exp) = translateMaybeExp unit locals' exp'
       in ((locals', decls' ++ decls), cReturnStat exp)

translateVarDecl :: CompilationUnit' -> Accumulator -> [CDerivedDeclr] -> VarDecl' -> (Accumulator, (CDeclr, Maybe CInit))
translateVarDecl unit acc' declrs (VarDecl' (VarId' name') init')
    = let name        = cIdent name'
          (acc, init) = translateVarInit unit acc' init'
       in (acc, (cDeclr name declrs, init))

translateVarInit :: CompilationUnit' -> Accumulator -> VarInit' -> (Accumulator, Maybe CInit)
translateVarInit unit (locals', decls') (InitExp' exp')
    = let (decls, exp) = translateExp unit locals' exp'
       in ((locals', decls' ++ decls), Just $ cExpInit exp)

translateVarInit _ acc (InitArray' Nothing)
    = (acc, Nothing)

translateVarInit unit acc (InitArray' (Just inits'))
    = undefined --cArrayInit <$> mapM (translateVarInit unit locals) inits'