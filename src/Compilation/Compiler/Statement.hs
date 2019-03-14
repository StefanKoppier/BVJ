module Compilation.Compiler.Statement where

import Data.List                       (groupBy, span)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants 
import Compilation.CProgram
import Compilation.Utility
import Compilation.Compiler.Expression
import Compilation.Compiler.Type
import Parsing.Syntax
import Parsing.Utility
import Linearization.Path
import Data.Accumulator

data ScopedStmt 
    = SameScope Stmt'
    | NewScope  [ScopedStmt]

type StatAccumulator a = Accumulator (LocalInformation, (Int, [CExtDecl])) a

runExpAccumulator :: CompilationUnit' -> Exp' -> StatAccumulator CExpr
runExpAccumulator unit exp = do
    (locals, expAcc) <- getAccumulator
    let (expAcc', cExpr) = runAccumulator (translateExp unit locals exp) expAcc
    updateAccumulator (\ (locals, _) -> (locals, expAcc'))
    return cExpr

runMaybeExpAccumulator :: CompilationUnit' -> Maybe Exp' -> StatAccumulator (Maybe CExpr)
runMaybeExpAccumulator _    Nothing    = return Nothing
runMaybeExpAccumulator unit (Just exp) = Just <$> runExpAccumulator unit exp

runTranslateVarInitAccumulator :: CompilationUnit' -> VarInit' -> StatAccumulator (Maybe CInit)
runTranslateVarInitAccumulator unit init = do
    (locals, expAcc) <- getAccumulator
    let (expAcc', cExpr) = runAccumulator (translateVarInit unit locals init) expAcc
    updateAccumulator (\ (locals, _) -> (locals, expAcc'))
    return cExpr

translateStmts :: CompilationUnit' -> LocalInformation -> ProgramPath -> ExpAccumulator CStat
translateStmts unit locals path = do
    let scoped = createScopedStmts 0 (map (\ (s, i) -> (s, depth i)) path)
    expAcc <- getAccumulator
    let (statAcc, stats) = runAccumulator (translateScopedStmts unit scoped) (locals, expAcc)
    updateAccumulator (const $ snd statAcc)
    return $ cCompoundStat stats

createScopedStmts :: Int -> [(Stmt', Int)] -> [ScopedStmt]
createScopedStmts _ [] = []
createScopedStmts x ((stat, y):stats)
    | x == y = SameScope stat : createScopedStmts x stats
    | x < y  = let (hd, tl) = span (\ (_, x') -> x' >= y) stats
                in NewScope (SameScope stat : createScopedStmts y hd)
                    : createScopedStmts x tl
    | x > y  = SameScope stat : createScopedStmts y stats
    
translateScopedStmts :: CompilationUnit' -> [ScopedStmt] -> StatAccumulator [CBlockItem]
translateScopedStmts _ [] = return []

translateScopedStmts unit (SameScope stat:stats) = do
    cStat  <- translateStmtAcc unit stat
    cStats <- translateScopedStmts unit stats
    return $ cStat : cStats

translateScopedStmts unit (NewScope stat:stats) = do
    (locals, _) <- getAccumulator
    cStat <- translateScopedStmts unit stat
    updateAccumulator (\ (_, expAcc) -> (locals, expAcc))
    cStats <- translateScopedStmts unit stats
    return $ cBlockStat (cCompoundStat cStat) : cStats

translateStmtAcc :: CompilationUnit' -> Stmt' -> StatAccumulator CBlockItem
translateStmtAcc unit (Decl' _ ty vars) = do
    let (cTy, declrs) = translateType unit (Just ty)
    cVars <- mapM (translateVarDecl unit declrs) vars
    updateAccumulator (\ ((className, locals), b) -> ((className, newLocals ++ locals), b))
    return $ cVarDeclStat (cDecl cTy cVars)
    where
        newLocals = namesOfDecls vars

translateStmtAcc _ Empty' = return cEmptyStat

translateStmtAcc unit (ExpStmt' exp) = do
    cExp <- runExpAccumulator unit exp
    return $ cExprStat cExp

translateStmtAcc unit (Assert' exp error) = do
    cExp <- runExpAccumulator unit exp
    let cError = cString error
    return $ cAssertStat cExp cError

translateStmtAcc unit (Assume' exp) = do
    cExp <- runExpAccumulator unit exp
    return $ cAssumeStat cExp

translateStmtAcc _ (Break' _)
    = return cEmptyStat

translateStmtAcc _ (Continue' _)
    = return cEmptyStat

translateStmtAcc unit (Return' exp) = do
    cExp <- runMaybeExpAccumulator unit exp
    return $ cReturnStat cExp

translateVarDecl :: CompilationUnit' -> [CDerivedDeclr] -> VarDecl' -> StatAccumulator (CDeclr, Maybe CInit)
translateVarDecl unit declrs (VarDecl' (VarId' name) init) = do
    let cName = cIdent name
    cInit <- runTranslateVarInitAccumulator unit init
    return (cDeclr cName declrs, cInit)
    