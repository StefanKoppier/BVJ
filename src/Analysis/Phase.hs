module Analysis.Phase where

import           Fold
import           Control.Phase
import           Control.Verbosity
import           Analysis.Complete
import           Analysis.Reachability
import           Data.Graph.Inductive.Graph
import           Language.Java.Syntax
import qualified Language.Java.Pretty       as J

--------------------------------------------------------------------------------
-- Main analysis phase
--------------------------------------------------------------------------------

analysisPhase :: Phase CompilationUnit CFG
analysisPhase verbosity program = do 
    newEitherT $ printHeader "2. PROGRAM ANALYSIS"
    let main = getMainMethod program
    newEitherT $ printTitled "Input program" (J.prettyPrint program)
    ast <- syntaxTransformationSubphase verbosity main
    cfg <- controlFlowAnalysisPhase verbosity ast
    reachabilitySubphase verbosity cfg
    
getMainMethod :: CompilationUnit -> Block
getMainMethod (CompilationUnit _ _ 
                (ClassTypeDecl 
                  (ClassDecl _ _ _ _ _ 
                    (ClassBody [MemberDecl (MethodDecl _ _ _ _ _ _ _ 
                      (MethodBody (Just block)))])) : _))
    = block

--------------------------------------------------------------------------------
-- Syntax transformation subphase
--------------------------------------------------------------------------------

syntaxTransformationSubphase :: Subphase Block Block'
syntaxTransformationSubphase verbosity block = do
    newEitherT $ printHeader "2.a syntax transformation"
    transformBlock block

transformBlock :: Block -> PhaseResult Block'
transformBlock = undefined

transformStmt :: Stmt -> PhaseResult Stmt'
transformStmt = foldStmt alg
    where
        alg = ( \ b         -> StmtBlock' <$> transformBlock b
              , \ g s       -> IfThen' g <$> s
              , \ g s1 s2   -> IfThenElse' g <$> s1 <*> s2
              , \ g s       -> While' Nothing g <$> s
              , \ i g u s   -> _
              , \ m t i g s -> unsupported "for"
              ,                return Empty'
              , \ e         -> return $ ExpStmt' e
              , \ g e       -> return $ Assert' g e
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , \ e s   -> unsupported "synchronized"
              , \ b c f -> unsupported "try catch (finally)"
              , \ l s -> labelize (Just l) <$> s
              )
        labelize l (While' _ g s)        = While' l g s 
        labelize l (BasicFor' _ i g u s) = BasicFor' l i g u s 

unsupported :: String -> PhaseResult Stmt'
unsupported = left . UnsupportedSyntax

{-
        alg = (Block -> r, -- StmtBlock
               Exp -> r -> r, -- IfThen
               Exp -> r -> r -> r, -- IfThenElse
               Exp -> r -> r,  -- While
               Maybe ForInit -> Maybe Exp -> Maybe [Exp] -> r -> r,  -- BasicFor
               [Modifier] -> Type -> Ident -> Exp -> r -> r,  -- EnhancedFor
               r, -- Empty
               Exp -> r, -- ExpStmt
               Exp -> Maybe Exp -> r, -- Assert
               Exp -> [SwitchBlock] -> r, -- Switch
               r -> Exp -> r, -- Do
               Maybe Ident -> r, -- Break
               Maybe Ident -> r, -- Continue
               Maybe Exp -> r, -- Return
               Exp -> Block -> r, -- Synchronized
               Exp -> r, -- Throw
               Block -> [Catch] -> Maybe Block -> r, -- Try
               Ident -> r -> r -- Labeled
               )
-}

{-
transformBlock :: Block -> PhaseResult Block'
transformBlock (Block ss) = Block' <$> transformBlockStmts ss

transformBlockStmts :: [BlockStmt] -> PhaseResult BlockStmts'
transformBlockStmts []     = return $ Single' $ BlockStmt' Empty'
transformBlockStmts [s]    = Single' <$> transformBlockStmt s
transformBlockStmts (s:ss) = Seq' <$> transformBlockStmt s <*> transformBlockStmts ss

transformBlockStmt :: BlockStmt -> PhaseResult BlockStmt'
transformBlockStmt (BlockStmt s)        = BlockStmt' <$> transformStmt s
transformBlockStmt (LocalVars ms ty vs) = LocalVars' ms ty <$> mapM transformVarDecl vs
transformBlockStmt s                    = left $ UnsupportedSyntax $ "The statment " ++ show s ++ " is not supported"

transformVarDecl :: VarDecl -> PhaseResult VarDecl'
transformVarDecl (VarDecl name Nothing)     = return $ VarDecl' name Nothing
transformVarDecl (VarDecl name (Just init)) = do
    init' <- transformVarInit init
    return $ VarDecl' name (Just init')

transformVarInit :: VarInit -> PhaseResult VarInit'
transformVarInit (InitExp e)      = return $ InitExp' e
transformVarInit (InitArray init) = InitArray' <$> transformArrayInit init

transformArrayInit :: ArrayInit -> PhaseResult ArrayInit'
transformArrayInit (ArrayInit inits) = ArrayInit' <$> mapM transformVarInit inits

transformStmt :: Stmt -> PhaseResult Stmt'
transformStmt (StmtBlock b)                  = StmtBlock' <$> transformBlock b
transformStmt (IfThen e s)                   = IfThen' e <$> transformStmt s
transformStmt (IfThenElse e s1 s2)           = IfThenElse' e <$> transformStmt s1 <*> transformStmt s2 
transformStmt (Labeled l (While e b))        = While' (Just l) e <$> transformStmt b
transformStmt (While e b)                    = While' Nothing e <$> transformStmt b
transformStmt (Labeled l (BasicFor i g u b)) = BasicFor' (Just l) i g u <$> transformStmt b
transformStmt (BasicFor i g u b)             = BasicFor' Nothing i g u <$> transformStmt b
transformStmt Empty                          = return Empty'
transformStmt (ExpStmt expr)                 = return $ ExpStmt' expr
transformStmt (Assert expr err)              = return $ Assert' expr err
transformStmt (Break i)                      = return $ Break' i
transformStmt (Continue i)                   = return $ Continue' i
-- transformStmt (Return e)                     = return $ Return' e 
transformStmt s                              = left $ UnsupportedSyntax $ "The statment " ++ show s ++ " is not supported"
-}
--------------------------------------------------------------------------------
-- Control Flow Analysis subphase
--------------------------------------------------------------------------------
             
controlFlowAnalysisPhase :: Subphase Block' CFG
controlFlowAnalysisPhase verbosity block = do
    newEitherT $ printHeader "2.b control flow analysis"
    newEitherT $ printTitled "Input block" (show block)
    return $ cfgOfBlock block

--------------------------------------------------------------------------------
-- Reachability Analysis subphase
--------------------------------------------------------------------------------

reachabilitySubphase :: Subphase CFG CFG
reachabilitySubphase verbosity cfg = do
    newEitherT $ printHeader "2.c reachability analysis"
    newEitherT $ printTitled "Input CFG" (prettify cfg)
    let init = (1, NodeStmt Empty')
    return $ reachableFrom init cfg
        