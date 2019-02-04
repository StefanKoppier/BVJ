module Analysis.Phase where

import           Parsing.Utility
import           Fold
import           Control.Phase
import           Control.Verbosity
import           Analysis.Complete
import           Analysis.Reachability
import           Data.Maybe
import qualified Data.Graph.Inductive.Graph as G
import           Language.Java.Syntax
import qualified Language.Java.Pretty       as J

--------------------------------------------------------------------------------
-- Main analysis phase
--------------------------------------------------------------------------------

analysisPhase :: Phase (String, CompilationUnit) CFG
analysisPhase verbosity (methodName, program) = do 
    newEitherT $ printHeader "2. PROGRAM ANALYSIS"
    case findMethodBody methodName program of
        Just method -> do
            newEitherT $ printTitled "Input program" (J.prettyPrint program)
            ast <- syntaxTransformationSubphase verbosity method
            cfg <- controlFlowAnalysisSubphase verbosity ast
            reachabilityAnalysisSubphase verbosity cfg
        Nothing -> 
            left (MethodNotFound methodName)
    
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

syntaxTransformationSubphase :: Subphase Block CompoundStmt'
syntaxTransformationSubphase verbosity block = do
    newEitherT $ printHeader "2.a syntax transformation"
    transformBlock block

transformBlock :: Block -> PhaseResult CompoundStmt'
transformBlock (Block [])     = return $ Stmt' Empty'
transformBlock (Block (s:ss)) = Seq' <$> transformBlockStmt s <*> transformBlock (Block ss)

transformBlockStmt :: BlockStmt -> PhaseResult CompoundStmt'
transformBlockStmt (BlockStmt s)        = transformStmt s
transformBlockStmt (LocalClass _)       = unsupported "local class"
transformBlockStmt (LocalVars ms ty ds) = return $ Stmt' (VarDecl' ms ty ds)

transformStmt :: Stmt -> PhaseResult CompoundStmt'
transformStmt = foldStmt alg
    where
        alg :: StmtAlgebra (PhaseResult CompoundStmt')
        alg = ( \ b         -> Block' <$> transformBlock b
              , \ g s       -> IfThenElse' g <$> s <*> compound Empty'
              , \ g s1 s2   -> IfThenElse' g <$> s1 <*> s2
              , \ g s       -> While' Nothing g <$> s
              , \ i g u s   -> do
                    s' <- s
                    let i' = maybe empty (\ (ForLocalVars ms ty ds) -> Stmt' $ VarDecl' ms ty ds) i
                    let u' = maybe empty (foldr (\ e -> Seq' (Stmt' $ ExpStmt' e)) empty) u
                    let g' = fromMaybe (Lit $ Boolean True) g
                    let while = While' Nothing g' (Seq' s' u')
                    return $ Block' $ Seq' i' while
              , \ m t i g s -> unsupported "for (iterator)"
              ,                compound Empty'
              ,                compound . ExpStmt'
              , \ g e       -> compound (Assert' g e)
              , \ e b       -> unsupported "switch"
              , \ s e       -> unsupported "do"
              ,                compound . Break'
              ,                compound . Continue'
              ,                compound . Return'
              , \ e s       -> unsupported "synchronized"
              , \ e         -> unsupported "throw"
              , \ b c f     -> unsupported "try catch (finally)"
              , \ l s       -> labelize (Just l) <$> s
              )
        labelize l (While' _ g s)        = While' l g s 

{-
transformStmt :: Stmt -> PhaseResult CompoundStmt'
transformStmt = foldStmt alg
    where
        alg :: StmtAlgebra (PhaseResult CompoundStmt')
        alg = ( \ b         -> Block' <$> transformBlock b
              , \ g s       -> IfThenElse' <$> transformExp g <*> s <*> compound Empty'
              , \ g s1 s2   -> IfThenElse' <$> transformExp g <*> s1 <*> s2
              , \ g s       -> While' Nothing <$> transformExp g <*> s
              , \ i g u s   -> do
                    s' <- s
                    g' <- maybe (return $ LitBool' True) transformExp g
                    let i' = maybe empty (\ (ForLocalVars ms ty ds) -> Stmt' $ VarDecl' ms ty ds) i
                    let u' = maybe empty (foldr (\ e -> Seq' (Stmt' $ ExpStmt' e)) empty) u
                    while <- While' Nothing <$> transformExp g' <*> return (Seq' s' u')
                    return $ Block' $ Seq' i' while
              , \ m t i g s     -> unsupported "for (iterator)"
              ,                    compound Empty'
              , \ e             -> Stmt' . ExpStmt' <$> transformExp e
              , \ g e           -> case e of
                                     Just e  -> (\ g -> Stmt' . Assert' g . Just) <$> transformExp g <*> transformExp e
                                     Nothing -> (\ g -> Stmt' . Assert' g) <$> transformExp g <*> return Nothing
              , \ e b           -> unsupported "switch"
              , \ s e           -> unsupported "do"
              ,                    compound . Break'
              ,                    compound . Continue'
              , \ case Just e   -> Stmt' . Return' . Just <$> transformExp e
                       Nothing  -> compound $ Return' Nothing
              , \ e s           -> unsupported "synchronized"
              , \ e             -> unsupported "throw"
              , \ b c f         -> unsupported "try catch (finally)"
              , \ l s           -> labelize (Just l) <$> s
              )

        labelize l (While' _ g s) = While' l g s 

transformExp :: Exp -> PhaseResult Exp'
transformExp = _ --foldExp alg
    where
        alg = ( \ case
                    Int     v -> return $ LitInt' v
                    Float   v -> return $ LitFloat' v
                    Double  v -> return $ LitDouble' v
                    Boolean v -> return $ LitBool' v
                    Null      -> return LitNull'
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined
              , undefined)
-}
empty :: CompoundStmt'
empty = Stmt' Empty'

compound :: Stmt' -> PhaseResult CompoundStmt'
compound = return . Stmt'

unsupported :: String -> PhaseResult CompoundStmt'
unsupported = left . UnsupportedSyntax

--------------------------------------------------------------------------------
-- Control Flow Analysis subphase
--------------------------------------------------------------------------------
             
controlFlowAnalysisSubphase :: Subphase CompoundStmt' CFG
controlFlowAnalysisSubphase verbosity stat = do
    newEitherT $ printHeader "2.b control flow analysis"
    newEitherT $ printTitled "Input block" (show stat)
    return $ cfgOfStmt stat

--------------------------------------------------------------------------------
-- Reachability Analysis subphase
--------------------------------------------------------------------------------

reachabilityAnalysisSubphase :: Subphase CFG CFG
reachabilityAnalysisSubphase verbosity cfg = do
    newEitherT $ printHeader "2.c reachability analysis"
    newEitherT $ printTitled "Input CFG" (G.prettify cfg)
    let init = (1, Stmt' Empty')
    return $ reachableFrom init cfg
        