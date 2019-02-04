module Translation.Phase(
    translationPhase
) where

import Language.Java.Syntax
import Fold
import Control.Phase
import Linearization.Phase
import Analysis.Complete

translationPhase :: Phase ProgramPaths [String]
translationPhase verbosity paths = do
    newEitherT $ printHeader "4. TRANSLATION"
    return [] -- $ map translate paths
{-
translate :: ProgramPath -> String
translate = concatMap translateStmt

translateStmt :: Stmt' -> PhaseResult String
--translateStmt (VarDecl' _ ty vs) = ""
--translateStmt Empty'             = ""
translateStmt (Assert' exp _) = do
    exp' <- translateExp exp
    return $ "__CPROVER_assert(" ++ exp' ++ "\"\");"

translateStmt (Assume' exp _) = do
    exp' <- translateExp exp
    return $ "__CPROVER_assume(" ++ exp' ++ "\"\");"

translateExp :: Exp' -> PhaseResult String
translateExp 
    = undefined {-foldExp alg
    where
        alg :: ExpAlgebra (PhaseResult String)
        alg = ( \ l -> unsupported "literal"
              , \ c -> unsupported "class literal"
              ,        unsupported "this"
              , \ n -> unsupported "this class"
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

unsupported :: String -> PhaseResult String
unsupported left . UnsupportedSyntax-}

      {-  alg = lit :: Literal -> r
        , classLit :: Maybe Type -> r
        , this :: r
        , thisClass :: Name -> r
        , instanceCreation :: [TypeArgument] -> TypeDeclSpecifier -> [Argument] -> Maybe ClassBody -> r
       
        , qualInstanceCreation :: r -> [TypeArgument] -> Ident -> [Argument] -> Maybe ClassBody -> r
        , arrayCreate :: Type -> [r] -> Int -> r
        , arrayCreateInit :: Type -> Int -> ArrayInit -> r
        , fieldAccess :: FieldAccess -> r
        , methodInv :: MethodInvocation -> r
        
        , arrayAccess :: ArrayIndex -> r
        , expName :: Name -> r
        , postIncrement :: r -> r
        , postDecrement :: r -> r
        , preIncrement :: r -> r
        
        , preDecrement :: r -> r
        , prePlus :: r -> r
        , preMinus :: r -> r
        , preBitCompl :: r -> r
        , preNot :: r -> r
        
        , cast :: Type -> r -> r
        , binOp :: r -> Op -> r -> r
        , instanceOf :: r -> RefType -> r
        , cond :: r -> r -> r -> r
        , assign :: Lhs -> AssignOp -> r -> r
        
        , lambda :: LambdaParams -> LambdaExpression -> r
        , methodRef :: Name -> Ident -> r-}

-}