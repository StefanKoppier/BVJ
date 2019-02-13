module Parsing.Phase where

import Language.Java.Parser hiding (methodRef)
import Language.Java.Syntax
import Parsing.Fold
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Phase
import Auxiliary.Pretty

--------------------------------------------------------------------------------
-- Parsing phase
--------------------------------------------------------------------------------

parsingPhase :: Phase String CompoundStmt'
parsingPhase args@Arguments{method} content = do
    newEitherT $ printHeader "1. PARSING"
    newEitherT $ printTitled "Input program" content
    case parser compilationUnit content of 
        Left  e       -> left $ ParseError (show e)
        Right program -> 
            case findMethodBody method program of
                Just method -> syntaxTransformationSubphase args method
                Nothing     -> left (MethodNotFound method)

--------------------------------------------------------------------------------
-- Syntax transformation subphase
--------------------------------------------------------------------------------

syntaxTransformationSubphase :: Subphase Block CompoundStmt'
syntaxTransformationSubphase _ block = do
    program <- transformBlock block
    return $ Seq' program (Stmt' Empty')

transformBlock :: Block -> PhaseResult CompoundStmt'
transformBlock (Block [])     = return $ Stmt' Empty'
transformBlock (Block [s])    = transformBlockStmt s
transformBlock (Block (s:ss)) = Seq' <$> transformBlockStmt s <*> transformBlock (Block ss)

transformBlockStmt :: BlockStmt -> PhaseResult CompoundStmt'
transformBlockStmt (BlockStmt s)        = transformStmt s
transformBlockStmt (LocalClass _)       = unsupported "local class"
transformBlockStmt (LocalVars ms ty ds) = (\ ms' ty' -> Stmt' . Decl' ms' ty') 
    <$> transformModifiers ms 
    <*> transformType ty 
    <*> transformVarDecls ty ds

transformModifiers :: [Modifier] -> PhaseResult [Modifier']
transformModifiers ms = mapM (return . const Static') [m | m@Static <- ms]

transformType :: Type -> PhaseResult Type'
transformType (PrimType BooleanT) = return $ PrimType' BooleanT'
transformType (PrimType ByteT)    = return $ PrimType' ByteT'
transformType (PrimType ShortT)   = return $ PrimType' ShortT'
transformType (PrimType IntT)     = return $ PrimType' IntT'
transformType (PrimType LongT)    = return $ PrimType' LongT'
transformType (PrimType CharT)    = return $ PrimType' CharT'
transformType (PrimType FloatT)   = return $ PrimType' FloatT'
transformType (PrimType DoubleT)  = return $ PrimType' DoubleT'
transformType (RefType ty)        = RefType' <$> transformRefType ty

transformRefType :: RefType -> PhaseResult RefType'
transformRefType (ArrayType ty)   = ArrayType' <$> transformType ty
transformRefType (ClassRefType _) = unsupported "class ref type"

transformVarDecls :: Type -> [VarDecl] -> PhaseResult [VarDecl']
transformVarDecls ty = mapM (transformVarDecl ty)

transformVarDecl :: Type -> VarDecl -> PhaseResult VarDecl'
transformVarDecl ty (VarDecl id Nothing)     = VarDecl' <$> transformVarDeclId id <*> defaultInit ty
transformVarDecl _  (VarDecl id (Just init)) = VarDecl' <$> transformVarDeclId id <*> transformVarInit init

transformVarDeclId :: VarDeclId -> PhaseResult VarDeclId'
transformVarDeclId (VarId (Ident n)) = return $ VarId' n
transformVarDeclId (VarDeclArray _)  = unsupported "array typed declaration"

transformVarInit :: VarInit -> PhaseResult VarInit'
transformVarInit (InitExp e)                = InitExp' <$> transformExp e
transformVarInit (InitArray (ArrayInit is)) = InitArray' . Just <$> mapM transformVarInit is

defaultInit :: Type -> PhaseResult VarInit'
defaultInit (PrimType BooleanT) = (return . InitExp' . Lit' . Boolean') False
defaultInit (PrimType ByteT)    = (return . InitExp' . Lit' . Int') 0 
defaultInit (PrimType ShortT)   = (return . InitExp' . Lit' . Int') 0 
defaultInit (PrimType IntT)     = (return . InitExp' . Lit' . Int') 0 
defaultInit (PrimType LongT)    = (return . InitExp' . Lit' . Int') 0 
defaultInit (PrimType CharT)    = (return . InitExp' . Lit' . Char') '\0'
defaultInit (PrimType FloatT)   = (return . InitExp' . Lit' . Float') 0.0
defaultInit (PrimType DoubleT)  = (return . InitExp' . Lit' . Double') 0.0
defaultInit (RefType ty)        = defaultRefInit ty

defaultRefInit :: RefType -> PhaseResult VarInit'
defaultRefInit (ArrayType    _) = (return . InitArray') Nothing
defaultRefInit (ClassRefType _) = unsupported "default class type init"

transformMaybeIdent :: Maybe Ident -> Maybe String
transformMaybeIdent (Just (Ident x)) = Just x
transformMaybeIdent Nothing          = Nothing

transformStmt :: Stmt -> PhaseResult CompoundStmt'
transformStmt = foldStmt alg
    where
        alg :: StmtAlgebra (PhaseResult CompoundStmt')
        alg = (                fmap Block' . transformBlock
              , \ g s       -> IfThenElse' <$> transformExp g <*> s <*> compound Empty'
              , \ g s1 s2   -> IfThenElse' <$> transformExp g <*> s1 <*> s2
              , \ g s       -> While' Nothing <$> transformExp g <*> s
              , \ i g u s   -> do
                    s' <- s
                    i' <- maybe (return emptyStmt) (\ (ForLocalVars ms ty ds) -> (\ ms' ty' -> Stmt' . Decl' ms' ty') <$> transformModifiers ms <*> transformType ty <*> transformVarDecls ty ds) i
                    u' <- maybe (return emptyStmt) (fmap (foldr (\ e -> Seq' (Stmt' $ ExpStmt' e)) emptyStmt) . mapM transformExp) u
                    g' <- maybe ((return . Lit' . Boolean') True) transformExp g
                    let while = While' Nothing g' (Seq' s' u')
                    return $ Block' $ Seq' i' while
              , \ m t i g s -> unsupported "for (iterator)"
              ,                compound Empty'
              ,                fmap (Stmt' . ExpStmt') . transformExp
              , \ g e       -> (\ e' -> Stmt' . Assert' e') <$> transformExp g <*> transformMaybeExp e
              , \ e b       -> unsupported "switch"
              , \ s e       -> unsupported "do"
              ,                compound . Break' . transformMaybeIdent
              ,                compound . Continue' . transformMaybeIdent
              ,                fmap (Stmt' . Return') . transformMaybeExp
              , \ e s       -> unsupported "synchronized"
              , \ e         -> unsupported "throw"
              , \ b c f     -> unsupported "try catch (finally)"
              , \ (Ident l) s -> labelize (Just l) <$> s
              )

        labelize l (While' _ g s) = While' l g s 

transformMaybeExp :: Maybe Exp -> PhaseResult (Maybe Exp')
transformMaybeExp (Just e) = Just <$> transformExp e
transformMaybeExp Nothing  = return Nothing

transformExp :: Exp -> PhaseResult Exp'
transformExp = foldExp alg
    where 
        alg :: ExpAlgebra (PhaseResult Exp')
        alg = ExpAlgebra {
          lit  = fmap Lit' . transformLiteral
        , this = unsupported "this"
        , thisClass = \ _
            -> unsupported "this class"
        , instanceCreation = \ _ _ _ _ 
            -> unsupported "instance creation"
        , qualInstanceCreation = \ _ _ _ _ _ 
            -> unsupported "qual instance creation"
        , arrayCreate = \ ty es 0
            -> ArrayCreate' <$> transformType ty <*> sequence es <*> return 0
        , arrayCreateInit = \ _ _ _
            -> unsupported "array creation"
        , fieldAccess = \ _
            -> unsupported "field access"
        , methodInv = \ _
            -> unsupported "method invocation"
        , arrayAccess = \ (ArrayIndex (ExpName (Name [Ident n])) es)
            -> ArrayAccess' n <$> mapM transformExp es
        , expName = fmap ExpName' . transformName
        , postIncrement = \ e
            -> PostIncrement' <$> e
        , postDecrement = \ e
            -> PostDecrement' <$> e
        , preIncrement = \ e
            -> PreIncrement' <$> e
        , preDecrement = \ e
            -> PreDecrement' <$> e
        , prePlus = \ e
            -> PrePlus' <$> e
        , preMinus = \ e
            -> PreMinus' <$> e
        , preBitCompl = \ e
            -> PreBitCompl' <$> e
        , preNot = \ e
            -> PreNot' <$> e
        , cast = \ _ _
            -> unsupported "cast"
        , binOp = \ e1 op e2
            -> BinOp' <$> e1 <*> transformOp op <*> e2
        , instanceOf = \ _ _
            -> unsupported "instance of"
        , cond = \ g e1 e2
            -> Cond' <$> g <*> e1 <*> e2
        , assign = \ lhs op e
            -> Assign' <$> transformLhs lhs <*> transformAssignOp op <*> e
        , lambda = \ _ _
            -> unsupported "lambda"
        , methodRef = \ _ _
            -> unsupported "method ref"
        }

transformName :: Name -> PhaseResult Name'
transformName (Name ns) = return [n | (Ident n) <- ns]

transformLhs :: Lhs -> PhaseResult Lhs'
transformLhs (NameLhs name) = Name' <$> transformName name
transformLhs (FieldLhs _)   = unsupported "field lhs"
transformLhs (ArrayLhs _)   = unsupported "array lhs"

transformOp:: Op -> PhaseResult Op'
transformOp Mult    = return Mult'
transformOp Div     = return Div'
transformOp Rem     = return Rem'
transformOp Add     = return Add'
transformOp Sub     = return Sub'
transformOp LShift  = return LShift'
transformOp RShift  = return RShift'
transformOp RRShift = return RRShift'
transformOp LThan   = return LThan'
transformOp GThan   = return GThan'
transformOp LThanE  = return LThanE'
transformOp GThanE  = return GThanE'
transformOp Equal   = return Equal'
transformOp NotEq   = return NotEq'
transformOp And     = return And'
transformOp Or      = return Or'
transformOp Xor     = return Xor'
transformOp CAnd    = return CAnd'
transformOp COr     = return COr'

transformAssignOp:: AssignOp -> PhaseResult AssignOp'
transformAssignOp EqualA   = return EqualA'
transformAssignOp MultA    = return MultA'
transformAssignOp DivA     = return DivA'
transformAssignOp RemA     = return RemA'
transformAssignOp AddA     = return AddA'
transformAssignOp SubA     = return SubA'
transformAssignOp LShiftA  = return LShiftA'
transformAssignOp RShiftA  = return RShiftA'
transformAssignOp RRShiftA = return RRShiftA'
transformAssignOp AndA     = return AndA'
transformAssignOp XorA     = return XorA'
transformAssignOp OrA      = return OrA'

transformLiteral :: Literal -> PhaseResult Literal'
transformLiteral (Int     v) = return $ Int'     v
transformLiteral (Float   v) = return $ Float'   (realToFrac v)
transformLiteral (Double  v) = return $ Double'  (realToFrac v)
transformLiteral (Boolean v) = return $ Boolean' v
transformLiteral (Char    v) = return $ Char'    v
transformLiteral (String  v) = return $ String'  v
transformLiteral Null        = return Null'

emptyStmt :: CompoundStmt'
emptyStmt = Stmt' Empty'

compound :: Stmt' -> PhaseResult CompoundStmt'
compound = return . Stmt'

unsupported :: String -> PhaseResult a
unsupported = left . UnsupportedSyntax
