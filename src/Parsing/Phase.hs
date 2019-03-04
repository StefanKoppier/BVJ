module Parsing.Phase where

import Language.Java.Parser hiding (methodRef)
import Language.Java.Syntax
import Language.Java.Pretty (prettyPrint)
import Language.Java.Fold
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Phase
import Auxiliary.Pretty

import Debug.Trace

--------------------------------------------------------------------------------
-- Parsing phase
--------------------------------------------------------------------------------

parsingPhase :: Phase String CompilationUnit'
parsingPhase args content = do
    newEitherT $ printHeader "1. PARSING"
    newEitherT $ printTitled "Input program" content
    case parser compilationUnit content of 
        Right program -> syntaxTransformationSubphase args program
        Left  e       -> parsingError (show e)

--------------------------------------------------------------------------------
-- Syntax transformation subphase
--------------------------------------------------------------------------------

syntaxTransformationSubphase :: Subphase CompilationUnit CompilationUnit'
syntaxTransformationSubphase _ = transformCompilationUnit

transformCompilationUnit :: CompilationUnit -> PhaseResult CompilationUnit'
transformCompilationUnit (CompilationUnit p _ dls) 
    = CompilationUnit' <$> transformPackageDecl p <*> mapM transformTypeDecl dls

transformPackageDecl :: Maybe PackageDecl -> PhaseResult (Maybe Name')
transformPackageDecl Nothing                = pure Nothing
transformPackageDecl (Just (PackageDecl n)) = Just <$> transformName n

transformTypeDecl :: TypeDecl -> PhaseResult TypeDecl'
transformTypeDecl (ClassTypeDecl cls)   = ClassTypeDecl' <$> transformClassDecl cls
transformTypeDecl (InterfaceTypeDecl _) = syntacticalError "interface declaration"
    
transformClassDecl :: ClassDecl -> PhaseResult ClassDecl'
transformClassDecl (ClassDecl ms (Ident n) [] Nothing [] (ClassBody ds)) 
    = ClassDecl' <$> transformModifiers ms <*> pure n <*> mapM transformDecl ds
transformClassDecl EnumDecl{}
    = syntacticalError "enum declaration"
transformClassDecl ClassDecl{}                              
    = syntacticalError "inheritance or generics"

transformDecl :: Decl -> PhaseResult Decl'
transformDecl (MemberDecl m) = MemberDecl' <$> transformMemberDecl m
transformDecl (InitDecl _ _) = syntacticalError "initializer declarations"

transformMemberDecl :: MemberDecl -> PhaseResult MemberDecl'
transformMemberDecl (FieldDecl ms ty vs) 
    = FieldDecl' <$> transformModifiers ms <*> transformType ty <*> transformVarDecls ty vs
transformMemberDecl (MethodDecl ms [] ty (Ident n) ps [] _ b)
    = MethodDecl' <$> transformModifiers ms <*> transformMaybeType ty <*> pure n <*> transformParams ps <*> transformMethodBody b
transformMemberDecl MethodDecl{}
    = syntacticalError "generics or exception"
transformMemberDecl (ConstructorDecl ms [] (Ident n) ps [] b)
    = ConstructorDecl' <$> transformModifiers ms <*> pure n <*> transformParams ps <*> transformConstructorBody b
transformMemberDecl ConstructorDecl{}
    = syntacticalError "generics or exception"
transformMemberDecl (MemberClassDecl _)
    = syntacticalError "nested class declaration"
transformMemberDecl (MemberInterfaceDecl _)
    = syntacticalError "nested interface declaration"

transformParams :: [FormalParam] -> PhaseResult [FormalParam']
transformParams = mapM transformParam

transformParam :: FormalParam -> PhaseResult FormalParam'
transformParam (FormalParam ms ty False id) 
    = FormalParam' <$> transformModifiers ms <*> transformType ty <*> transformVarDeclId id
transformParam (FormalParam _ _ True _)
    = syntacticalError "variable arity parameter"

transformConstructorBody :: ConstructorBody -> PhaseResult CompoundStmt'
transformConstructorBody (ConstructorBody Nothing ss)
    = let b' = Block (ss ++ [BlockStmt Empty]) in transformBlock b'
transformConstructorBody (ConstructorBody (Just _) _) 
    = syntacticalError "base class constructor call"

transformMethodBody :: MethodBody -> PhaseResult CompoundStmt'
transformMethodBody (MethodBody (Just (Block b))) 
    = let b' = Block (b ++ [BlockStmt Empty]) in transformBlock b'
transformMethodBody (MethodBody Nothing)  
    = syntacticalError "method without implementation"

transformBlock :: Block -> PhaseResult CompoundStmt'
transformBlock (Block [])     = pure emptyStmt
transformBlock (Block [s])    = transformBlockStmt s
transformBlock (Block (s:ss)) = Seq' <$> transformBlockStmt s <*> transformBlock (Block ss)

transformBlockStmt :: BlockStmt -> PhaseResult CompoundStmt'
transformBlockStmt (BlockStmt s)        = transformStmt s
transformBlockStmt (LocalClass _)       = syntacticalError "local class"
transformBlockStmt (LocalVars ms ty ds) = (\ ms' ty' -> Stmt' . Decl' ms' ty') 
    <$> transformModifiers ms 
    <*> transformType ty 
    <*> transformVarDecls ty ds

transformModifiers :: [Modifier] -> PhaseResult [Modifier']
transformModifiers = mapM transformModifier

transformModifier :: Modifier -> PhaseResult Modifier'
transformModifier Public        = pure Public'
transformModifier Private       = pure Private'
transformModifier Protected     = pure Protected'
transformModifier Abstract      = syntacticalError "abstract modifier"
transformModifier Final         = pure Final'
transformModifier Static        = pure Static'
transformModifier StrictFP      = syntacticalError "strictfp modifier"
transformModifier Transient     = syntacticalError "transient modifier"
transformModifier Volatile      = syntacticalError "volatile modifier"
transformModifier Annotation{}  = syntacticalError "annotation"
transformModifier Synchronized_ = syntacticalError "synchronized modifier"

transformMaybeType :: Maybe Type -> PhaseResult (Maybe Type')
transformMaybeType (Just ty) = Just <$> transformType ty
transformMaybeType Nothing   = pure Nothing

transformType :: Type -> PhaseResult Type'
transformType (PrimType BooleanT) = pure $ PrimType' BooleanT'
transformType (PrimType ByteT)    = pure $ PrimType' ByteT'
transformType (PrimType ShortT)   = pure $ PrimType' ShortT'
transformType (PrimType IntT)     = pure $ PrimType' IntT'
transformType (PrimType LongT)    = pure $ PrimType' LongT'
transformType (PrimType CharT)    = pure $ PrimType' CharT'
transformType (PrimType FloatT)   = pure $ PrimType' FloatT'
transformType (PrimType DoubleT)  = pure $ PrimType' DoubleT'
transformType (RefType ty)        = RefType' <$> transformRefType ty

transformRefType :: RefType -> PhaseResult RefType'
transformRefType (ClassRefType ty) = ClassRefType' <$> transformClassType ty
transformRefType (ArrayType ty)    = ArrayType'    <$> transformType ty

transformClassType :: ClassType -> PhaseResult ClassType'
transformClassType (ClassType tys) = ClassType' <$> pure [i | (Ident i) <- map fst tys]

transformVarDecls :: Type -> [VarDecl] -> PhaseResult [VarDecl']
transformVarDecls ty = mapM (transformVarDecl ty)

transformVarDecl :: Type -> VarDecl -> PhaseResult VarDecl'
transformVarDecl ty (VarDecl id Nothing)     = VarDecl' <$> transformVarDeclId id <*> defaultInit ty
transformVarDecl _  (VarDecl id (Just init)) = VarDecl' <$> transformVarDeclId id <*> transformVarInit init

transformVarDeclId :: VarDeclId -> PhaseResult VarDeclId'
transformVarDeclId (VarId (Ident n)) = pure $ VarId' n
transformVarDeclId (VarDeclArray _)  = syntacticalError "array typed declaration"

transformVarInit :: VarInit -> PhaseResult VarInit'
transformVarInit (InitExp e)                = InitExp' <$> transformExp e
transformVarInit (InitArray (ArrayInit is)) = InitArray' . Just' <$> mapM transformVarInit is

defaultInit :: Type -> PhaseResult VarInit'
defaultInit (PrimType BooleanT) = (pure . InitExp' . Lit' . Boolean') False
defaultInit (PrimType ByteT)    = (pure . InitExp' . Lit' . Int') 0 
defaultInit (PrimType ShortT)   = (pure . InitExp' . Lit' . Int') 0 
defaultInit (PrimType IntT)     = (pure . InitExp' . Lit' . Int') 0 
defaultInit (PrimType LongT)    = (pure . InitExp' . Lit' . Int') 0 
defaultInit (PrimType CharT)    = (pure . InitExp' . Lit' . Char') '\0'
defaultInit (PrimType FloatT)   = (pure . InitExp' . Lit' . Float') 0.0
defaultInit (PrimType DoubleT)  = (pure . InitExp' . Lit' . Double') 0.0
defaultInit (RefType ty)        = defaultRefInit ty

defaultRefInit :: RefType -> PhaseResult VarInit'
defaultRefInit (ArrayType    _) = (pure . InitArray') Nothing'
defaultRefInit (ClassRefType _) = pure (InitExp' (Lit' Null'))

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
                    i' <- maybe (pure emptyStmt) (\ (ForLocalVars ms ty ds) -> (\ ms' ty' -> Stmt' . Decl' ms' ty') <$> transformModifiers ms <*> transformType ty <*> transformVarDecls ty ds) i
                    u' <- maybe (pure emptyStmt) (fmap (foldr (\ e -> Seq' (Stmt' $ ExpStmt' e)) emptyStmt) . mapM transformExp) u
                    g' <- maybe ((pure . Lit' . Boolean') True) transformExp g
                    let while = While' Nothing g' (Seq' s' u')
                    pure $ Block' $ Seq' i' (Seq' while emptyStmt)
              , \ m t i g s -> syntacticalError "for (iterator)"
              ,                compound Empty'
              ,                fmap (Stmt' . ExpStmt') . transformExp
              , \ g -> \case Just m  -> (\ m' -> Stmt' . Assert' m') <$> transformExp g <*> transformExpToString m --transformExp m
                             Nothing -> (\ m' -> Stmt' . Assert' m') <$> transformExp g <*> pure ""
              , \ e bs      -> Switch' <$> transformExp e <*> mapM transformSwitchBlock bs
              , \ s e       -> syntacticalError "do"
              ,                compound . Break' . transformMaybeIdent
              ,                compound . Continue' . transformMaybeIdent
              , \case (Just e) -> Stmt' . ReturnExp' <$> transformExp e
                      Nothing  -> pure (Stmt' Return')
              , \ e s       -> syntacticalError "synchronized"
              , \ e         -> syntacticalError "throw"
              , \ b c f     -> syntacticalError "try catch (finally)"
              , \ (Ident l) s -> labelize (Just l) <$> s
              )

        labelize l (While' _ g s) = While' l g s 

transformSwitchBlock :: SwitchBlock -> PhaseResult SwitchBlock'
transformSwitchBlock (SwitchBlock (SwitchCase e) s) 
    = SwitchBlock' . Just <$> transformExp e <*> transformBlock (Block s)
transformSwitchBlock (SwitchBlock Default s) 
    = SwitchBlock' Nothing <$> transformBlock (Block s)

transformExpToString :: Exp -> PhaseResult String
transformExpToString (Lit (String string)) = pure string
transformExpToString e                     = pure (prettyPrint e)

transformExps :: [Exp] -> PhaseResult [Exp']
transformExps = mapM transformExp

transformExp :: Exp -> PhaseResult Exp'
transformExp = foldExp alg
    where 
        alg :: ExpAlgebra (PhaseResult Exp')
        alg = ExpAlgebra {
          lit  = fmap Lit' . transformLiteral
        , this = pure This'
        , thisClass = \ _
            -> syntacticalError "this class"
        , instanceCreation = transformInstanceCreation
        , qualInstanceCreation = \ _ _ _ _ _ 
            -> syntacticalError "qual instance creation"
        , arrayCreate = \ ty es 0
            -> ArrayCreate' <$> transformType ty <*> sequence es <*> pure 0
        , arrayCreateInit = \ ty ds (ArrayInit is)
            -> ArrayCreateInit' <$> transformType ty <*> pure ds <*> mapM transformVarInit is
        , fieldAccess = fmap FieldAccess' . transformFieldAccess
        , methodInv = fmap MethodInv' . transformMethodInvocation
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
            -> syntacticalError "cast"
        , binOp = \ e1 op e2
            -> BinOp' <$> e1 <*> transformOp op <*> e2
        , instanceOf = \ _ _
            -> syntacticalError "instance of"
        , cond = \ g e1 e2
            -> Cond' <$> g <*> e1 <*> e2
        , assign = \ lhs op e
            -> Assign' <$> transformLhs lhs <*> transformAssignOp op <*> e
        , lambda = \ _ _
            -> syntacticalError "lambda"
        , methodRef = \ _ _
            -> syntacticalError "method ref"
        }

transformMethodInvocation :: MethodInvocation -> PhaseResult MethodInvocation'
transformMethodInvocation (MethodCall n args) 
    = MethodCall' <$> transformName n <*> transformExps args
transformMethodInvocation (PrimaryMethodCall e [] (Ident name) args)
    = PrimaryMethodCall' <$> transformExp e <*> pure name <*> transformExps args
transformMethodInvocation PrimaryMethodCall{} = syntacticalError "generics"

transformName :: Name -> PhaseResult Name'
transformName (Name ns) = pure [n | (Ident n) <- ns]

transformLhs :: Lhs -> PhaseResult Lhs'
transformLhs (NameLhs name)    = Name'  <$> transformName name
transformLhs (FieldLhs access) = Field' <$> transformFieldAccess access
transformLhs (ArrayLhs index)  = Array' <$> transformArrayIndex index

transformArrayIndex :: ArrayIndex -> PhaseResult ArrayIndex'
transformArrayIndex (ArrayIndex array indices)
    = ArrayIndex' <$> transformExp array <*> transformExps indices

transformFieldAccess :: FieldAccess -> PhaseResult FieldAccess'
transformFieldAccess (PrimaryFieldAccess e (Ident field))
    = PrimaryFieldAccess' <$> transformExp e <*> pure field
    
transformFieldAccess (ClassFieldAccess ty (Ident name))
    = ClassFieldAccess' <$> transformName ty <*> pure name

transformFieldAccess (SuperFieldAccess _)
    = syntacticalError "field access of super class"

transformInstanceCreation :: [TypeArgument] -> TypeDeclSpecifier -> [Argument] -> Maybe ClassBody -> PhaseResult Exp'
transformInstanceCreation (ty:tys) _ _ _ = syntacticalError "generics"
transformInstanceCreation _ _ _ (Just b) = syntacticalError "anonymous class creation"
transformInstanceCreation [] (TypeDeclSpecifier ty) args Nothing
    = InstanceCreation' <$> transformClassType ty <*> transformExps args 
transformInstanceCreation _ s _ _ = syntacticalError "generics"

transformOp:: Op -> PhaseResult Op'
transformOp Mult    = pure Mult'
transformOp Div     = pure Div'
transformOp Rem     = pure Rem'
transformOp Add     = pure Add'
transformOp Sub     = pure Sub'
transformOp LShift  = pure LShift'
transformOp RShift  = pure RShift'
transformOp RRShift = pure RRShift'
transformOp LThan   = pure LThan'
transformOp GThan   = pure GThan'
transformOp LThanE  = pure LThanE'
transformOp GThanE  = pure GThanE'
transformOp Equal   = pure Equal'
transformOp NotEq   = pure NotEq'
transformOp And     = pure And'
transformOp Or      = pure Or'
transformOp Xor     = pure Xor'
transformOp CAnd    = pure CAnd'
transformOp COr     = pure COr'

transformAssignOp:: AssignOp -> PhaseResult AssignOp'
transformAssignOp EqualA   = pure EqualA'
transformAssignOp MultA    = pure MultA'
transformAssignOp DivA     = pure DivA'
transformAssignOp RemA     = pure RemA'
transformAssignOp AddA     = pure AddA'
transformAssignOp SubA     = pure SubA'
transformAssignOp LShiftA  = pure LShiftA'
transformAssignOp RShiftA  = pure RShiftA'
transformAssignOp RRShiftA = pure RRShiftA'
transformAssignOp AndA     = pure AndA'
transformAssignOp XorA     = pure XorA'
transformAssignOp OrA      = pure OrA'

transformLiteral :: Literal -> PhaseResult Literal'
transformLiteral (Int     v) = pure $ Int'     v
transformLiteral (Float   v) = pure $ Float'   (realToFrac v)
transformLiteral (Double  v) = pure $ Double'  (realToFrac v)
transformLiteral (Boolean v) = pure $ Boolean' v
transformLiteral (Char    v) = pure $ Char'    v
transformLiteral (String  v) = pure $ String'  v
transformLiteral Null        = pure Null'

emptyStmt :: CompoundStmt'
emptyStmt = Stmt' Empty'

compound :: Stmt' -> PhaseResult CompoundStmt'
compound = pure . Stmt'
