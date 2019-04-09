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
    liftIO $ printHeader "1. PARSING"
    liftIO $ printTitled "Input program" content
    case parser compilationUnit content of 
        Right program -> syntaxTransformationSubphase args program
        Left  e       -> throwParsingError (show e)

--------------------------------------------------------------------------------
-- Syntax transformation subphase
--------------------------------------------------------------------------------

syntaxTransformationSubphase :: Subphase CompilationUnit CompilationUnit'
syntaxTransformationSubphase _ = transformCompilationUnit

transformCompilationUnit :: CompilationUnit -> PhaseResult CompilationUnit'
transformCompilationUnit (CompilationUnit package imports types) 
    = CompilationUnit' 
    <$> transformPackageDecl package 
    <*> mapM transformImportdecl imports 
    <*> mapM transformTypeDecl types

transformPackageDecl :: Maybe PackageDecl -> PhaseResult (Maybe Name')
transformPackageDecl Nothing                = pure Nothing
transformPackageDecl (Just (PackageDecl n)) = Just <$> transformName n

transformImportdecl :: ImportDecl -> PhaseResult ImportDecl'
transformImportdecl (ImportDecl id name everything)
    = ImportDecl' <$> pure id <*> transformName name <*> pure everything

transformTypeDecl :: TypeDecl -> PhaseResult TypeDecl'
transformTypeDecl (ClassTypeDecl cls)   = ClassTypeDecl' <$> transformClassDecl cls
transformTypeDecl (InterfaceTypeDecl _) = throwSyntacticalError "interface declaration"
    
transformClassDecl :: ClassDecl -> PhaseResult ClassDecl'
transformClassDecl (ClassDecl ms (Ident n) [] Nothing [] (ClassBody ds)) 
    = ClassDecl' <$> transformModifiers ms <*> pure n <*> mapM transformDecl ds
transformClassDecl EnumDecl{}
    = throwSyntacticalError "enum declaration"
transformClassDecl ClassDecl{}                              
    = throwSyntacticalError "inheritance or generics"

transformDecl :: Decl -> PhaseResult Decl'
transformDecl (MemberDecl m) = MemberDecl' <$> transformMemberDecl m
transformDecl (InitDecl _ _) = throwSyntacticalError "initializer declarations"

transformMemberDecl :: MemberDecl -> PhaseResult MemberDecl'
transformMemberDecl (FieldDecl ms ty vs) 
    = FieldDecl' <$> transformModifiers ms <*> transformType ty <*> transformVarDecls ty vs
transformMemberDecl (MethodDecl ms [] ty (Ident n) ps [] _ b)
    = MethodDecl' <$> transformModifiers ms <*> transformMaybeType ty <*> pure n <*> transformParams ps <*> transformMethodBody b
transformMemberDecl MethodDecl{}
    = throwSyntacticalError "generics or exception"
transformMemberDecl (ConstructorDecl ms [] (Ident n) ps [] b)
    = ConstructorDecl' <$> transformModifiers ms <*> pure n <*> transformParams ps <*> transformConstructorBody b
transformMemberDecl ConstructorDecl{}
    = throwSyntacticalError "generics or exception"
transformMemberDecl (MemberClassDecl _)
    = throwSyntacticalError "nested class declaration"
transformMemberDecl (MemberInterfaceDecl _)
    = throwSyntacticalError "nested interface declaration"

transformParams :: [FormalParam] -> PhaseResult [FormalParam']
transformParams = mapM transformParam

transformParam :: FormalParam -> PhaseResult FormalParam'
transformParam (FormalParam ms ty False id) 
    = FormalParam' <$> transformModifiers ms <*> transformType ty <*> transformVarDeclId id
transformParam (FormalParam _ _ True _)
    = throwSyntacticalError "variable arity parameter"

transformConstructorBody :: ConstructorBody -> PhaseResult CompoundStmts'
transformConstructorBody (ConstructorBody Nothing ss) = do
    ss' <- transformBlockStmts ss
    return $ [emptyStmt] ++ ss' ++ [emptyStmt]
transformConstructorBody (ConstructorBody (Just _) _) 
    = throwSyntacticalError "base class constructor call"

transformMethodBody :: MethodBody -> PhaseResult CompoundStmts'
transformMethodBody (MethodBody (Just (Block b)))  = do
    b' <- transformBlockStmts b
    return $ [emptyStmt] ++ b' ++ [emptyStmt]
transformMethodBody (MethodBody Nothing)  
    = throwSyntacticalError "method without implementation"

transformBlock :: Block -> PhaseResult CompoundStmt'
transformBlock (Block ss) = Block' Nothing <$> transformBlockStmts ss

transformMaybeBlock :: Maybe Block -> PhaseResult MaybeCompoundStmts'
transformMaybeBlock Nothing           = pure Nothing
transformMaybeBlock (Just (Block [])) = pure Nothing
transformMaybeBlock (Just (Block b))  = Just <$> transformBlockStmts b

transformBlockStmts :: [BlockStmt] -> PhaseResult CompoundStmts'
transformBlockStmts []     = return []
transformBlockStmts (s:ss) = do
    s'  <- transformBlockStmt s
    ss' <- transformBlockStmts ss
    case s of
        BlockStmt StmtBlock{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        BlockStmt While{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        BlockStmt BasicFor{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        BlockStmt IfThenElse{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        BlockStmt IfThen{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        BlockStmt Try{}
            -> return ([emptyStmt, s', emptyStmt] ++ ss')
        _   -> return (s' : ss')

transformBlockStmt :: BlockStmt -> PhaseResult CompoundStmt'
transformBlockStmt (BlockStmt s)        = transformStmt s
transformBlockStmt (LocalClass _)       = throwSyntacticalError "local class"
transformBlockStmt (LocalVars ms ty ds) = (\ ms' ty' -> Stmt' . Decl' ms' ty') 
    <$> transformModifiers ms 
    <*> transformType ty 
    <*> transformVarDecls ty ds

transformModifiers :: [Modifier] -> PhaseResult [Modifier']
transformModifiers = mapM transformModifier

transformModifier :: Modifier -> PhaseResult Modifier'
transformModifier Public         = pure Public'
transformModifier Private        = pure Private'
transformModifier Protected      = pure Protected'
transformModifier Abstract       = pure Abstract'
transformModifier Final          = pure Final'
transformModifier Static         = pure Static'
transformModifier StrictFP       = pure StrictFP'
transformModifier Transient      = pure Transient'
transformModifier Volatile       = pure Volatile'
transformModifier (Annotation a) = Annotation' <$> transformAnnotation a
transformModifier Synchronized_  = pure Synchronized'

transformAnnotation :: Annotation -> PhaseResult Annotation'
transformAnnotation (NormalAnnotation name values)
    = NormalAnnotation' <$> transformName name <*> mapM (\ (Ident name, v) -> (name,) <$> transformElementValue v) values
transformAnnotation (SingleElementAnnotation name value)
    = SingleElementAnnotation' <$> transformName name <*> transformElementValue value
transformAnnotation (MarkerAnnotation name)
    = MarkerAnnotation' <$> transformName name

transformElementValue :: ElementValue -> PhaseResult ElementValue'
transformElementValue (EVVal init) 
    = ElementValue' <$> transformVarInit init
transformElementValue (EVAnn annotation) 
    = ElementAnnotation' <$> transformAnnotation annotation

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

transformToInnerMostArrayType :: Type -> PhaseResult Type'
transformToInnerMostArrayType (RefType (ArrayType t))
    = transformToInnerMostArrayType t
transformToInnerMostArrayType ty
    = transformType ty

transformVarDecls :: Type -> [VarDecl] -> PhaseResult [VarDecl']
transformVarDecls ty = mapM (transformVarDecl ty)

transformVarDecl :: Type -> VarDecl -> PhaseResult VarDecl'
transformVarDecl ty (VarDecl id Nothing)     
    = VarDecl' <$> transformVarDeclId id <*> defaultInit ty
transformVarDecl _  (VarDecl id (Just init@InitExp{})) 
    = VarDecl' <$> transformVarDeclId id <*> transformVarInit init
transformVarDecl ty (VarDecl id (Just init@(InitArray (ArrayInit inits)))) = do
    id'        <- transformVarDeclId id
    inits'     <- mapM transformVarInit inits
    ty'        <- transformToInnerMostArrayType ty
    dimensions <- dimensionsOfVarInit init
    return $ VarDecl' id' (InitExp' (ArrayCreateInit' ty' dimensions inits'))

dimensionsOfVarInit :: VarInit -> PhaseResult Int
dimensionsOfVarInit (InitArray (ArrayInit (init:_))) = (1+) <$> dimensionsOfVarInit init
dimensionsOfVarInit (InitExp _)                      = pure 0

transformVarDeclId :: VarDeclId -> PhaseResult VarDeclId'
transformVarDeclId (VarId (Ident n)) = pure $ VarId' n
transformVarDeclId (VarDeclArray _)  = throwSyntacticalError "array typed declaration"

transformVarInit :: VarInit -> PhaseResult VarInit'
transformVarInit (InitExp e)                = InitExp' <$> transformExp e
transformVarInit (InitArray (ArrayInit is)) = InitArray' . Just <$> mapM transformVarInit is

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
defaultRefInit (ArrayType    _) = (pure . InitArray') Nothing
defaultRefInit (ClassRefType _) = pure (InitExp' (Lit' Null'))

transformMaybeIdent :: Maybe Ident -> Maybe String
transformMaybeIdent (Just (Ident x)) = Just x
transformMaybeIdent Nothing          = Nothing

transformStmt :: Stmt -> PhaseResult CompoundStmt'
transformStmt = foldStmt alg
    where
        alg :: StmtAlgebra (PhaseResult CompoundStmt')
        alg = (                    transformBlock
              ,                    transformIf
              ,                    transformIfThenElse
              , \ g s           -> While' Nothing <$> transformExp g <*> s
              ,                    transformFor
              , \ m t i g s     -> throwSyntacticalError "for (iterator)"
              ,                    compound Empty'
              ,                    fmap (Stmt' . ExpStmt') . transformExp
              , \ g m           -> (\ m' -> Stmt' . Assert' m') <$> transformExp g <*> transformMaybeExp m
              , \ e bs          -> Switch' <$> transformExp e <*> mapM transformSwitchBlock bs
              , \ s e           -> throwSyntacticalError "do"
              ,                    compound . Break' . transformMaybeIdent
              ,                    compound . Continue' . transformMaybeIdent
              , \ e             -> Stmt' . Return' <$> transformMaybeExp e
              , \ e s           -> throwSyntacticalError "synchronized"
              , \ e             -> Stmt' . Throw' <$> transformExp e
              , \ (Block b) c f -> Try' <$> transformBlockStmts b <*> transformCatches c <*> transformMaybeBlock f
              , \ (Ident l) s   -> labelize (Just l) <$> s
              )
        labelize l (While' _ g s) = While' l g s
        labelize l (Block' _ ss)  = Block' Nothing (labelizeFirstWhile l ss)

labelizeFirstWhile :: Maybe String -> CompoundStmts' -> CompoundStmts'
labelizeFirstWhile l (While' _ guard body:stats) 
    = While' l guard body : stats
labelizeFirstWhile l (stat:stats)
    = stat : labelizeFirstWhile l stats

transformIf :: Exp -> PhaseResult CompoundStmt' -> PhaseResult CompoundStmt'
transformIf guard stat = transformIfThenElse guard stat (pure $ Block' Nothing [emptyStmt])

transformIfThenElse :: Exp -> PhaseResult CompoundStmt' -> PhaseResult CompoundStmt' -> PhaseResult CompoundStmt' 
transformIfThenElse guard stat1 stat2 = do
    guard' <- transformExp guard
    stat1' <- transformToBlock stat1
    stat2' <- transformToBlock stat2
    return $ IfThenElse' guard' stat1' stat2'
    
transformToBlock :: PhaseResult CompoundStmt' -> PhaseResult CompoundStmt'
transformToBlock stat = do
    stat' <- stat
    case stat' of
        Block' _ _ -> stat
        _          -> return $ Block' Nothing [emptyStmt, stat', emptyStmt]

-- | Transforms a for loop into an equivalent while loop.
transformFor :: Maybe ForInit -> Maybe Exp -> Maybe [Exp] -> PhaseResult CompoundStmt' -> PhaseResult CompoundStmt'
transformFor init guard update body = do
    init'     <- maybe (pure emptyStmt) transformForInit init
    guard'    <- maybe (pure $ Lit' $ Boolean' True) transformExp guard
    update'   <- maybe (pure []) transformForUpdate update
    body'     <- body
    whileBody <- transformForBody body' update'
    let while = While' Nothing guard' whileBody
    pure (Block' Nothing [init', while, emptyStmt])

transformForInit :: ForInit -> PhaseResult CompoundStmt'
transformForInit (ForLocalVars ms ty ds)
    = (\ ms' ty' -> Stmt' . Decl' ms' ty') <$> transformModifiers ms <*> transformType ty <*> transformVarDecls ty ds

transformForBody :: CompoundStmt' -> CompoundStmts' -> PhaseResult CompoundStmt'
transformForBody (Block' _ ss) update -- TODO: check this ident
    = pure $ Block' Nothing (ss ++ update)
transformForBody s update
    = pure $ Block' Nothing (s : update)

transformForUpdate :: [Exp] -> PhaseResult CompoundStmts'
transformForUpdate es = do
    es' <- transformExps es
    pure $ map (Stmt' . ExpStmt') es'

transformSwitchBlock :: SwitchBlock -> PhaseResult SwitchBlock'
transformSwitchBlock (SwitchBlock (SwitchCase e) s) 
    = SwitchBlock' . Just <$> transformExp e <*> transformBlock (Block s)
transformSwitchBlock (SwitchBlock Default s) 
    = SwitchBlock' Nothing <$> transformBlock (Block s)

transformCatches :: [Catch] -> PhaseResult Catches'
transformCatches = mapM transformCatch

transformCatch :: Catch -> PhaseResult Catch'
transformCatch (Catch exc (Block body)) 
    = Catch' <$> transformParam exc <*> transformBlockStmts body

transformExpToString :: Exp -> PhaseResult String
transformExpToString (Lit (String string)) = pure string
transformExpToString e                     = pure (prettyPrint e)

transformExps :: [Exp] -> PhaseResult [Exp']
transformExps = mapM transformExp

transformMaybeExp :: Maybe Exp -> PhaseResult (Maybe Exp')
transformMaybeExp Nothing  = pure Nothing
transformMaybeExp (Just e) = Just <$> transformExp e

transformExp :: Exp -> PhaseResult Exp'
transformExp = foldExp alg
    where 
        alg :: ExpAlgebra (PhaseResult Exp')
        alg = ExpAlgebra {
          lit  = fmap Lit' . transformLiteral
        , this = pure This'
        , thisClass = \ _
            -> throwSyntacticalError "this class"
        , instanceCreation = transformInstanceCreation
        , qualInstanceCreation = \ _ _ _ _ _ 
            -> throwSyntacticalError "qual instance creation"
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
            -> throwSyntacticalError "cast"
        , binOp = \ e1 op e2
            -> BinOp' <$> e1 <*> transformOp op <*> e2
        , instanceOf = \ _ _
            -> throwSyntacticalError "instance of"
        , cond = \ g e1 e2
            -> Cond' <$> g <*> e1 <*> e2
        , assign = \ lhs op e
            -> Assign' <$> transformLhs lhs <*> transformAssignOp op <*> e
        , lambda = \ _ _
            -> throwSyntacticalError "lambda"
        , methodRef = \ _ _
            -> throwSyntacticalError "method ref"
        }

transformMethodInvocation :: MethodInvocation -> PhaseResult MethodInvocation'
transformMethodInvocation (MethodCall n args) 
    = MethodCall' <$> transformName n <*> transformExps args
transformMethodInvocation (PrimaryMethodCall e [] (Ident name) args)
    = PrimaryMethodCall' <$> transformExp e <*> pure name <*> transformExps args
transformMethodInvocation PrimaryMethodCall{} = throwSyntacticalError "generics"

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
    = throwSyntacticalError "field access of super class"

transformInstanceCreation :: [TypeArgument] -> TypeDeclSpecifier -> [Argument] -> Maybe ClassBody -> PhaseResult Exp'
transformInstanceCreation (ty:tys) _ _ _ = throwSyntacticalError "generics"
transformInstanceCreation _ _ _ (Just b) = throwSyntacticalError "anonymous class creation"
transformInstanceCreation [] (TypeDeclSpecifier ty) args Nothing
    = InstanceCreation' <$> transformClassType ty <*> transformExps args 
transformInstanceCreation _ s _ _ = throwSyntacticalError "generics"

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

transformAssignOp :: AssignOp -> PhaseResult AssignOp'
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
