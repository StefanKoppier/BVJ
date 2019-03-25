module Parsing.Utility where

import Data.List
import Data.Maybe
import Parsing.Syntax

import Debug.Trace

class WithModifiers a where
    isStatic     :: a -> Bool

instance WithModifiers MemberDecl' where
    isStatic (FieldDecl'       ms _ _)     = Static' `elem` ms
    isStatic (MethodDecl'      ms _ _ _ _) = Static' `elem` ms
    isStatic (ConstructorDecl' ms _ _ _)   = True

getMethod :: CompilationUnit' -> Scope -> Maybe MemberDecl'
getMethod unit (Scope _ scopeClass scopeMember)
    -- Case: the method is a constructor.
    | (Just class') <- findClass scopeMember unit
        = findConstructor class'

    -- Case: the method is a method.
    | otherwise
        = findMethod scopeMember (fromJust $ findClass scopeClass unit)

containsNonStaticFieldWithName :: ClassDecl' -> String -> Bool
containsNonStaticFieldWithName classDecl field
    = let fields          = getFields classDecl
          nonStaticFields = filter (not . isStatic) fields
       in any (containsFieldWithName field) nonStaticFields 

getParams :: MemberDecl' -> [FormalParam']
getParams (MethodDecl' _ _ _ params _)    = params
getParams (ConstructorDecl' _ _ params _) = params
getParams (FieldDecl' _ _ _)              = trace "IMPLEMENTATION ERROR: getParams call on FieldDecl'" []

nameOfClass :: ClassDecl' -> String
nameOfClass (ClassDecl' _ name _) = name

nameOfMember :: MemberDecl' -> String
nameOfMember (MethodDecl' _ _ name _ _)    = name
nameOfMember (ConstructorDecl' _ name _ _) = name

namesOfParams :: FormalParams' -> [String]
namesOfParams = map nameOfParam

nameOfParam :: FormalParam' -> String
nameOfParam (FormalParam' _ _ (VarId' name)) = name

namesOfDecls :: VarDecls' -> [String]
namesOfDecls = map nameOfDecl

nameOfRefType :: RefType' -> Name'
nameOfRefType (ClassRefType' ty) = nameOfClassType ty

nameOfClassType :: ClassType' -> Name'
nameOfClassType (ClassType' name) = name

nameOfDecl :: VarDecl' -> String
nameOfDecl (VarDecl' (VarId' name) _) = name

namesOfField :: MemberDecl' -> [String]
namesOfField (FieldDecl' _ _ var) = map nameOfDecl var

findClass :: String -> CompilationUnit' -> Maybe ClassDecl'
findClass name (CompilationUnit' _ _ decls) 
    | [c] <- filter (hasClassName name) classes 
        = Just c
    | otherwise
        = Nothing
    where
        classes = getClasses decls

isRefType :: Maybe Type' -> Bool
isRefType (Just (RefType'  _)) = True 
isRefType _                    = False

getClasses :: TypeDecls' -> [ClassDecl']
getClasses ds = [c | (ClassTypeDecl' c) <- ds]

hasClassName :: String -> ClassDecl' -> Bool
hasClassName name (ClassDecl' _ name' _) = name == name'

containsFieldWithName :: String -> MemberDecl' -> Bool
containsFieldWithName name field
    = name `elem` namesOfField field

findConstructor :: ClassDecl' -> Maybe MemberDecl'
findConstructor (ClassDecl' _ _ body)
    | [c] <- constructors = Just c
    | otherwise           = Nothing
    where
        constructors = [c | MemberDecl'(c@ConstructorDecl'{}) <- body]

findMethod :: String -> ClassDecl' -> Maybe MemberDecl'
findMethod name (ClassDecl' _ _ body)
    | [m] <- filter (hasMethodName name) methods 
        = Just m
    | otherwise
        = Nothing
    where
        methods = getMethods body

getMethods :: Decls' -> [MemberDecl']
getMethods ds = [m | (MemberDecl' m@MethodDecl'{}) <- ds]

getReturnTypeOfMethod :: MemberDecl' -> Maybe (Maybe Type')
getReturnTypeOfMethod (ConstructorDecl' _ name _ _) 
    = Just . Just . RefType' . ClassRefType' . ClassType' $ [name]
getReturnTypeOfMethod (MethodDecl' _ ty _ _ _) = Just ty

getParamsOfMethod :: MemberDecl' -> Maybe [FormalParam']
getParamsOfMethod (MethodDecl' _ _ _ ps _)    = Just ps
getParamsOfMethod (ConstructorDecl' _ _ ps _) = Just ps

hasMethodName :: String -> MemberDecl' -> Bool
hasMethodName name (MethodDecl' _ _ name' _ _) = name == name'

getFields :: ClassDecl' -> [MemberDecl']
getFields (ClassDecl' _ _ body) = [f | (MemberDecl'(f@FieldDecl'{})) <- body] 

sizesOfVarInit :: VarInits' -> [Exp']
sizesOfVarInit []                           = []
sizesOfVarInit (InitExp' _:xs)              = [Lit' (Int' (fromIntegral (1 + length xs)))]
sizesOfVarInit (InitArray' (Just inits):xs) = Lit' (Int' (fromIntegral (1 + length xs))) : sizesOfVarInit inits 

findMainClass :: CompilationUnit' -> Maybe String
findMainClass (CompilationUnit' package _ decls)
    | Just (ClassDecl' _ name _) <- mainClass
        = Just $ maybe name (\ p -> intercalate "." (p ++ [name])) package
    | Nothing <- mainClass
        = Nothing
    where
        mainClass = find (isJust . findMethod "main") classes
        classes   = getClasses decls

findMainScope :: CompilationUnit' -> Maybe Scope
findMainScope unit@(CompilationUnit' package _ _)
    | (Just className) <- findMainClass unit
        = Just $ Scope package className "main"
    | otherwise
        = Nothing