module Parsing.Utility where

import Data.List
import Data.Maybe
import Parsing.Syntax

getMethod :: CompilationUnit' -> Scope -> Maybe MemberDecl'
getMethod unit (Scope _ scopeClass scopeMember)
    -- Case: the method is a constructor.
    | (Just class') <- findClass scopeMember unit
        = findConstructor class'

    -- Case: the method is a method.
    | otherwise
        = findMethod scopeMember (fromJust $ findClass scopeClass unit)

nameOfRefType :: RefType' -> Name'
nameOfRefType (ClassRefType' ty) = nameOfClassType ty

nameOfClassType :: ClassType' -> Name'
nameOfClassType (ClassType' name) = name

getReturnTypeOfMethod :: MemberDecl' -> Maybe (Maybe Type')
getReturnTypeOfMethod (ConstructorDecl' _ name _ _) 
    = Just . Just . RefType' . ClassRefType' . ClassType' $ [name]
getReturnTypeOfMethod (MethodDecl' _ ty _ _ _) = Just ty

getMethods :: Decls' -> [MemberDecl']
getMethods ds = [m | (MemberDecl' m@MethodDecl'{}) <- ds]

hasClassName :: String -> ClassDecl' -> Bool
hasClassName name (ClassDecl' _ name' _) = name == name'

hasMethodName :: String -> MemberDecl' -> Bool
hasMethodName name (MethodDecl' _ _ name' _ _) = name == name'

getClasses :: TypeDecls' -> [ClassDecl']
getClasses ds = [c | (ClassTypeDecl' c) <- ds]

findMethod :: String -> ClassDecl' -> Maybe MemberDecl'
findMethod name (ClassDecl' _ _ body)
    | [m] <- filter (hasMethodName name) methods 
        = Just m
    | otherwise
        = Nothing
    where
        methods = getMethods body

findClass :: String -> CompilationUnit' -> Maybe ClassDecl'
findClass name (CompilationUnit' _ _ decls) 
    | [c] <- filter (hasClassName name) classes 
        = Just c
    | otherwise
        = Nothing
    where
        classes = getClasses decls

findConstructor :: ClassDecl' -> Maybe MemberDecl'
findConstructor (ClassDecl' _ _ body)
    | [c] <- constructors = Just c
    | otherwise           = Nothing
    where
        constructors = [c | MemberDecl'(c@ConstructorDecl'{}) <- body]

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

findMethodScope :: CompilationUnit' -> Name' -> Maybe Scope
findMethodScope (CompilationUnit' _ _ _) [className, methodName] 
    = Just $ Scope Nothing className methodName

findMethodScope (CompilationUnit' _ _ _) name
    = Just $ Scope (Just packageName) className methodName
    where
        packageLength = length name - 2
        (packageName, [className, methodName]) = splitAt packageLength name