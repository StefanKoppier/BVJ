{-|
Module      : Parsing.Utility
Description : Module containing utility functions for the AST.
-}
module Parsing.Utility where

import Data.List
import Data.Maybe
import Parsing.Syntax

-- | Returns the method of the given scope, if it exists.
getMethod :: CompilationUnit' -> Scope -> Maybe MemberDecl'
getMethod unit (Scope _ scopeClass scopeMember)
    -- Case: the method is a constructor.
    | (Just class') <- findClass scopeMember unit
        = findConstructor class'

    -- Case: the method is a method.
    | otherwise
        = findMethod scopeMember (fromJust $ findClass scopeClass unit)

-- | Returns the name of the reference type.
nameOfRefType :: RefType' -> Name'
nameOfRefType (ClassRefType' ty) = nameOfClassType ty

-- | Returns the name of the class.
nameOfClassType :: ClassType' -> Name'
nameOfClassType (ClassType' name) = name

-- | Returns the return type of the method, if it is not a field.
-- The `void` type is modelled as Nothing.
getReturnTypeOfMethod :: MemberDecl' -> Maybe (Maybe Type')
getReturnTypeOfMethod (ConstructorDecl' _ name _ _) 
    = Just . Just . RefType' . ClassRefType' . ClassType' $ [name]
getReturnTypeOfMethod (MethodDecl' _ ty _ _ _) = Just ty
getReturnTypeOfMethod (FieldDecl' _ _ _) = Nothing

-- | Returns all method declarations from the declarations.
getMethods :: Decls' -> [MemberDecl']
getMethods ds = [m | (MemberDecl' m@MethodDecl'{}) <- ds]

-- | Returns True if the class declaration equals the given name.
hasClassName :: String -> ClassDecl' -> Bool
hasClassName name (ClassDecl' _ name' _) = name == name'

-- | Returns True if the method declaration equals the given name.
hasMethodName :: String -> MemberDecl' -> Bool
hasMethodName name (MethodDecl' _ _ name' _ _) = name == name'

-- | Returns all class declarations from the type declaration.
getClasses :: TypeDecls' -> [ClassDecl']
getClasses ds = [c | (ClassTypeDecl' c) <- ds]

-- | Returns the method with the given name from the class, if it exists.
findMethod :: String -> ClassDecl' -> Maybe MemberDecl'
findMethod name (ClassDecl' _ _ body)
    | [m] <- filter (hasMethodName name) methods 
        = Just m
    | otherwise
        = Nothing
    where
        methods = getMethods body

-- | Returns the class with the given name from the compilation unit, if it exists.
findClass :: String -> CompilationUnit' -> Maybe ClassDecl'
findClass name (CompilationUnit' _ _ decls) 
    | [c] <- filter (hasClassName name) classes 
        = Just c
    | otherwise
        = Nothing
    where
        classes = getClasses decls

-- | Returns the constructor with the given name from the class, if it exists.
findConstructor :: ClassDecl' -> Maybe MemberDecl'
findConstructor (ClassDecl' _ _ body)
    | [c] <- constructors = Just c
    | otherwise           = Nothing
    where
        constructors = [c | MemberDecl'(c@ConstructorDecl'{}) <- body]

-- | Returns the class name containing the `main` method, if it exists.
findMainClass :: CompilationUnit' -> Maybe String
findMainClass (CompilationUnit' package _ decls)
    | Just (ClassDecl' _ name _) <- mainClass
        = Just $ maybe name (\ p -> intercalate "." (p ++ [name])) package
    | Nothing <- mainClass
        = Nothing
    where
        mainClass = find (isJust . findMethod "main") classes
        classes   = getClasses decls

-- | Returns the scope of the `main` method, if it exists.
findMainScope :: CompilationUnit' -> Maybe Scope
findMainScope unit@(CompilationUnit' package _ _)
    | (Just className) <- findMainClass unit
        = Just $ Scope package className "main"
    | otherwise
        = Nothing

-- | Returns the scope of the method of the given name.
findMethodScope :: CompilationUnit' -> Name' -> Maybe Scope
findMethodScope (CompilationUnit' _ _ _) [className, methodName] 
    = Just $ Scope Nothing className methodName

findMethodScope (CompilationUnit' _ _ _) name
    = Just $ Scope (Just packageName) className methodName
    where
        packageLength = length name - 2
        (packageName, [className, methodName]) = splitAt packageLength name