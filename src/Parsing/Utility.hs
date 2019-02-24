module Parsing.Utility where

import Data.List
import Data.Maybe
import Parsing.Syntax

findClass :: String -> CompilationUnit' -> Maybe ClassDecl'
findClass name (CompilationUnit' _ decls) 
    | [c] <- filter (hasClassName name) classes 
        = Just c
    | otherwise
        = Nothing
    where
        classes = getClasses decls

getClasses :: TypeDecls' -> [ClassDecl']
getClasses ds = [c | (ClassTypeDecl' c) <- ds]

hasClassName :: String -> ClassDecl' -> Bool
hasClassName name (ClassDecl' _ name' _) = name == name'

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