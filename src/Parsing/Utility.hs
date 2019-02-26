module Parsing.Utility where

import Data.List
import Data.Maybe
import Parsing.Syntax

import Debug.Trace

class WithModifiers a where
    isStatic :: a -> Bool

instance WithModifiers MemberDecl' where
    isStatic (FieldDecl'       ms _ _)     = Static' `elem` ms
    isStatic (MethodDecl'      ms _ _ _ _) = Static' `elem` ms
    isStatic (ConstructorDecl' ms _ _ _)   = True

getMethod :: CompilationUnit' -> String -> Maybe MemberDecl'
getMethod unit methodName 
    -- Case: the method is a constructor.
    | (Just class') <- findClass methodName unit
        = findConstructor class'

    -- Case: the method is a method.
    | otherwise
        = findMethod methodName (fromJust $ findClass "Main" unit)
        
getParams :: MemberDecl' -> [FormalParam']
getParams (MethodDecl' _ _ _ params _)    = params
getParams (ConstructorDecl' _ _ params _) = params
getParams (FieldDecl' _ _ _)              = trace "IMPLEMENTATION ERROR: getParams call on FieldDecl'" []

findClass :: String -> CompilationUnit' -> Maybe ClassDecl'
findClass name (CompilationUnit' _ decls) 
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