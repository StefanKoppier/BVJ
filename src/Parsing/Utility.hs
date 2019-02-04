module Parsing.Utility where

import Data.List
import Data.Maybe
import Language.Java.Syntax

findMethodBody :: String -> CompilationUnit -> Maybe Block
findMethodBody name program = do
    method <- findMethod name program
    getMethodBlock method

findMethod :: String -> CompilationUnit -> Maybe MemberDecl
findMethod name program = do
    let ident   = Ident name
    let classes = getClasses program
    let bodies  = mapMaybe getClassBody classes
    let members = concatMap getClassBodyMembers bodies
    find (isMethodDeclWithIdent ident) members

isMethodDeclWithIdent :: Ident -> MemberDecl -> Bool
isMethodDeclWithIdent ident (MethodDecl _ _ _ name _ _ _ _)
    = ident == name
isMethodDeclWithIdent _ _
    = False

getClasses :: CompilationUnit -> [ClassDecl]
getClasses (CompilationUnit _ _ decls)
    = (map (\ (ClassTypeDecl c) -> c) . filter isClass) decls

getClassBody :: ClassDecl -> Maybe ClassBody
getClassBody (ClassDecl _ _ _ _ _ body) = Just body
getClassBody _                          = Nothing

getClassBodyMembers :: ClassBody -> [MemberDecl]
getClassBodyMembers (ClassBody decls) 
    =  (map (\ (MemberDecl d) -> d) . filter isMemberDecl) decls

getMethodBlock :: MemberDecl -> Maybe Block
getMethodBlock (MethodDecl _ _ _ _ _ _ _ (MethodBody body)) = body
getMethodBlock _                                            = Nothing

isMemberDecl :: Decl -> Bool
isMemberDecl (MemberDecl _) = True
isMemberDecl _              = False

isClass :: TypeDecl -> Bool
isClass (ClassTypeDecl _) = True
isClass _                 = False
