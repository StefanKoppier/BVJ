module Compilation.Compiler.Class where

import Data.Maybe
import Data.List
import Data.Function               (on)
import Parsing.Syntax
import Parsing.Utility
import Auxiliary.Phase
import Compilation.Compiler.Method

buildClass :: CompilationUnit' -> ProgramPath -> PhaseResult TypeDecl'
buildClass unit path = do
    let methodGroups = (groupBy ((==) `on` (snd . snd)) . sortOn (snd . snd)) path
    methods <- mapM (buildMethod unit) methodGroups
    let fields = [f | f@(MemberDecl' FieldDecl'{}) <- members]
    return (ClassTypeDecl' (ClassDecl' modifiers name (fields ++ methods)))
    where
        name                             = className (head path)
        (ClassDecl' modifiers _ members) = fromJust $ findClass name unit
