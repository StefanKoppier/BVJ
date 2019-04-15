module Compilation.Compiler.Expression where

import Data.Either
import Data.List
import Data.Maybe
import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty
import Parsing.Fold
import Parsing.Utility

buildMethodExp :: CompilationUnit' -> [VarDeclId'] -> Exp' -> Exp'
buildMethodExp unit locals
    = foldExp alg
    where
        alg :: ExpAlgebra' Exp'
        alg = ExpAlgebra' {
          lit              = Lit'
        , this             = This'
        , instanceCreation = modifyInstanceCreation unit
        , arrayCreate      = ArrayCreate'
        , arrayCreateInit  = ArrayCreateInit'
        , fieldAccess      = FieldAccess' 
        , methodInv        = MethodInv'  
        , arrayAccess      = ArrayAccess'
        , expName          = ExpName'
        , postIncrement    = PostIncrement'
        , postDecrement    = PostDecrement'
        , preIncrement     = PreIncrement'
        , preDecrement     = PreDecrement'
        , prePlus          = PrePlus'
        , preMinus         = PreMinus'
        , preBitCompl      = PreBitCompl'
        , preNot           = PreNot'
        , binOp            = BinOp'
        , cond             = Cond' 
        , assign           = Assign'
        }

buildConstructorExp :: CompilationUnit' -> [VarDeclId'] -> Exp' -> Exp'
buildConstructorExp unit locals
    = foldExp alg
    where
        alg :: ExpAlgebra' Exp'
        alg = ExpAlgebra' {
          lit            = Lit'
        , this             = ExpName' ["_thisObj__"]
        , instanceCreation = modifyInstanceCreation unit
        , arrayCreate      = ArrayCreate'
        , arrayCreateInit  = ArrayCreateInit'
        , fieldAccess      = FieldAccess'
        , methodInv        = MethodInv'  
        , arrayAccess      = ArrayAccess'
        , expName          = ExpName'
        , postIncrement    = PostIncrement'
        , postDecrement    = PostDecrement'
        , preIncrement     = PreIncrement'
        , preDecrement     = PreDecrement'
        , prePlus          = PrePlus'
        , preMinus         = PreMinus'
        , preBitCompl      = PreBitCompl'
        , preNot           = PreNot'
        , binOp            = BinOp'
        , cond             = Cond' 
        , assign           = \ lhs op e
            -> Assign' (buildConstructorLhs unit locals lhs) op e
        }

modifyInstanceCreation :: CompilationUnit' -> ClassType' -> Exps' -> Exp'
modifyInstanceCreation unit@(CompilationUnit' _ _ decls) ty@(ClassType' name) args
    | isDefinedConstructor unit ty
        = MethodInv' (MethodCall' (createConstructorCallName ty) args)
    | otherwise
        = InstanceCreation' ty args

buildConstructorLhs :: CompilationUnit' -> [VarDeclId'] -> Lhs' -> Lhs'
buildConstructorLhs unit locals (Field' (PrimaryFieldAccess' e field))
    = Field' $ PrimaryFieldAccess' (buildConstructorExp unit locals e) field

buildConstructorLhs _ locals f@(Name' name)
    -- Case: the variable is not a local declaration.
    | [variable] <- name
    , not (isLocalDeclaration variable locals)
        = Name' ["_thisObj__", variable]
    
    -- Case: any other.
    | otherwise
        = f

isLocalDeclaration :: String -> [VarDeclId'] -> Bool
isLocalDeclaration variable declarations
    = variable `elem` [s | (VarId' s) <- declarations]

isDefinedConstructor :: CompilationUnit' -> ClassType' -> Bool
isDefinedConstructor unit@(CompilationUnit' _ _ decls) (ClassType' name)
    = let className = toClassName "" (last name) 
       in isJust (findClass className unit)

createConstructorCallName :: ClassType' -> Name'
createConstructorCallName (ClassType' name)
    = init name ++ [toClassName "" (last name), last name]
    
toClassName :: String -> String -> String
toClassName acc []       = acc
toClassName acc ('_':xs) = acc
toClassName acc (x:xs)   = toClassName (acc ++ [x]) xs