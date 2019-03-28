module Compilation.Compiler.Expression where

import Data.Either
import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty
import Parsing.Fold

import Debug.Trace

buildMethodExp :: [VarDeclId'] -> Exp' -> Exp'
buildMethodExp locals
    = foldExp alg
    where
        alg :: ExpAlgebra' Exp'
        alg = ExpAlgebra' {
          lit              = Lit'
        , this             = This'
        , instanceCreation = \ ty args
            -> MethodInv' (MethodCall' (createConstructorCallName ty) args)
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

buildConstructorExp :: [VarDeclId'] -> Exp' -> Exp'
buildConstructorExp locals
    = foldExp alg
    where
        alg :: ExpAlgebra' Exp'
        alg = ExpAlgebra' {
          lit            = Lit'
        , this             = ExpName' ["_thisObj__"]
        , instanceCreation = \ ty args
            -> MethodInv' (MethodCall' (createConstructorCallName ty) args)
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
            -> Assign' (buildConstructorLhs locals lhs) op e
        }

buildConstructorLhs :: [VarDeclId'] -> Lhs' -> Lhs'
buildConstructorLhs locals (Field' (PrimaryFieldAccess' e field))
    = Field' $ PrimaryFieldAccess' (buildConstructorExp locals e) field

buildConstructorLhs locals f@(Name' name)
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

createConstructorCallName :: ClassType' -> Name'
createConstructorCallName (ClassType' name)
    = init name ++ [createAcc "" (last name), last name]
    where
        createAcc :: String -> String -> String
        createAcc acc ('_':xs) = acc
        createAcc acc (x:xs)   = createAcc (acc ++ [x]) xs