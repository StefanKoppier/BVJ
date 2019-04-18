{-|
Module      : Compilation.Compiler.Expression
Description : Module containing the AST generation of an expression.
-}
module Compilation.Compiler.Expression where

import Data.Maybe
import Auxiliary.Phase
import Parsing.Pretty()
import Parsing.Fold
import Parsing.Utility

-- | Generates the expression in a method of a given expression.
buildMethodExp :: CompilationUnit' -> [VarDeclId'] -> Exp' -> Exp'
buildMethodExp unit _
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

-- | Generates the expression in a constructor of a given expression.
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

-- | Modifies the instance creation to a regular method call, if the constructor
-- is defined locally.
modifyInstanceCreation :: CompilationUnit' -> ClassType' -> Exps' -> Exp'
modifyInstanceCreation unit ty@(ClassType' _) args
    | isDefinedConstructor unit ty
        = MethodInv' (MethodCall' (createConstructorCallName ty) args)
    | otherwise
        = InstanceCreation' ty args

-- | Generates the modified lhs in a constructor call.
buildConstructorLhs :: CompilationUnit' -> [VarDeclId'] -> Lhs' -> Lhs'
buildConstructorLhs unit locals (Field' (PrimaryFieldAccess' e field))
    = Field' $ PrimaryFieldAccess' (buildConstructorExp unit locals e) field

buildConstructorLhs _ _ f@(Field' (ClassFieldAccess' _ _))
    = f

buildConstructorLhs _ locals f@(Name' name)
    -- Case: the variable is not a local declaration.
    | [variable] <- name
    , not (isLocalDeclaration variable locals)
        = Name' ["_thisObj__", variable]
    
    -- Case: any other.
    | otherwise
        = f

buildConstructorLhs unit locals (Array' (ArrayIndex' array indices))
    = let array'   = buildConstructorExp unit locals array
          indices' = map (buildConstructorExp unit locals) indices
       in Array' $ ArrayIndex' array' indices'

-- | Returns true if the variable is an element of the declarations.
isLocalDeclaration :: String -> [VarDeclId'] -> Bool
isLocalDeclaration variable declarations
    = variable `elem` [s | (VarId' s) <- declarations]

-- | Returns true if a constructor of the given type is defined in the compilation
-- unit.
isDefinedConstructor :: CompilationUnit' -> ClassType' -> Bool
isDefinedConstructor unit (ClassType' name)
    = isJust (findClass (toClassName "" (last name)) unit)

-- | Generates the constructor call name.
createConstructorCallName :: ClassType' -> Name'
createConstructorCallName (ClassType' name)
    = init name ++ [toClassName "" (last name), last name]
    
-- | Maps the given class name to it's class name using an accumulator.
toClassName :: String -> String -> String
toClassName acc []      = acc
toClassName acc ('_':_) = acc
toClassName acc (x:xs)  = toClassName (acc ++ [x]) xs
