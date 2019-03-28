module Parsing.Fold where

import Parsing.Syntax

data ExpAlgebra' r = ExpAlgebra'
  { lit              :: Literal' -> r
  , this             :: r
  , instanceCreation :: ClassType' -> [r] -> r
  , arrayCreate      :: Type' -> [r] -> Int -> r
  , arrayCreateInit  :: Type' -> Int -> VarInits' -> r
  , fieldAccess      :: FieldAccess' -> r
  , methodInv        :: MethodInvocation' -> r
  , arrayAccess      :: String -> [r] -> r
  , expName          :: Name' -> r
  , postIncrement    :: r -> r
  , postDecrement    :: r -> r
  , preIncrement     :: r -> r
  , preDecrement     :: r -> r
  , prePlus          :: r -> r
  , preMinus         :: r -> r
  , preBitCompl      :: r -> r
  , preNot           :: r -> r
  , binOp            :: r -> Op' -> r -> r
  , cond             :: r -> r -> r -> r
  , assign           :: Lhs' -> AssignOp' -> r -> r
  }

foldExp :: ExpAlgebra' r -> Exp' -> r
foldExp (ExpAlgebra' fLit fThis fInstanceCreation fArrayCreate fArrayCreateInit 
                     fFieldAccess fMethodInv fArrayAccess fExpName fPostIncrement 
                     fPostDecrement fPreIncrement fPreDecrement fPrePlus 
                     fPreMinus fPreBitCompl fPreNot fBinOp fCond fAssign) 
    = fold
    where
        fold e = case e of
                    Lit' lit -> fLit lit
                    This' -> fThis
                    InstanceCreation' classType args -> fInstanceCreation classType (map fold args)
                    ArrayCreate' t exps dim -> fArrayCreate t (map fold exps) dim
                    ArrayCreateInit' t dim arrayInit -> fArrayCreateInit t dim arrayInit
                    FieldAccess' fieldAccess -> fFieldAccess fieldAccess
                    MethodInv' invocation -> fMethodInv invocation
                    ArrayAccess' i is -> fArrayAccess i (map fold is)
                    ExpName' name -> fExpName name
                    PostIncrement' e -> fPostIncrement (fold e)
                    PostDecrement'  e -> fPostDecrement  (fold e)
                    PreIncrement' e -> fPreIncrement (fold e)
                    PreDecrement'  e -> fPreDecrement  (fold e)
                    PrePlus' e -> fPrePlus (fold e)
                    PreMinus' e -> fPreMinus (fold e)
                    PreBitCompl' e -> fPreBitCompl (fold e)
                    PreNot' e -> fPreNot (fold e)
                    BinOp' e1 op e2 -> fBinOp (fold e1) op (fold e2)
                    Cond' g e1 e2 -> fCond (fold g) (fold e1) (fold e2)
                    Assign' lhs assOp e -> fAssign lhs assOp (fold e)