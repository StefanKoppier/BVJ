

-- UUAGC 0.9.52.1 (Complete.ag)
module Analysis.Complete where

import Language.Java.Syntax
import Analysis.Flow

import Data.Maybe

{-# LINE 133 "CFA.ag" #-}

newest :: Labels -> Label
newest = new . maximum

new :: Label -> Label
new = (1+)

newIfJust :: Maybe a -> Label -> Label
newIfJust (Just _) = new
newIfJust Nothing  = id

node :: Label -> Stmt' -> Node
node l s = NodeStmt l s

forInitNode :: Label -> Maybe ForInit -> [Node]
forInitNode l (Just i) = [NodeForInit l i]
forInitNode _ Nothing  = []

forUpdateNode :: Label -> Maybe [Exp] -> [Node]
forUpdateNode l (Just es) = [NodeForUpdate l es]
forUpdateNode _ Nothing   = []

flowOfBlock :: Block' -> Flow
flowOfBlock block
    = flow_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block' = 0 } 
        
nodesOfBlock :: Block' -> Nodes
nodesOfBlock block
    = nodes_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block' = 0 } 

instance Show Node where
    show (NodeStmt l s)      = show l ++ ": " ++ show s
    show (NodeVarDecl l s)   = show l ++ ": " ++ show s
    show (NodeForInit l s)   = show l ++ ": " ++ show s
    show (NodeForUpdate l s) = show l ++ ": " ++ show s
{-# LINE 52 "Complete.hs" #-}

{-# LINE 120 "Syntax.ag" #-}

transformBlock :: Block -> Block'
transformBlock (Block ss) = Block' $ transformBlockStmts ss

transformBlockStmts :: [BlockStmt] -> BlockStmts'
transformBlockStmts []     = Stmt' $ BlockStmt' $ Empty'
transformBlockStmts (s:[]) = Stmt' $ transformBlockStmt s
transformBlockStmts (s:ss) = Seq' (transformBlockStmts [s]) (transformBlockStmts ss)

transformBlockStmt :: BlockStmt -> BlockStmt'
transformBlockStmt (BlockStmt s)        = BlockStmt' $ transformStmt s
transformBlockStmt (LocalVars ms ty vs) = LocalVars' ms ty (map transformVarDecl vs)

transformVarDecl :: VarDecl -> VarDecl'
transformVarDecl (VarDecl name (Just init)) = VarDecl' name $ Just (transformVarInit init)
transformVarDecl (VarDecl name Nothing)     = VarDecl' name Nothing

transformVarInit :: VarInit -> VarInit'
transformVarInit (InitExp e)      = InitExp' e
transformVarInit (InitArray init) = InitArray' $ transformArrayInit init

transformArrayInit :: ArrayInit -> ArrayInit'
transformArrayInit (ArrayInit inits) = ArrayInit' $ map transformVarInit inits

transformStmt :: Stmt -> Stmt'
--transformStmt (StmtBlock (Block [])) = Empty'
transformStmt (StmtBlock b)          = StmtBlock' $ transformBlock b
transformStmt (IfThen e s)           = IfThen' e $ transformStmt s 
transformStmt (IfThenElse e s1 s2)   = IfThenElse' e (transformStmt s1) (transformStmt s2) 
transformStmt (While e b)            = While' e $ transformStmt b
transformStmt (BasicFor i g u b)     = BasicFor' i g u $ transformStmt b
transformStmt Empty                  = Empty'
transformStmt (ExpStmt e)            = ExpStmt' e
transformStmt (Assert e error)       = Assert' e error
transformStmt (Break i)              = Break' i
transformStmt (Continue i)           = Continue' i
transformStmt (Return e)             = Return' e 
{-# LINE 92 "Complete.hs" #-}
-- ArrayInit' --------------------------------------------------
data ArrayInit' = ArrayInit' (VarInits')
                deriving ( Eq,Show)
-- cata
sem_ArrayInit' :: (ArrayInit') ->
                  (T_ArrayInit')
sem_ArrayInit' (ArrayInit' _inits) =
    (sem_ArrayInit'_ArrayInit' (sem_VarInits' _inits))
-- semantic domain
type T_ArrayInit' = ( ArrayInit')
data Inh_ArrayInit' = Inh_ArrayInit' {}
data Syn_ArrayInit' = Syn_ArrayInit' {self_Syn_ArrayInit' :: ArrayInit'}
wrap_ArrayInit' :: (T_ArrayInit') ->
                   (Inh_ArrayInit') ->
                   (Syn_ArrayInit')
wrap_ArrayInit' sem (Inh_ArrayInit') =
    (let ( _lhsOself) = sem
     in  (Syn_ArrayInit' _lhsOself))
sem_ArrayInit'_ArrayInit' :: (T_VarInits') ->
                             (T_ArrayInit')
sem_ArrayInit'_ArrayInit' inits_ =
    (let _lhsOself :: ArrayInit'
         _initsIself :: VarInits'
         _self =
             ArrayInit' _initsIself
         _lhsOself =
             _self
         ( _initsIself) =
             inits_
     in  ( _lhsOself))
-- Block' ------------------------------------------------------
data Block' = Block' (BlockStmts')
            deriving ( Eq,Show)
-- cata
sem_Block' :: (Block') ->
              (T_Block')
sem_Block' (Block' _blocks) =
    (sem_Block'_Block' (sem_BlockStmts' _blocks))
-- semantic domain
type T_Block' = Label ->
                ( Labels,Flow,Label,Label,Nodes,Block')
data Inh_Block' = Inh_Block' {label_Inh_Block' :: Label}
data Syn_Block' = Syn_Block' {final_Syn_Block' :: Labels,flow_Syn_Block' :: Flow,init_Syn_Block' :: Label,label_Syn_Block' :: Label,nodes_Syn_Block' :: Nodes,self_Syn_Block' :: Block'}
wrap_Block' :: (T_Block') ->
               (Inh_Block') ->
               (Syn_Block')
wrap_Block' sem (Inh_Block' _lhsIlabel) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself) = sem _lhsIlabel
     in  (Syn_Block' _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOnodes _lhsOself))
sem_Block'_Block' :: (T_BlockStmts') ->
                     (T_Block')
sem_Block'_Block' blocks_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: Block'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _blocksOlabel :: Label
              _blocksIfinal :: Labels
              _blocksIflow :: Flow
              _blocksIinit :: Label
              _blocksIlabel :: Label
              _blocksInodes :: Nodes
              _blocksIself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 25 "CFA.ag" #-}
                   _blocksIlabel
                   {-# LINE 162 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   _blocksIflow
                   {-# LINE 167 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 18 "CFA.ag" #-}
                   _blocksInodes
                   {-# LINE 172 "Complete.hs" #-}
                   )
              _self =
                  Block' _blocksIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 21 "CFA.ag" #-}
                   _blocksIfinal
                   {-# LINE 181 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 20 "CFA.ag" #-}
                   _blocksIinit
                   {-# LINE 186 "Complete.hs" #-}
                   )
              _blocksOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 191 "Complete.hs" #-}
                   )
              ( _blocksIfinal,_blocksIflow,_blocksIinit,_blocksIlabel,_blocksInodes,_blocksIself) =
                  blocks_ _blocksOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
-- BlockStmt' --------------------------------------------------
data BlockStmt' = BlockStmt' (Stmt')
                | LocalVars' (([Modifier])) (Type) (VarDecls')
                deriving ( Eq,Show)
-- cata
sem_BlockStmt' :: (BlockStmt') ->
                  (T_BlockStmt')
sem_BlockStmt' (BlockStmt' _stat) =
    (sem_BlockStmt'_BlockStmt' (sem_Stmt' _stat))
sem_BlockStmt' (LocalVars' _modifiers _ty _vars) =
    (sem_BlockStmt'_LocalVars' _modifiers _ty (sem_VarDecls' _vars))
-- semantic domain
type T_BlockStmt' = Label ->
                    ( Labels,Flow,Label,Label,Nodes,BlockStmt')
data Inh_BlockStmt' = Inh_BlockStmt' {label_Inh_BlockStmt' :: Label}
data Syn_BlockStmt' = Syn_BlockStmt' {final_Syn_BlockStmt' :: Labels,flow_Syn_BlockStmt' :: Flow,init_Syn_BlockStmt' :: Label,label_Syn_BlockStmt' :: Label,nodes_Syn_BlockStmt' :: Nodes,self_Syn_BlockStmt' :: BlockStmt'}
wrap_BlockStmt' :: (T_BlockStmt') ->
                   (Inh_BlockStmt') ->
                   (Syn_BlockStmt')
wrap_BlockStmt' sem (Inh_BlockStmt' _lhsIlabel) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself) = sem _lhsIlabel
     in  (Syn_BlockStmt' _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOnodes _lhsOself))
sem_BlockStmt'_BlockStmt' :: (T_Stmt') ->
                             (T_BlockStmt')
sem_BlockStmt'_BlockStmt' stat_ =
    (\ _lhsIlabel ->
         (let _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmt'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _lhsOlabel :: Label
              _statOlabel :: Label
              _statIfinal :: Labels
              _statIflow :: Flow
              _statIinit :: Label
              _statIlabel :: Label
              _statInodes :: Nodes
              _statIself :: Stmt'
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   _statIflow
                   {-# LINE 238 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 18 "CFA.ag" #-}
                   _statInodes
                   {-# LINE 243 "Complete.hs" #-}
                   )
              _self =
                  BlockStmt' _statIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 21 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 252 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 20 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 257 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 262 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 267 "Complete.hs" #-}
                   )
              ( _statIfinal,_statIflow,_statIinit,_statIlabel,_statInodes,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_BlockStmt'_LocalVars' :: ([Modifier]) ->
                             Type ->
                             (T_VarDecls') ->
                             (T_BlockStmt')
sem_BlockStmt'_LocalVars' modifiers_ ty_ vars_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: BlockStmt'
              _varsIself :: VarDecls'
              _lhsOlabel =
                  ({-# LINE 51 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 288 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 52 "CFA.ag" #-}
                   [NodeVarDecl (new _lhsIlabel) _self]
                   {-# LINE 293 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 298 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 54 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 303 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 308 "Complete.hs" #-}
                   )
              _self =
                  LocalVars' modifiers_ ty_ _varsIself
              _lhsOself =
                  _self
              ( _varsIself) =
                  vars_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
-- BlockStmts' -------------------------------------------------
data BlockStmts' = Seq' (BlockStmts') (BlockStmts')
                 | Stmt' (BlockStmt')
                 deriving ( Eq,Show)
-- cata
sem_BlockStmts' :: (BlockStmts') ->
                   (T_BlockStmts')
sem_BlockStmts' (Seq' _s1 _s2) =
    (sem_BlockStmts'_Seq' (sem_BlockStmts' _s1) (sem_BlockStmts' _s2))
sem_BlockStmts' (Stmt' _s) =
    (sem_BlockStmts'_Stmt' (sem_BlockStmt' _s))
-- semantic domain
type T_BlockStmts' = Label ->
                     ( Labels,Flow,Label,Label,Nodes,BlockStmts')
data Inh_BlockStmts' = Inh_BlockStmts' {label_Inh_BlockStmts' :: Label}
data Syn_BlockStmts' = Syn_BlockStmts' {final_Syn_BlockStmts' :: Labels,flow_Syn_BlockStmts' :: Flow,init_Syn_BlockStmts' :: Label,label_Syn_BlockStmts' :: Label,nodes_Syn_BlockStmts' :: Nodes,self_Syn_BlockStmts' :: BlockStmts'}
wrap_BlockStmts' :: (T_BlockStmts') ->
                    (Inh_BlockStmts') ->
                    (Syn_BlockStmts')
wrap_BlockStmts' sem (Inh_BlockStmts' _lhsIlabel) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself) = sem _lhsIlabel
     in  (Syn_BlockStmts' _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOnodes _lhsOself))
sem_BlockStmts'_Seq' :: (T_BlockStmts') ->
                        (T_BlockStmts') ->
                        (T_BlockStmts')
sem_BlockStmts'_Seq' s1_ s2_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmts'
              _s1Olabel :: Label
              _s2Olabel :: Label
              _s1Ifinal :: Labels
              _s1Iflow :: Flow
              _s1Iinit :: Label
              _s1Ilabel :: Label
              _s1Inodes :: Nodes
              _s1Iself :: BlockStmts'
              _s2Ifinal :: Labels
              _s2Iflow :: Flow
              _s2Iinit :: Label
              _s2Ilabel :: Label
              _s2Inodes :: Nodes
              _s2Iself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 28 "CFA.ag" #-}
                   maximum _s2Ifinal
                   {-# LINE 367 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 29 "CFA.ag" #-}
                   _s1Iinit
                   {-# LINE 372 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 30 "CFA.ag" #-}
                   _s2Ifinal
                   {-# LINE 377 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "CFA.ag" #-}
                   intraEdges _s1Ifinal _s2Iinit <> _s1Iflow <> _s2Iflow
                   {-# LINE 382 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 18 "CFA.ag" #-}
                   _s1Inodes ++ _s2Inodes
                   {-# LINE 387 "Complete.hs" #-}
                   )
              _self =
                  Seq' _s1Iself _s2Iself
              _lhsOself =
                  _self
              _s1Olabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 396 "Complete.hs" #-}
                   )
              _s2Olabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _s1Ilabel
                   {-# LINE 401 "Complete.hs" #-}
                   )
              ( _s1Ifinal,_s1Iflow,_s1Iinit,_s1Ilabel,_s1Inodes,_s1Iself) =
                  s1_ _s1Olabel
              ( _s2Ifinal,_s2Iflow,_s2Iinit,_s2Ilabel,_s2Inodes,_s2Iself) =
                  s2_ _s2Olabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_BlockStmts'_Stmt' :: (T_BlockStmt') ->
                         (T_BlockStmts')
sem_BlockStmts'_Stmt' s_ =
    (\ _lhsIlabel ->
         (let _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmts'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _lhsOlabel :: Label
              _sOlabel :: Label
              _sIfinal :: Labels
              _sIflow :: Flow
              _sIinit :: Label
              _sIlabel :: Label
              _sInodes :: Nodes
              _sIself :: BlockStmt'
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   _sIflow
                   {-# LINE 428 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 18 "CFA.ag" #-}
                   _sInodes
                   {-# LINE 433 "Complete.hs" #-}
                   )
              _self =
                  Stmt' _sIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 21 "CFA.ag" #-}
                   _sIfinal
                   {-# LINE 442 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 20 "CFA.ag" #-}
                   _sIinit
                   {-# LINE 447 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _sIlabel
                   {-# LINE 452 "Complete.hs" #-}
                   )
              _sOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 457 "Complete.hs" #-}
                   )
              ( _sIfinal,_sIflow,_sIinit,_sIlabel,_sInodes,_sIself) =
                  s_ _sOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
-- Node --------------------------------------------------------
data Node = NodeStmt (Label) (Stmt')
          | NodeVarDecl (Label) (BlockStmt')
          | NodeForInit (Label) (ForInit)
          | NodeForUpdate (Label) (([Exp]))
          deriving ( Eq)
-- cata
sem_Node :: Node ->
            T_Node
sem_Node (NodeStmt _label _value) =
    (sem_Node_NodeStmt _label (sem_Stmt' _value))
sem_Node (NodeVarDecl _label _value) =
    (sem_Node_NodeVarDecl _label (sem_BlockStmt' _value))
sem_Node (NodeForInit _label _value) =
    (sem_Node_NodeForInit _label _value)
sem_Node (NodeForUpdate _label _value) =
    (sem_Node_NodeForUpdate _label _value)
-- semantic domain
type T_Node = ( Node)
data Inh_Node = Inh_Node {}
data Syn_Node = Syn_Node {self_Syn_Node :: Node}
wrap_Node :: T_Node ->
             Inh_Node ->
             Syn_Node
wrap_Node sem (Inh_Node) =
    (let ( _lhsOself) = sem
     in  (Syn_Node _lhsOself))
sem_Node_NodeStmt :: Label ->
                     (T_Stmt') ->
                     T_Node
sem_Node_NodeStmt label_ value_ =
    (let _lhsOself :: Node
         _valueOlabel :: Label
         _valueIfinal :: Labels
         _valueIflow :: Flow
         _valueIinit :: Label
         _valueIlabel :: Label
         _valueInodes :: Nodes
         _valueIself :: Stmt'
         _self =
             NodeStmt label_ _valueIself
         _lhsOself =
             _self
         _valueOlabel =
             ({-# LINE 22 "CFA.ag" #-}
              label_
              {-# LINE 508 "Complete.hs" #-}
              )
         ( _valueIfinal,_valueIflow,_valueIinit,_valueIlabel,_valueInodes,_valueIself) =
             value_ _valueOlabel
     in  ( _lhsOself))
sem_Node_NodeVarDecl :: Label ->
                        (T_BlockStmt') ->
                        T_Node
sem_Node_NodeVarDecl label_ value_ =
    (let _lhsOself :: Node
         _valueOlabel :: Label
         _valueIfinal :: Labels
         _valueIflow :: Flow
         _valueIinit :: Label
         _valueIlabel :: Label
         _valueInodes :: Nodes
         _valueIself :: BlockStmt'
         _self =
             NodeVarDecl label_ _valueIself
         _lhsOself =
             _self
         _valueOlabel =
             ({-# LINE 22 "CFA.ag" #-}
              label_
              {-# LINE 532 "Complete.hs" #-}
              )
         ( _valueIfinal,_valueIflow,_valueIinit,_valueIlabel,_valueInodes,_valueIself) =
             value_ _valueOlabel
     in  ( _lhsOself))
sem_Node_NodeForInit :: Label ->
                        ForInit ->
                        T_Node
sem_Node_NodeForInit label_ value_ =
    (let _lhsOself :: Node
         _self =
             NodeForInit label_ value_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Node_NodeForUpdate :: Label ->
                          ([Exp]) ->
                          T_Node
sem_Node_NodeForUpdate label_ value_ =
    (let _lhsOself :: Node
         _self =
             NodeForUpdate label_ value_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Nodes -------------------------------------------------------
type Nodes = [Node]
-- cata
sem_Nodes :: Nodes ->
             T_Nodes
sem_Nodes list =
    (Prelude.foldr sem_Nodes_Cons sem_Nodes_Nil (Prelude.map sem_Node list))
-- semantic domain
type T_Nodes = ( Nodes)
data Inh_Nodes = Inh_Nodes {}
data Syn_Nodes = Syn_Nodes {self_Syn_Nodes :: Nodes}
wrap_Nodes :: T_Nodes ->
              Inh_Nodes ->
              Syn_Nodes
wrap_Nodes sem (Inh_Nodes) =
    (let ( _lhsOself) = sem
     in  (Syn_Nodes _lhsOself))
sem_Nodes_Cons :: T_Node ->
                  T_Nodes ->
                  T_Nodes
sem_Nodes_Cons hd_ tl_ =
    (let _lhsOself :: Nodes
         _hdIself :: Node
         _tlIself :: Nodes
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Nodes_Nil :: T_Nodes
sem_Nodes_Nil =
    (let _lhsOself :: Nodes
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Stmt' -------------------------------------------------------
data Stmt' = StmtBlock' (Block')
           | IfThen' (Exp) (Stmt')
           | IfThenElse' (Exp) (Stmt') (Stmt')
           | While' (Exp) (Stmt')
           | BasicFor' ((Maybe ForInit)) ((Maybe Exp)) ((Maybe [Exp])) (Stmt')
           | Empty'
           | ExpStmt' (Exp)
           | Assert' (Exp) ((Maybe Exp))
           | Break' ((Maybe Ident))
           | Continue' ((Maybe Ident))
           | Return' ((Maybe Exp))
           deriving ( Eq,Show)
-- cata
sem_Stmt' :: (Stmt') ->
             (T_Stmt')
sem_Stmt' (StmtBlock' _block) =
    (sem_Stmt'_StmtBlock' (sem_Block' _block))
sem_Stmt' (IfThen' _exp _stat) =
    (sem_Stmt'_IfThen' _exp (sem_Stmt' _stat))
sem_Stmt' (IfThenElse' _exp _stat1 _stat2) =
    (sem_Stmt'_IfThenElse' _exp (sem_Stmt' _stat1) (sem_Stmt' _stat2))
sem_Stmt' (While' _exp _body) =
    (sem_Stmt'_While' _exp (sem_Stmt' _body))
sem_Stmt' (BasicFor' _init _guard _update _body) =
    (sem_Stmt'_BasicFor' _init _guard _update (sem_Stmt' _body))
sem_Stmt' (Empty') =
    (sem_Stmt'_Empty')
sem_Stmt' (ExpStmt' _exp) =
    (sem_Stmt'_ExpStmt' _exp)
sem_Stmt' (Assert' _exp _error) =
    (sem_Stmt'_Assert' _exp _error)
sem_Stmt' (Break' _ident) =
    (sem_Stmt'_Break' _ident)
sem_Stmt' (Continue' _ident) =
    (sem_Stmt'_Continue' _ident)
sem_Stmt' (Return' _exp) =
    (sem_Stmt'_Return' _exp)
-- semantic domain
type T_Stmt' = Label ->
               ( Labels,Flow,Label,Label,Nodes,Stmt')
data Inh_Stmt' = Inh_Stmt' {label_Inh_Stmt' :: Label}
data Syn_Stmt' = Syn_Stmt' {final_Syn_Stmt' :: Labels,flow_Syn_Stmt' :: Flow,init_Syn_Stmt' :: Label,label_Syn_Stmt' :: Label,nodes_Syn_Stmt' :: Nodes,self_Syn_Stmt' :: Stmt'}
wrap_Stmt' :: (T_Stmt') ->
              (Inh_Stmt') ->
              (Syn_Stmt')
wrap_Stmt' sem (Inh_Stmt' _lhsIlabel) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself) = sem _lhsIlabel
     in  (Syn_Stmt' _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOnodes _lhsOself))
sem_Stmt'_StmtBlock' :: (T_Block') ->
                        (T_Stmt')
sem_Stmt'_StmtBlock' block_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: Stmt'
              _blockOlabel :: Label
              _blockIfinal :: Labels
              _blockIflow :: Flow
              _blockIinit :: Label
              _blockIlabel :: Label
              _blockInodes :: Nodes
              _blockIself :: Block'
              _lhsOlabel =
                  ({-# LINE 57 "CFA.ag" #-}
                   maximum _blockIfinal
                   {-# LINE 667 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "CFA.ag" #-}
                   _blockIinit
                   {-# LINE 672 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 59 "CFA.ag" #-}
                   _blockIfinal
                   {-# LINE 677 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 60 "CFA.ag" #-}
                   _blockIflow
                   {-# LINE 682 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 18 "CFA.ag" #-}
                   _blockInodes
                   {-# LINE 687 "Complete.hs" #-}
                   )
              _self =
                  StmtBlock' _blockIself
              _lhsOself =
                  _self
              _blockOlabel =
                  ({-# LINE 22 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 696 "Complete.hs" #-}
                   )
              ( _blockIfinal,_blockIflow,_blockIinit,_blockIlabel,_blockInodes,_blockIself) =
                  block_ _blockOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_IfThen' :: Exp ->
                     (T_Stmt') ->
                     (T_Stmt')
sem_Stmt'_IfThen' exp_ stat_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _statOlabel :: Label
              _lhsOself :: Stmt'
              _statIfinal :: Labels
              _statIflow :: Flow
              _statIinit :: Label
              _statIlabel :: Label
              _statInodes :: Nodes
              _statIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 62 "CFA.ag" #-}
                   maximum _statIfinal
                   {-# LINE 722 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 63 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : _statInodes
                   {-# LINE 727 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 732 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 65 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 737 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 66 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _statIinit) <> _statIflow
                   {-# LINE 742 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 67 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 747 "Complete.hs" #-}
                   )
              _self =
                  IfThen' exp_ _statIself
              _lhsOself =
                  _self
              ( _statIfinal,_statIflow,_statIinit,_statIlabel,_statInodes,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_IfThenElse' :: Exp ->
                         (T_Stmt') ->
                         (T_Stmt') ->
                         (T_Stmt')
sem_Stmt'_IfThenElse' exp_ stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOself :: Stmt'
              _stat1Ifinal :: Labels
              _stat1Iflow :: Flow
              _stat1Iinit :: Label
              _stat1Ilabel :: Label
              _stat1Inodes :: Nodes
              _stat1Iself :: Stmt'
              _stat2Ifinal :: Labels
              _stat2Iflow :: Flow
              _stat2Iinit :: Label
              _stat2Ilabel :: Label
              _stat2Inodes :: Nodes
              _stat2Iself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 69 "CFA.ag" #-}
                   maximum _stat2Ifinal
                   {-# LINE 785 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 70 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : (_stat1Inodes ++ _stat2Inodes)
                   {-# LINE 790 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 71 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 795 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "CFA.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 800 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 73 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _stat1Iinit) <>
                   (intraEdge (new _lhsIlabel) _stat2Iinit) <>
                   _stat1Iflow                              <>
                   _stat2Iflow
                   {-# LINE 808 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 77 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 813 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 78 "CFA.ag" #-}
                   maximum _stat1Ifinal
                   {-# LINE 818 "Complete.hs" #-}
                   )
              _self =
                  IfThenElse' exp_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Ilabel,_stat1Inodes,_stat1Iself) =
                  stat1_ _stat1Olabel
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Ilabel,_stat2Inodes,_stat2Iself) =
                  stat2_ _stat2Olabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_While' :: Exp ->
                    (T_Stmt') ->
                    (T_Stmt')
sem_Stmt'_While' exp_ body_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _bodyOlabel :: Label
              _lhsOself :: Stmt'
              _bodyIfinal :: Labels
              _bodyIflow :: Flow
              _bodyIinit :: Label
              _bodyIlabel :: Label
              _bodyInodes :: Nodes
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 80 "CFA.ag" #-}
                   maximum _bodyIfinal
                   {-# LINE 850 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 81 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : _bodyInodes
                   {-# LINE 855 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 82 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 860 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 83 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 865 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 84 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _bodyIinit)   <>
                   _bodyIflow                                <>
                   (intraEdges _bodyIfinal (new _lhsIlabel))
                   {-# LINE 872 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 87 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 877 "Complete.hs" #-}
                   )
              _self =
                  While' exp_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIfinal,_bodyIflow,_bodyIinit,_bodyIlabel,_bodyInodes,_bodyIself) =
                  body_ _bodyOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_BasicFor' :: (Maybe ForInit) ->
                       (Maybe Exp) ->
                       (Maybe [Exp]) ->
                       (T_Stmt') ->
                       (T_Stmt')
sem_Stmt'_BasicFor' init_ guard_ update_ body_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _bodyOlabel :: Label
              _lhsOself :: Stmt'
              _bodyIfinal :: Labels
              _bodyIflow :: Flow
              _bodyIinit :: Label
              _bodyIlabel :: Label
              _bodyInodes :: Nodes
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 89 "CFA.ag" #-}
                   maximum _bodyIfinal
                   {-# LINE 909 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 90 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]              ++
                   (forInitNode (new $ new _lhsIlabel) init_) ++
                   _bodyInodes                                ++
                   (forUpdateNode (new $ new $ newIfJust init_ _lhsIlabel) update_)
                   {-# LINE 917 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 94 "CFA.ag" #-}
                   new $ newIfJust init_ _lhsIlabel
                   {-# LINE 922 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 927 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 96 "CFA.ag" #-}
                   maybe mempty (const $ intraEdge (new $ new _lhsIlabel) (new _lhsIlabel)) init_ <>
                   intraEdge (new _lhsIlabel) _bodyIinit <>
                   intraEdges _bodyIfinal (maybe (new _lhsIlabel) (const $ new $ new $ newIfJust init_ _lhsIlabel) update_) <>
                   maybe mempty (const $ intraEdge (new $ new $ newIfJust init_ _lhsIlabel) (new _lhsIlabel)) update_ <>
                   _bodyIflow
                   {-# LINE 936 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 101 "CFA.ag" #-}
                   new $ newIfJust init_ $ newIfJust update_ _lhsIlabel
                   {-# LINE 941 "Complete.hs" #-}
                   )
              _self =
                  BasicFor' init_ guard_ update_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIfinal,_bodyIflow,_bodyIinit,_bodyIlabel,_bodyInodes,_bodyIself) =
                  body_ _bodyOlabel
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_Empty' :: (T_Stmt')
sem_Stmt'_Empty' =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 103 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 962 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 104 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 967 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 105 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 972 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 106 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 977 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 982 "Complete.hs" #-}
                   )
              _self =
                  Empty'
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_ExpStmt' :: Exp ->
                      (T_Stmt')
sem_Stmt'_ExpStmt' exp_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 108 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1002 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 109 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1007 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 110 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1012 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 111 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1017 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 1022 "Complete.hs" #-}
                   )
              _self =
                  ExpStmt' exp_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_Assert' :: Exp ->
                     (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Assert' exp_ error_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 113 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1043 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 114 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1048 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 115 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1053 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 116 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1058 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 1063 "Complete.hs" #-}
                   )
              _self =
                  Assert' exp_ error_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_Break' :: (Maybe Ident) ->
                    (T_Stmt')
sem_Stmt'_Break' ident_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 118 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1083 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 119 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1088 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 120 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1093 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 121 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1098 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 1103 "Complete.hs" #-}
                   )
              _self =
                  Break' ident_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_Continue' :: (Maybe Ident) ->
                       (T_Stmt')
sem_Stmt'_Continue' ident_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 123 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1123 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 124 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1128 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 125 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1133 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 126 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1138 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 1143 "Complete.hs" #-}
                   )
              _self =
                  Continue' ident_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
sem_Stmt'_Return' :: (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Return' exp_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Label
              _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 128 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1163 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 129 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1168 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 130 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1173 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 131 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1178 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 19 "CFA.ag" #-}
                   mempty
                   {-# LINE 1183 "Complete.hs" #-}
                   )
              _self =
                  Return' exp_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOnodes,_lhsOself)))
-- VarDecl' ----------------------------------------------------
data VarDecl' = VarDecl' (VarDeclId) (((Maybe VarInit')))
              deriving ( Eq,Show)
-- cata
sem_VarDecl' :: (VarDecl') ->
                (T_VarDecl')
sem_VarDecl' (VarDecl' _name _init) =
    (sem_VarDecl'_VarDecl' _name _init)
-- semantic domain
type T_VarDecl' = ( VarDecl')
data Inh_VarDecl' = Inh_VarDecl' {}
data Syn_VarDecl' = Syn_VarDecl' {self_Syn_VarDecl' :: VarDecl'}
wrap_VarDecl' :: (T_VarDecl') ->
                 (Inh_VarDecl') ->
                 (Syn_VarDecl')
wrap_VarDecl' sem (Inh_VarDecl') =
    (let ( _lhsOself) = sem
     in  (Syn_VarDecl' _lhsOself))
sem_VarDecl'_VarDecl' :: VarDeclId ->
                         ((Maybe VarInit')) ->
                         (T_VarDecl')
sem_VarDecl'_VarDecl' name_ init_ =
    (let _lhsOself :: VarDecl'
         _self =
             VarDecl' name_ init_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- VarDecls' ---------------------------------------------------
type VarDecls' = [VarDecl']
-- cata
sem_VarDecls' :: (VarDecls') ->
                 (T_VarDecls')
sem_VarDecls' list =
    (Prelude.foldr sem_VarDecls'_Cons sem_VarDecls'_Nil (Prelude.map sem_VarDecl' list))
-- semantic domain
type T_VarDecls' = ( VarDecls')
data Inh_VarDecls' = Inh_VarDecls' {}
data Syn_VarDecls' = Syn_VarDecls' {self_Syn_VarDecls' :: VarDecls'}
wrap_VarDecls' :: (T_VarDecls') ->
                  (Inh_VarDecls') ->
                  (Syn_VarDecls')
wrap_VarDecls' sem (Inh_VarDecls') =
    (let ( _lhsOself) = sem
     in  (Syn_VarDecls' _lhsOself))
sem_VarDecls'_Cons :: (T_VarDecl') ->
                      (T_VarDecls') ->
                      (T_VarDecls')
sem_VarDecls'_Cons hd_ tl_ =
    (let _lhsOself :: VarDecls'
         _hdIself :: VarDecl'
         _tlIself :: VarDecls'
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_VarDecls'_Nil :: (T_VarDecls')
sem_VarDecls'_Nil =
    (let _lhsOself :: VarDecls'
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- VarInit' ----------------------------------------------------
data VarInit' = InitExp' (Exp)
              | InitArray' (ArrayInit')
              deriving ( Eq,Show)
-- cata
sem_VarInit' :: (VarInit') ->
                (T_VarInit')
sem_VarInit' (InitExp' _exp) =
    (sem_VarInit'_InitExp' _exp)
sem_VarInit' (InitArray' _init) =
    (sem_VarInit'_InitArray' (sem_ArrayInit' _init))
-- semantic domain
type T_VarInit' = ( VarInit')
data Inh_VarInit' = Inh_VarInit' {}
data Syn_VarInit' = Syn_VarInit' {self_Syn_VarInit' :: VarInit'}
wrap_VarInit' :: (T_VarInit') ->
                 (Inh_VarInit') ->
                 (Syn_VarInit')
wrap_VarInit' sem (Inh_VarInit') =
    (let ( _lhsOself) = sem
     in  (Syn_VarInit' _lhsOself))
sem_VarInit'_InitExp' :: Exp ->
                         (T_VarInit')
sem_VarInit'_InitExp' exp_ =
    (let _lhsOself :: VarInit'
         _self =
             InitExp' exp_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_VarInit'_InitArray' :: (T_ArrayInit') ->
                           (T_VarInit')
sem_VarInit'_InitArray' init_ =
    (let _lhsOself :: VarInit'
         _initIself :: ArrayInit'
         _self =
             InitArray' _initIself
         _lhsOself =
             _self
         ( _initIself) =
             init_
     in  ( _lhsOself))
-- VarInits' ---------------------------------------------------
type VarInits' = [VarInit']
-- cata
sem_VarInits' :: (VarInits') ->
                 (T_VarInits')
sem_VarInits' list =
    (Prelude.foldr sem_VarInits'_Cons sem_VarInits'_Nil (Prelude.map sem_VarInit' list))
-- semantic domain
type T_VarInits' = ( VarInits')
data Inh_VarInits' = Inh_VarInits' {}
data Syn_VarInits' = Syn_VarInits' {self_Syn_VarInits' :: VarInits'}
wrap_VarInits' :: (T_VarInits') ->
                  (Inh_VarInits') ->
                  (Syn_VarInits')
wrap_VarInits' sem (Inh_VarInits') =
    (let ( _lhsOself) = sem
     in  (Syn_VarInits' _lhsOself))
sem_VarInits'_Cons :: (T_VarInit') ->
                      (T_VarInits') ->
                      (T_VarInits')
sem_VarInits'_Cons hd_ tl_ =
    (let _lhsOself :: VarInits'
         _hdIself :: VarInit'
         _tlIself :: VarInits'
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_VarInits'_Nil :: (T_VarInits')
sem_VarInits'_Nil =
    (let _lhsOself :: VarInits'
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))