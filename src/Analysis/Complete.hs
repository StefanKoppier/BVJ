

-- UUAGC 0.9.52.1 (Complete.ag)
module Analysis.Complete where

import Language.Java.Syntax
import Analysis.Flow

import           Data.Maybe
import qualified Data.Map   as M

import Debug.Trace

{-# LINE 17 "CFA.ag" #-}

type Environment a = M.Map Ident a

unlabeled :: Ident
unlabeled = Ident "$loop$"
{-# LINE 21 "Complete.hs" #-}

{-# LINE 153 "CFA.ag" #-}

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
        initial = Inh_Block' { label_Inh_Block'     = 0
                             , labelings_Inh_Block' = M.empty } 
        
nodesOfBlock :: Block' -> Nodes
nodesOfBlock block
    = nodes_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block'     = 0
                             , labelings_Inh_Block' = M.empty } 

instance Show Node where
    show (NodeStmt l s)      = show l ++ ": " ++ show s
    show (NodeVarDecl l s)   = show l ++ ": " ++ show s
    show (NodeForInit l s)   = show l ++ ": " ++ show s
    show (NodeForUpdate l s) = show l ++ ": " ++ show s
{-# LINE 65 "Complete.hs" #-}

{-# LINE 121 "Syntax.ag" #-}

transformBlock :: Block -> Block'
transformBlock (Block ss) = Block' $ transformBlockStmts ss

transformBlockStmts :: [BlockStmt] -> BlockStmts'
transformBlockStmts []     = Single' $ BlockStmt' $ Empty'
transformBlockStmts (s:[]) = Single' $ transformBlockStmt s
transformBlockStmts (s:ss) = Seq' (transformBlockStmt s) (transformBlockStmts ss)

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
transformStmt (StmtBlock b)        = StmtBlock' $ transformBlock b
transformStmt (IfThen e s)         = IfThen' e $ transformStmt s 
transformStmt (IfThenElse e s1 s2) = IfThenElse' e (transformStmt s1) (transformStmt s2) 
transformStmt (While e b)          = While' e $ transformStmt b
transformStmt (BasicFor i g u b)   = BasicFor' i g u $ transformStmt b
transformStmt Empty                = Empty'
transformStmt (ExpStmt e)          = ExpStmt' e
transformStmt (Assert e error)     = Assert' e error
transformStmt (Break i)            = Break' i
transformStmt (Continue i)         = Continue' i
transformStmt (Return e)           = Return' e 
transformStmt (Labeled l s)        = Labeled' l $ transformStmt s 
{-# LINE 105 "Complete.hs" #-}
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
                (Environment (Label, Label)) ->
                ( Labels,Flow,Label,Nodes,Block')
data Inh_Block' = Inh_Block' {label_Inh_Block' :: Label,labelings_Inh_Block' :: (Environment (Label, Label))}
data Syn_Block' = Syn_Block' {final_Syn_Block' :: Labels,flow_Syn_Block' :: Flow,init_Syn_Block' :: Label,nodes_Syn_Block' :: Nodes,self_Syn_Block' :: Block'}
wrap_Block' :: (T_Block') ->
               (Inh_Block') ->
               (Syn_Block')
wrap_Block' sem (Inh_Block' _lhsIlabel _lhsIlabelings) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself) = sem _lhsIlabel _lhsIlabelings
     in  (Syn_Block' _lhsOfinal _lhsOflow _lhsOinit _lhsOnodes _lhsOself))
sem_Block'_Block' :: (T_BlockStmts') ->
                     (T_Block')
sem_Block'_Block' blocks_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: Block'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _blocksOlabel :: Label
              _blocksOlabelings :: (Environment (Label, Label))
              _blocksIfinal :: Labels
              _blocksIflow :: Flow
              _blocksIinit :: Label
              _blocksInodes :: Nodes
              _blocksIself :: BlockStmts'
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   _blocksIflow
                   {-# LINE 176 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "CFA.ag" #-}
                   _blocksInodes
                   {-# LINE 181 "Complete.hs" #-}
                   )
              _self =
                  Block' _blocksIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 28 "CFA.ag" #-}
                   _blocksIfinal
                   {-# LINE 190 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 27 "CFA.ag" #-}
                   _blocksIinit
                   {-# LINE 195 "Complete.hs" #-}
                   )
              _blocksOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 200 "Complete.hs" #-}
                   )
              _blocksOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 205 "Complete.hs" #-}
                   )
              ( _blocksIfinal,_blocksIflow,_blocksIinit,_blocksInodes,_blocksIself) =
                  blocks_ _blocksOlabel _blocksOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
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
                    (Environment (Label, Label)) ->
                    ( Labels,Flow,Label,Nodes,BlockStmt')
data Inh_BlockStmt' = Inh_BlockStmt' {label_Inh_BlockStmt' :: Label,labelings_Inh_BlockStmt' :: (Environment (Label, Label))}
data Syn_BlockStmt' = Syn_BlockStmt' {final_Syn_BlockStmt' :: Labels,flow_Syn_BlockStmt' :: Flow,init_Syn_BlockStmt' :: Label,nodes_Syn_BlockStmt' :: Nodes,self_Syn_BlockStmt' :: BlockStmt'}
wrap_BlockStmt' :: (T_BlockStmt') ->
                   (Inh_BlockStmt') ->
                   (Syn_BlockStmt')
wrap_BlockStmt' sem (Inh_BlockStmt' _lhsIlabel _lhsIlabelings) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself) = sem _lhsIlabel _lhsIlabelings
     in  (Syn_BlockStmt' _lhsOfinal _lhsOflow _lhsOinit _lhsOnodes _lhsOself))
sem_BlockStmt'_BlockStmt' :: (T_Stmt') ->
                             (T_BlockStmt')
sem_BlockStmt'_BlockStmt' stat_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmt'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _statOlabel :: Label
              _statOlabelings :: (Environment (Label, Label))
              _statIfinal :: Labels
              _statIflow :: Flow
              _statIinit :: Label
              _statInodes :: Nodes
              _statIself :: Stmt'
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   _statIflow
                   {-# LINE 253 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "CFA.ag" #-}
                   _statInodes
                   {-# LINE 258 "Complete.hs" #-}
                   )
              _self =
                  BlockStmt' _statIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 28 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 267 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 27 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 272 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 277 "Complete.hs" #-}
                   )
              _statOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 282 "Complete.hs" #-}
                   )
              ( _statIfinal,_statIflow,_statIinit,_statInodes,_statIself) =
                  stat_ _statOlabel _statOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_BlockStmt'_LocalVars' :: ([Modifier]) ->
                             Type ->
                             (T_VarDecls') ->
                             (T_BlockStmt')
sem_BlockStmt'_LocalVars' modifiers_ ty_ vars_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: BlockStmt'
              _varsIself :: VarDecls'
              _lhsOnodes =
                  ({-# LINE 65 "CFA.ag" #-}
                   [NodeVarDecl (new _lhsIlabel) _self]
                   {-# LINE 303 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 66 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 308 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 67 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 313 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 318 "Complete.hs" #-}
                   )
              _self =
                  LocalVars' modifiers_ ty_ _varsIself
              _lhsOself =
                  _self
              ( _varsIself) =
                  vars_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
-- BlockStmts' -------------------------------------------------
data BlockStmts' = Seq' (BlockStmt') (BlockStmts')
                 | Single' (BlockStmt')
                 deriving ( Eq,Show)
-- cata
sem_BlockStmts' :: (BlockStmts') ->
                   (T_BlockStmts')
sem_BlockStmts' (Seq' _s1 _s2) =
    (sem_BlockStmts'_Seq' (sem_BlockStmt' _s1) (sem_BlockStmts' _s2))
sem_BlockStmts' (Single' _s) =
    (sem_BlockStmts'_Single' (sem_BlockStmt' _s))
-- semantic domain
type T_BlockStmts' = Label ->
                     (Environment (Label, Label)) ->
                     ( Labels,Flow,Label,Nodes,BlockStmts')
data Inh_BlockStmts' = Inh_BlockStmts' {label_Inh_BlockStmts' :: Label,labelings_Inh_BlockStmts' :: (Environment (Label, Label))}
data Syn_BlockStmts' = Syn_BlockStmts' {final_Syn_BlockStmts' :: Labels,flow_Syn_BlockStmts' :: Flow,init_Syn_BlockStmts' :: Label,nodes_Syn_BlockStmts' :: Nodes,self_Syn_BlockStmts' :: BlockStmts'}
wrap_BlockStmts' :: (T_BlockStmts') ->
                    (Inh_BlockStmts') ->
                    (Syn_BlockStmts')
wrap_BlockStmts' sem (Inh_BlockStmts' _lhsIlabel _lhsIlabelings) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself) = sem _lhsIlabel _lhsIlabelings
     in  (Syn_BlockStmts' _lhsOfinal _lhsOflow _lhsOinit _lhsOnodes _lhsOself))
sem_BlockStmts'_Seq' :: (T_BlockStmt') ->
                        (T_BlockStmts') ->
                        (T_BlockStmts')
sem_BlockStmts'_Seq' s1_ s2_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _s1Olabel :: Label
              _s2Olabel :: Label
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmts'
              _s1Olabelings :: (Environment (Label, Label))
              _s2Olabelings :: (Environment (Label, Label))
              _s1Ifinal :: Labels
              _s1Iflow :: Flow
              _s1Iinit :: Label
              _s1Inodes :: Nodes
              _s1Iself :: BlockStmt'
              _s2Ifinal :: Labels
              _s2Iflow :: Flow
              _s2Iinit :: Label
              _s2Inodes :: Nodes
              _s2Iself :: BlockStmts'
              _lhsOinit =
                  ({-# LINE 37 "CFA.ag" #-}
                   _s1Iinit
                   {-# LINE 378 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 38 "CFA.ag" #-}
                   _s2Ifinal
                   {-# LINE 383 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 39 "CFA.ag" #-}
                   case _s1Iself of
                       BlockStmt' (Continue' Nothing)
                           -> intraEdges _s1Ifinal (fst $ _lhsIlabelings M.! unlabeled)
                       BlockStmt' (Continue' (Just l))
                           -> intraEdges _s1Ifinal (fst $ _lhsIlabelings M.! l)
                       BlockStmt' (Break' _)
                           -> intraEdges _s1Ifinal (snd $ _lhsIlabelings M.! unlabeled)
                       BlockStmt' (Break' (Just l))
                           -> intraEdges _s1Ifinal (snd $ _lhsIlabelings M.! l)
                       BlockStmt' (Return' _)
                           -> undefined
                       _   -> _s1Iflow <> intraEdges _s1Ifinal _s2Iinit <> _s2Iflow
                   {-# LINE 399 "Complete.hs" #-}
                   )
              _s1Olabel =
                  ({-# LINE 54 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 404 "Complete.hs" #-}
                   )
              _s2Olabel =
                  ({-# LINE 55 "CFA.ag" #-}
                   maximum _s1Ifinal
                   {-# LINE 409 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "CFA.ag" #-}
                   _s1Inodes ++ _s2Inodes
                   {-# LINE 414 "Complete.hs" #-}
                   )
              _self =
                  Seq' _s1Iself _s2Iself
              _lhsOself =
                  _self
              _s1Olabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 423 "Complete.hs" #-}
                   )
              _s2Olabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 428 "Complete.hs" #-}
                   )
              ( _s1Ifinal,_s1Iflow,_s1Iinit,_s1Inodes,_s1Iself) =
                  s1_ _s1Olabel _s1Olabelings
              ( _s2Ifinal,_s2Iflow,_s2Iinit,_s2Inodes,_s2Iself) =
                  s2_ _s2Olabel _s2Olabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_BlockStmts'_Single' :: (T_BlockStmt') ->
                           (T_BlockStmts')
sem_BlockStmts'_Single' s_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: BlockStmts'
              _lhsOfinal :: Labels
              _lhsOinit :: Label
              _sOlabel :: Label
              _sOlabelings :: (Environment (Label, Label))
              _sIfinal :: Labels
              _sIflow :: Flow
              _sIinit :: Label
              _sInodes :: Nodes
              _sIself :: BlockStmt'
              _lhsOflow =
                  ({-# LINE 56 "CFA.ag" #-}
                   case _sIself of
                      BlockStmt' (Continue' (Just l))
                          -> trace (show $ intraEdges _sIfinal (fst $ _lhsIlabelings M.! l)) $ intraEdges _sIfinal (fst $ _lhsIlabelings M.! l)
                      BlockStmt' (Break' (Just l))
                          -> intraEdges _sIfinal (snd $ _lhsIlabelings M.! l)
                      _   -> _sIflow
                   {-# LINE 460 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "CFA.ag" #-}
                   _sInodes
                   {-# LINE 465 "Complete.hs" #-}
                   )
              _self =
                  Single' _sIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 28 "CFA.ag" #-}
                   _sIfinal
                   {-# LINE 474 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 27 "CFA.ag" #-}
                   _sIinit
                   {-# LINE 479 "Complete.hs" #-}
                   )
              _sOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 484 "Complete.hs" #-}
                   )
              _sOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 489 "Complete.hs" #-}
                   )
              ( _sIfinal,_sIflow,_sIinit,_sInodes,_sIself) =
                  s_ _sOlabel _sOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
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
         _valueOlabelings :: (Environment (Label, Label))
         _valueIfinal :: Labels
         _valueIflow :: Flow
         _valueIinit :: Label
         _valueInodes :: Nodes
         _valueIself :: Stmt'
         _self =
             NodeStmt label_ _valueIself
         _lhsOself =
             _self
         _valueOlabel =
             ({-# LINE 29 "CFA.ag" #-}
              label_
              {-# LINE 540 "Complete.hs" #-}
              )
         _valueOlabelings =
             ({-# LINE 30 "CFA.ag" #-}
              error "missing rule: Node.NodeStmt.value.labelings"
              {-# LINE 545 "Complete.hs" #-}
              )
         ( _valueIfinal,_valueIflow,_valueIinit,_valueInodes,_valueIself) =
             value_ _valueOlabel _valueOlabelings
     in  ( _lhsOself))
sem_Node_NodeVarDecl :: Label ->
                        (T_BlockStmt') ->
                        T_Node
sem_Node_NodeVarDecl label_ value_ =
    (let _lhsOself :: Node
         _valueOlabel :: Label
         _valueOlabelings :: (Environment (Label, Label))
         _valueIfinal :: Labels
         _valueIflow :: Flow
         _valueIinit :: Label
         _valueInodes :: Nodes
         _valueIself :: BlockStmt'
         _self =
             NodeVarDecl label_ _valueIself
         _lhsOself =
             _self
         _valueOlabel =
             ({-# LINE 29 "CFA.ag" #-}
              label_
              {-# LINE 569 "Complete.hs" #-}
              )
         _valueOlabelings =
             ({-# LINE 30 "CFA.ag" #-}
              error "missing rule: Node.NodeVarDecl.value.labelings"
              {-# LINE 574 "Complete.hs" #-}
              )
         ( _valueIfinal,_valueIflow,_valueIinit,_valueInodes,_valueIself) =
             value_ _valueOlabel _valueOlabelings
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
           | Labeled' (Ident) (Stmt')
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
sem_Stmt' (Labeled' _ident _stat) =
    (sem_Stmt'_Labeled' _ident (sem_Stmt' _stat))
-- semantic domain
type T_Stmt' = Label ->
               (Environment (Label, Label)) ->
               ( Labels,Flow,Label,Nodes,Stmt')
data Inh_Stmt' = Inh_Stmt' {label_Inh_Stmt' :: Label,labelings_Inh_Stmt' :: (Environment (Label, Label))}
data Syn_Stmt' = Syn_Stmt' {final_Syn_Stmt' :: Labels,flow_Syn_Stmt' :: Flow,init_Syn_Stmt' :: Label,nodes_Syn_Stmt' :: Nodes,self_Syn_Stmt' :: Stmt'}
wrap_Stmt' :: (T_Stmt') ->
              (Inh_Stmt') ->
              (Syn_Stmt')
wrap_Stmt' sem (Inh_Stmt' _lhsIlabel _lhsIlabelings) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself) = sem _lhsIlabel _lhsIlabelings
     in  (Syn_Stmt' _lhsOfinal _lhsOflow _lhsOinit _lhsOnodes _lhsOself))
sem_Stmt'_StmtBlock' :: (T_Block') ->
                        (T_Stmt')
sem_Stmt'_StmtBlock' block_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOnodes :: Nodes
              _lhsOself :: Stmt'
              _blockOlabel :: Label
              _blockOlabelings :: (Environment (Label, Label))
              _blockIfinal :: Labels
              _blockIflow :: Flow
              _blockIinit :: Label
              _blockInodes :: Nodes
              _blockIself :: Block'
              _lhsOinit =
                  ({-# LINE 71 "CFA.ag" #-}
                   _blockIinit
                   {-# LINE 713 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "CFA.ag" #-}
                   _blockIfinal
                   {-# LINE 718 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 73 "CFA.ag" #-}
                   _blockIflow
                   {-# LINE 723 "Complete.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 25 "CFA.ag" #-}
                   _blockInodes
                   {-# LINE 728 "Complete.hs" #-}
                   )
              _self =
                  StmtBlock' _blockIself
              _lhsOself =
                  _self
              _blockOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 737 "Complete.hs" #-}
                   )
              _blockOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 742 "Complete.hs" #-}
                   )
              ( _blockIfinal,_blockIflow,_blockIinit,_blockInodes,_blockIself) =
                  block_ _blockOlabel _blockOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_IfThen' :: Exp ->
                     (T_Stmt') ->
                     (T_Stmt')
sem_Stmt'_IfThen' exp_ stat_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _statOlabel :: Label
              _lhsOself :: Stmt'
              _statOlabelings :: (Environment (Label, Label))
              _statIfinal :: Labels
              _statIflow :: Flow
              _statIinit :: Label
              _statInodes :: Nodes
              _statIself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 76 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : _statInodes
                   {-# LINE 768 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 77 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 773 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 78 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 778 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 79 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _statIinit) <> _statIflow
                   {-# LINE 783 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 80 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 788 "Complete.hs" #-}
                   )
              _self =
                  IfThen' exp_ _statIself
              _lhsOself =
                  _self
              _statOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 797 "Complete.hs" #-}
                   )
              ( _statIfinal,_statIflow,_statIinit,_statInodes,_statIself) =
                  stat_ _statOlabel _statOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_IfThenElse' :: Exp ->
                         (T_Stmt') ->
                         (T_Stmt') ->
                         (T_Stmt')
sem_Stmt'_IfThenElse' exp_ stat1_ stat2_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _stat1Olabel :: Label
              _stat2Olabel :: Label
              _lhsOself :: Stmt'
              _stat1Olabelings :: (Environment (Label, Label))
              _stat2Olabelings :: (Environment (Label, Label))
              _stat1Ifinal :: Labels
              _stat1Iflow :: Flow
              _stat1Iinit :: Label
              _stat1Inodes :: Nodes
              _stat1Iself :: Stmt'
              _stat2Ifinal :: Labels
              _stat2Iflow :: Flow
              _stat2Iinit :: Label
              _stat2Inodes :: Nodes
              _stat2Iself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 83 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : (_stat1Inodes ++ _stat2Inodes)
                   {-# LINE 831 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 84 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 836 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 85 "CFA.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 841 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 86 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _stat1Iinit) <>
                   (intraEdge (new _lhsIlabel) _stat2Iinit) <>
                   _stat1Iflow                              <>
                   _stat2Iflow
                   {-# LINE 849 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 90 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 854 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 91 "CFA.ag" #-}
                   maximum _stat1Ifinal
                   {-# LINE 859 "Complete.hs" #-}
                   )
              _self =
                  IfThenElse' exp_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _stat1Olabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 868 "Complete.hs" #-}
                   )
              _stat2Olabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 873 "Complete.hs" #-}
                   )
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Inodes,_stat1Iself) =
                  stat1_ _stat1Olabel _stat1Olabelings
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Inodes,_stat2Iself) =
                  stat2_ _stat2Olabel _stat2Olabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_While' :: Exp ->
                    (T_Stmt') ->
                    (T_Stmt')
sem_Stmt'_While' exp_ body_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _bodyOlabel :: Label
              _bodyOlabelings :: (Environment (Label, Label))
              _lhsOself :: Stmt'
              _bodyIfinal :: Labels
              _bodyIflow :: Flow
              _bodyIinit :: Label
              _bodyInodes :: Nodes
              _bodyIself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 94 "CFA.ag" #-}
                   (node (new _lhsIlabel) _self) : _bodyInodes
                   {-# LINE 901 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 95 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 906 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 96 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 911 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "CFA.ag" #-}
                   (intraEdge (new _lhsIlabel) _bodyIinit)   <>
                   _bodyIflow                                <>
                   (intraEdges _bodyIfinal (new _lhsIlabel))
                   {-# LINE 918 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 100 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 923 "Complete.hs" #-}
                   )
              _bodyOlabelings =
                  ({-# LINE 101 "CFA.ag" #-}
                   M.insert unlabeled (new _lhsIlabel, (-1)) _lhsIlabelings
                   {-# LINE 928 "Complete.hs" #-}
                   )
              _self =
                  While' exp_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIfinal,_bodyIflow,_bodyIinit,_bodyInodes,_bodyIself) =
                  body_ _bodyOlabel _bodyOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_BasicFor' :: (Maybe ForInit) ->
                       (Maybe Exp) ->
                       (Maybe [Exp]) ->
                       (T_Stmt') ->
                       (T_Stmt')
sem_Stmt'_BasicFor' init_ guard_ update_ body_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _bodyOlabel :: Label
              _lhsOself :: Stmt'
              _bodyOlabelings :: (Environment (Label, Label))
              _bodyIfinal :: Labels
              _bodyIflow :: Flow
              _bodyIinit :: Label
              _bodyInodes :: Nodes
              _bodyIself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 104 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]              ++
                   (forInitNode (new $ new _lhsIlabel) init_) ++
                   _bodyInodes                                ++
                   (forUpdateNode (new $ new $ newIfJust init_ _lhsIlabel) update_)
                   {-# LINE 963 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 108 "CFA.ag" #-}
                   new $ newIfJust init_ _lhsIlabel
                   {-# LINE 968 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 109 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 973 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 110 "CFA.ag" #-}
                   maybe mempty (const $ intraEdge (new $ new _lhsIlabel) (new _lhsIlabel)) init_ <>
                   intraEdge (new _lhsIlabel) _bodyIinit <>
                   intraEdges _bodyIfinal (maybe (new _lhsIlabel) (const $ new $ new $ newIfJust init_ _lhsIlabel) update_) <>
                   maybe mempty (const $ intraEdge (new $ new $ newIfJust init_ _lhsIlabel) (new _lhsIlabel)) update_ <>
                   _bodyIflow
                   {-# LINE 982 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 115 "CFA.ag" #-}
                   new $ newIfJust init_ $ newIfJust update_ _lhsIlabel
                   {-# LINE 987 "Complete.hs" #-}
                   )
              _self =
                  BasicFor' init_ guard_ update_ _bodyIself
              _lhsOself =
                  _self
              _bodyOlabelings =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIlabelings
                   {-# LINE 996 "Complete.hs" #-}
                   )
              ( _bodyIfinal,_bodyIflow,_bodyIinit,_bodyInodes,_bodyIself) =
                  body_ _bodyOlabel _bodyOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Empty' :: (T_Stmt')
sem_Stmt'_Empty' =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 118 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1013 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 119 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1018 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 120 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1023 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1028 "Complete.hs" #-}
                   )
              _self =
                  Empty'
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_ExpStmt' :: Exp ->
                      (T_Stmt')
sem_Stmt'_ExpStmt' exp_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 123 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1048 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 124 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1053 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 125 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1058 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1063 "Complete.hs" #-}
                   )
              _self =
                  ExpStmt' exp_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Assert' :: Exp ->
                     (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Assert' exp_ error_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 128 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1084 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 129 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1089 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 130 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1094 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1099 "Complete.hs" #-}
                   )
              _self =
                  Assert' exp_ error_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Break' :: (Maybe Ident) ->
                    (T_Stmt')
sem_Stmt'_Break' ident_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 133 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1119 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 134 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1124 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 135 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1129 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1134 "Complete.hs" #-}
                   )
              _self =
                  Break' ident_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Continue' :: (Maybe Ident) ->
                       (T_Stmt')
sem_Stmt'_Continue' ident_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 138 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1154 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 139 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1159 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 140 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1164 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1169 "Complete.hs" #-}
                   )
              _self =
                  Continue' ident_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Return' :: (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Return' exp_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 143 "CFA.ag" #-}
                   [node (new _lhsIlabel) _self]
                   {-# LINE 1189 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 144 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1194 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 145 "CFA.ag" #-}
                   [new _lhsIlabel]
                   {-# LINE 1199 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   mempty
                   {-# LINE 1204 "Complete.hs" #-}
                   )
              _self =
                  Return' exp_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
sem_Stmt'_Labeled' :: Ident ->
                      (T_Stmt') ->
                      (T_Stmt')
sem_Stmt'_Labeled' ident_ stat_ =
    (\ _lhsIlabel
       _lhsIlabelings ->
         (let _lhsOnodes :: Nodes
              _lhsOinit :: Label
              _lhsOfinal :: Labels
              _statOlabelings :: (Environment (Label, Label))
              _lhsOflow :: Flow
              _lhsOself :: Stmt'
              _statOlabel :: Label
              _statIfinal :: Labels
              _statIflow :: Flow
              _statIinit :: Label
              _statInodes :: Nodes
              _statIself :: Stmt'
              _lhsOnodes =
                  ({-# LINE 148 "CFA.ag" #-}
                   _statInodes
                   {-# LINE 1232 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 149 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 1237 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 150 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 1242 "Complete.hs" #-}
                   )
              _statOlabelings =
                  ({-# LINE 151 "CFA.ag" #-}
                   M.insert ident_ (_statIinit, (-1)) _lhsIlabelings
                   {-# LINE 1247 "Complete.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 26 "CFA.ag" #-}
                   _statIflow
                   {-# LINE 1252 "Complete.hs" #-}
                   )
              _self =
                  Labeled' ident_ _statIself
              _lhsOself =
                  _self
              _statOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 1261 "Complete.hs" #-}
                   )
              ( _statIfinal,_statIflow,_statIinit,_statInodes,_statIself) =
                  stat_ _statOlabel _statOlabelings
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOnodes,_lhsOself)))
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