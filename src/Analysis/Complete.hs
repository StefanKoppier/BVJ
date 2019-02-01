

-- UUAGC 0.9.52.1 (Complete.ag)
module Analysis.Complete where

import           Language.Java.Syntax
import           Data.Monoid
import qualified Data.Set                          as S
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

{-# LINE 1 "CFA.ag" #-}

data CFGNodeValue
    = NodeStmt      Stmt'
    | NodeVarDecl   BlockStmt'
    | NodeForInit   ForInit
    | NodeForUpdate [Exp]
    deriving (Show, Eq)
    
type CFGNode = LNode CFGNodeValue

instance {-# OVERLAPS #-} Ord CFGNode where
    (x,_) <= (y,_) = x <= y

type CFGNodes = S.Set CFGNode

type CFGEdge = UEdge

type CFGEdges = [CFGEdge]

type CFGContext = (Adj (), Node, CFGNodeValue, Adj ())

type CFG = Gr CFGNodeValue ()
{-# LINE 36 "Complete.hs" #-}

{-# LINE 167 "CFA.ag" #-}

new :: Node -> Node
new = (1+)

newIfJust :: Maybe a -> Node -> Node
newIfJust = maybe id (const new)

nodeStmt :: Node -> Stmt' -> CFGNode
nodeStmt l s = (l, NodeStmt s)

nodeVarDecl :: Node -> BlockStmt' -> CFGNode
nodeVarDecl l b = (l, NodeVarDecl b)

nodeForInit :: Node -> ForInit -> CFGNode
nodeForInit l i = (l, NodeForInit i)

nodeForUpdate :: Node -> [Exp] -> CFGNode
nodeForUpdate l u = (l, NodeForUpdate u)

intraEdge :: CFGNode -> CFGNode -> CFGEdge
intraEdge (x,_) (y,_) = (x, y, ())

intraEdges :: CFGNodes -> CFGNode -> CFGEdges
intraEdges xs y = S.toList $ S.map ((flip intraEdge) y) xs

isLabelOfThisLoop :: Maybe Ident -> CFGNode -> Bool
isLabelOfThisLoop _        (_, NodeStmt (Break' Nothing))      = True
isLabelOfThisLoop _        (_, NodeStmt (Continue' Nothing))   = True
isLabelOfThisLoop Nothing  (_, NodeStmt (Break' (Just _)))     = False
isLabelOfThisLoop Nothing  (_, NodeStmt (Continue' (Just _)))  = False
isLabelOfThisLoop (Just l) (_, NodeStmt (Break' (Just l')))    = l == l'
isLabelOfThisLoop (Just l) (_, NodeStmt (Continue' (Just l'))) = l == l'
isLabelOfThisLoop _        _                                   = error "Not a break or continue statement."

finalOfBlock :: Block' -> CFGNodes
finalOfBlock block
    = final_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block' = 0
                             , cfg_Inh_Block'   = empty }

initOfBlock :: Block' -> CFGNode
initOfBlock block
    = init_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block' = 0
                             , cfg_Inh_Block'   = empty }

cfgOfBlock :: Block' -> CFG
cfgOfBlock block
    = cfg_Syn_Block' $ wrap_Block' (sem_Block' block) initial
    where
        initial = Inh_Block' { label_Inh_Block' = 0
                             , cfg_Inh_Block'   = empty }
{-# LINE 93 "Complete.hs" #-}
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
type T_Block' = CFG ->
                Node ->
                ( CFGNodes,CFG,CFGNodes,CFGNodes,CFGNode,Node,Block')
data Inh_Block' = Inh_Block' {cfg_Inh_Block' :: CFG,label_Inh_Block' :: Node}
data Syn_Block' = Syn_Block' {breaks_Syn_Block' :: CFGNodes,cfg_Syn_Block' :: CFG,continues_Syn_Block' :: CFGNodes,final_Syn_Block' :: CFGNodes,init_Syn_Block' :: CFGNode,label_Syn_Block' :: Node,self_Syn_Block' :: Block'}
wrap_Block' :: (T_Block') ->
               (Inh_Block') ->
               (Syn_Block')
wrap_Block' sem (Inh_Block' _lhsIcfg _lhsIlabel) =
    (let ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_Block' _lhsObreaks _lhsOcfg _lhsOcontinues _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_Block'_Block' :: (T_BlockStmts') ->
                     (T_Block')
sem_Block'_Block' blocks_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Block'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _blocksOcfg :: CFG
              _blocksOlabel :: Node
              _blocksIbreaks :: CFGNodes
              _blocksIcfg :: CFG
              _blocksIcontinues :: CFGNodes
              _blocksIfinal :: CFGNodes
              _blocksIinit :: CFGNode
              _blocksIlabel :: Node
              _blocksIself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 34 "CFA.ag" #-}
                   _blocksIlabel
                   {-# LINE 168 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _blocksIbreaks
                   {-# LINE 173 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _blocksIcontinues
                   {-# LINE 178 "Complete.hs" #-}
                   )
              _self =
                  Block' _blocksIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _blocksIcfg
                   {-# LINE 187 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 27 "CFA.ag" #-}
                   _blocksIfinal
                   {-# LINE 192 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 26 "CFA.ag" #-}
                   _blocksIinit
                   {-# LINE 197 "Complete.hs" #-}
                   )
              _blocksOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 202 "Complete.hs" #-}
                   )
              _blocksOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 207 "Complete.hs" #-}
                   )
              ( _blocksIbreaks,_blocksIcfg,_blocksIcontinues,_blocksIfinal,_blocksIinit,_blocksIlabel,_blocksIself) =
                  blocks_ _blocksOcfg _blocksOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
type T_BlockStmt' = CFG ->
                    Node ->
                    ( CFGNodes,CFG,CFGNodes,CFGNodes,CFGNode,Node,BlockStmt')
data Inh_BlockStmt' = Inh_BlockStmt' {cfg_Inh_BlockStmt' :: CFG,label_Inh_BlockStmt' :: Node}
data Syn_BlockStmt' = Syn_BlockStmt' {breaks_Syn_BlockStmt' :: CFGNodes,cfg_Syn_BlockStmt' :: CFG,continues_Syn_BlockStmt' :: CFGNodes,final_Syn_BlockStmt' :: CFGNodes,init_Syn_BlockStmt' :: CFGNode,label_Syn_BlockStmt' :: Node,self_Syn_BlockStmt' :: BlockStmt'}
wrap_BlockStmt' :: (T_BlockStmt') ->
                   (Inh_BlockStmt') ->
                   (Syn_BlockStmt')
wrap_BlockStmt' sem (Inh_BlockStmt' _lhsIcfg _lhsIlabel) =
    (let ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_BlockStmt' _lhsObreaks _lhsOcfg _lhsOcontinues _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_BlockStmt'_BlockStmt' :: (T_Stmt') ->
                             (T_BlockStmt')
sem_BlockStmt'_BlockStmt' stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: BlockStmt'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _statOcfg :: CFG
              _statOlabel :: Node
              _statIbreaks :: CFGNodes
              _statIcfg :: CFG
              _statIcontinues :: CFGNodes
              _statIfinal :: CFGNodes
              _statIinit :: CFGNode
              _statIlabel :: Node
              _statIself :: Stmt'
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _statIbreaks
                   {-# LINE 259 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _statIcontinues
                   {-# LINE 264 "Complete.hs" #-}
                   )
              _self =
                  BlockStmt' _statIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _statIcfg
                   {-# LINE 273 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 27 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 278 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 26 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 283 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 288 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 293 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 298 "Complete.hs" #-}
                   )
              ( _statIbreaks,_statIcfg,_statIcontinues,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_BlockStmt'_LocalVars' :: ([Modifier]) ->
                             Type ->
                             (T_VarDecls') ->
                             (T_BlockStmt')
sem_BlockStmt'_LocalVars' modifiers_ ty_ vars_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: BlockStmt'
              _varsIself :: VarDecls'
              _lhsOlabel =
                  ({-# LINE 54 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 321 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 55 "CFA.ag" #-}
                   nodeVarDecl (new _lhsIlabel) _self
                   {-# LINE 326 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 56 "CFA.ag" #-}
                   S.singleton $ nodeVarDecl (new _lhsIlabel) _self
                   {-# LINE 331 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 57 "CFA.ag" #-}
                   insNode (nodeVarDecl (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 336 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 341 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 346 "Complete.hs" #-}
                   )
              _self =
                  LocalVars' modifiers_ ty_ _varsIself
              _lhsOself =
                  _self
              ( _varsIself) =
                  vars_
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
-- BlockStmts' -------------------------------------------------
data BlockStmts' = Seq' (BlockStmt') (BlockStmts')
                 | Single' (BlockStmt')
                 deriving ( Eq,Show)
-- cata
sem_BlockStmts' :: (BlockStmts') ->
                   (T_BlockStmts')
sem_BlockStmts' (Seq' _stat1 _stat2) =
    (sem_BlockStmts'_Seq' (sem_BlockStmt' _stat1) (sem_BlockStmts' _stat2))
sem_BlockStmts' (Single' _s) =
    (sem_BlockStmts'_Single' (sem_BlockStmt' _s))
-- semantic domain
type T_BlockStmts' = CFG ->
                     Node ->
                     ( CFGNodes,CFG,CFGNodes,CFGNodes,CFGNode,Node,BlockStmts')
data Inh_BlockStmts' = Inh_BlockStmts' {cfg_Inh_BlockStmts' :: CFG,label_Inh_BlockStmts' :: Node}
data Syn_BlockStmts' = Syn_BlockStmts' {breaks_Syn_BlockStmts' :: CFGNodes,cfg_Syn_BlockStmts' :: CFG,continues_Syn_BlockStmts' :: CFGNodes,final_Syn_BlockStmts' :: CFGNodes,init_Syn_BlockStmts' :: CFGNode,label_Syn_BlockStmts' :: Node,self_Syn_BlockStmts' :: BlockStmts'}
wrap_BlockStmts' :: (T_BlockStmts') ->
                    (Inh_BlockStmts') ->
                    (Syn_BlockStmts')
wrap_BlockStmts' sem (Inh_BlockStmts' _lhsIcfg _lhsIlabel) =
    (let ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_BlockStmts' _lhsObreaks _lhsOcfg _lhsOcontinues _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_BlockStmts'_Seq' :: (T_BlockStmt') ->
                        (T_BlockStmts') ->
                        (T_BlockStmts')
sem_BlockStmts'_Seq' stat1_ stat2_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _stat1Olabel :: Node
              _stat1Ocfg :: CFG
              _stat2Olabel :: Node
              _stat2Ocfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: BlockStmts'
              _stat1Ibreaks :: CFGNodes
              _stat1Icfg :: CFG
              _stat1Icontinues :: CFGNodes
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: BlockStmt'
              _stat2Ibreaks :: CFGNodes
              _stat2Icfg :: CFG
              _stat2Icontinues :: CFGNodes
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 37 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 412 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 38 "CFA.ag" #-}
                   _stat1Iinit
                   {-# LINE 417 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 39 "CFA.ag" #-}
                   _stat2Ifinal
                   {-# LINE 422 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 40 "CFA.ag" #-}
                   _stat2Icfg
                   {-# LINE 427 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 41 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 432 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 42 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 437 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 43 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 442 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 44 "CFA.ag" #-}
                   case _stat1Iself of
                     BlockStmt' (Break' _)
                         -> _stat1Icfg
                     BlockStmt' (Continue' _)
                         -> _stat1Icfg
                     _   -> insEdges (intraEdges _stat1Ifinal _stat2Iinit) _stat1Icfg
                   {-# LINE 452 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _stat1Ibreaks <> _stat2Ibreaks
                   {-# LINE 457 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _stat1Icontinues <> _stat2Icontinues
                   {-# LINE 462 "Complete.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ibreaks,_stat1Icfg,_stat1Icontinues,_stat1Ifinal,_stat1Iinit,_stat1Ilabel,_stat1Iself) =
                  stat1_ _stat1Ocfg _stat1Olabel
              ( _stat2Ibreaks,_stat2Icfg,_stat2Icontinues,_stat2Ifinal,_stat2Iinit,_stat2Ilabel,_stat2Iself) =
                  stat2_ _stat2Ocfg _stat2Olabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_BlockStmts'_Single' :: (T_BlockStmt') ->
                           (T_BlockStmts')
sem_BlockStmts'_Single' s_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: BlockStmts'
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _sOcfg :: CFG
              _sOlabel :: Node
              _sIbreaks :: CFGNodes
              _sIcfg :: CFG
              _sIcontinues :: CFGNodes
              _sIfinal :: CFGNodes
              _sIinit :: CFGNode
              _sIlabel :: Node
              _sIself :: BlockStmt'
              _lhsOcfg =
                  ({-# LINE 51 "CFA.ag" #-}
                   _sIcfg
                   {-# LINE 497 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _sIbreaks
                   {-# LINE 502 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _sIcontinues
                   {-# LINE 507 "Complete.hs" #-}
                   )
              _self =
                  Single' _sIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 27 "CFA.ag" #-}
                   _sIfinal
                   {-# LINE 516 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 26 "CFA.ag" #-}
                   _sIinit
                   {-# LINE 521 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _sIlabel
                   {-# LINE 526 "Complete.hs" #-}
                   )
              _sOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 531 "Complete.hs" #-}
                   )
              _sOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 536 "Complete.hs" #-}
                   )
              ( _sIbreaks,_sIcfg,_sIcontinues,_sIfinal,_sIinit,_sIlabel,_sIself) =
                  s_ _sOcfg _sOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
-- Stmt' -------------------------------------------------------
data Stmt' = StmtBlock' (Block')
           | IfThen' (Exp) (Stmt')
           | IfThenElse' (Exp) (Stmt') (Stmt')
           | While' ((Maybe Ident)) (Exp) (Stmt')
           | BasicFor' ((Maybe Ident)) ((Maybe ForInit)) ((Maybe Exp)) ((Maybe [Exp])) (Stmt')
           | Empty'
           | ExpStmt' (Exp)
           | Assert' (Exp) ((Maybe Exp))
           | Assume' (Exp)
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
sem_Stmt' (While' _ident _exp _body) =
    (sem_Stmt'_While' _ident _exp (sem_Stmt' _body))
sem_Stmt' (BasicFor' _ident _init _guard _update _body) =
    (sem_Stmt'_BasicFor' _ident _init _guard _update (sem_Stmt' _body))
sem_Stmt' (Empty') =
    (sem_Stmt'_Empty')
sem_Stmt' (ExpStmt' _exp) =
    (sem_Stmt'_ExpStmt' _exp)
sem_Stmt' (Assert' _exp _error) =
    (sem_Stmt'_Assert' _exp _error)
sem_Stmt' (Assume' _exp) =
    (sem_Stmt'_Assume' _exp)
sem_Stmt' (Break' _ident) =
    (sem_Stmt'_Break' _ident)
sem_Stmt' (Continue' _ident) =
    (sem_Stmt'_Continue' _ident)
sem_Stmt' (Return' _exp) =
    (sem_Stmt'_Return' _exp)
-- semantic domain
type T_Stmt' = CFG ->
               Node ->
               ( CFGNodes,CFG,CFGNodes,CFGNodes,CFGNode,Node,Stmt')
data Inh_Stmt' = Inh_Stmt' {cfg_Inh_Stmt' :: CFG,label_Inh_Stmt' :: Node}
data Syn_Stmt' = Syn_Stmt' {breaks_Syn_Stmt' :: CFGNodes,cfg_Syn_Stmt' :: CFG,continues_Syn_Stmt' :: CFGNodes,final_Syn_Stmt' :: CFGNodes,init_Syn_Stmt' :: CFGNode,label_Syn_Stmt' :: Node,self_Syn_Stmt' :: Stmt'}
wrap_Stmt' :: (T_Stmt') ->
              (Inh_Stmt') ->
              (Syn_Stmt')
wrap_Stmt' sem (Inh_Stmt' _lhsIcfg _lhsIlabel) =
    (let ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_Stmt' _lhsObreaks _lhsOcfg _lhsOcontinues _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_Stmt'_StmtBlock' :: (T_Block') ->
                        (T_Stmt')
sem_Stmt'_StmtBlock' block_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _blockOcfg :: CFG
              _blockOlabel :: Node
              _blockIbreaks :: CFGNodes
              _blockIcfg :: CFG
              _blockIcontinues :: CFGNodes
              _blockIfinal :: CFGNodes
              _blockIinit :: CFGNode
              _blockIlabel :: Node
              _blockIself :: Block'
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _blockIbreaks
                   {-# LINE 618 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _blockIcontinues
                   {-# LINE 623 "Complete.hs" #-}
                   )
              _self =
                  StmtBlock' _blockIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _blockIcfg
                   {-# LINE 632 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 27 "CFA.ag" #-}
                   _blockIfinal
                   {-# LINE 637 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 26 "CFA.ag" #-}
                   _blockIinit
                   {-# LINE 642 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _blockIlabel
                   {-# LINE 647 "Complete.hs" #-}
                   )
              _blockOcfg =
                  ({-# LINE 30 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 652 "Complete.hs" #-}
                   )
              _blockOlabel =
                  ({-# LINE 31 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 657 "Complete.hs" #-}
                   )
              ( _blockIbreaks,_blockIcfg,_blockIcontinues,_blockIfinal,_blockIinit,_blockIlabel,_blockIself) =
                  block_ _blockOcfg _blockOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_IfThen' :: Exp ->
                     (T_Stmt') ->
                     (T_Stmt')
sem_Stmt'_IfThen' exp_ stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _statOlabel :: Node
              _statOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _statIbreaks :: CFGNodes
              _statIcfg :: CFG
              _statIcontinues :: CFGNodes
              _statIfinal :: CFGNodes
              _statIinit :: CFGNode
              _statIlabel :: Node
              _statIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 60 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 687 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 61 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 692 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 62 "CFA.ag" #-}
                   S.insert (nodeStmt (new _lhsIlabel) _self) _statIfinal
                   {-# LINE 697 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 63 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                      insEdge (intraEdge self _statIinit) $
                      insNode self _statIcfg
                   {-# LINE 704 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 68 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 709 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 69 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 714 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _statIbreaks
                   {-# LINE 719 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _statIcontinues
                   {-# LINE 724 "Complete.hs" #-}
                   )
              _self =
                  IfThen' exp_ _statIself
              _lhsOself =
                  _self
              ( _statIbreaks,_statIcfg,_statIcontinues,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_IfThenElse' :: Exp ->
                         (T_Stmt') ->
                         (T_Stmt') ->
                         (T_Stmt')
sem_Stmt'_IfThenElse' exp_ stat1_ stat2_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _stat1Olabel :: Node
              _stat1Ocfg :: CFG
              _stat2Olabel :: Node
              _stat2Ocfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _stat1Ibreaks :: CFGNodes
              _stat1Icfg :: CFG
              _stat1Icontinues :: CFGNodes
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: Stmt'
              _stat2Ibreaks :: CFGNodes
              _stat2Icfg :: CFG
              _stat2Icontinues :: CFGNodes
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 71 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 768 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 72 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 773 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 73 "CFA.ag" #-}
                   _stat1Ifinal `S.union` _stat2Ifinal
                   {-# LINE 778 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 74 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                     insEdge (intraEdge self _stat1Iinit) $
                     insEdge (intraEdge self _stat2Iinit) $
                     insNode self _stat2Icfg
                   {-# LINE 786 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 81 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 791 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 82 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 796 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 83 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 801 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 84 "CFA.ag" #-}
                   _stat1Icfg
                   {-# LINE 806 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   _stat1Ibreaks <> _stat2Ibreaks
                   {-# LINE 811 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   _stat1Icontinues <> _stat2Icontinues
                   {-# LINE 816 "Complete.hs" #-}
                   )
              _self =
                  IfThenElse' exp_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ibreaks,_stat1Icfg,_stat1Icontinues,_stat1Ifinal,_stat1Iinit,_stat1Ilabel,_stat1Iself) =
                  stat1_ _stat1Ocfg _stat1Olabel
              ( _stat2Ibreaks,_stat2Icfg,_stat2Icontinues,_stat2Ifinal,_stat2Iinit,_stat2Ilabel,_stat2Iself) =
                  stat2_ _stat2Ocfg _stat2Olabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_While' :: (Maybe Ident) ->
                    Exp ->
                    (T_Stmt') ->
                    (T_Stmt')
sem_Stmt'_While' ident_ exp_ body_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _bodyOlabel :: Node
              _bodyOcfg :: CFG
              _lhsOself :: Stmt'
              _bodyIbreaks :: CFGNodes
              _bodyIcfg :: CFG
              _bodyIcontinues :: CFGNodes
              _bodyIfinal :: CFGNodes
              _bodyIinit :: CFGNode
              _bodyIlabel :: Node
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 86 "CFA.ag" #-}
                   _bodyIlabel
                   {-# LINE 853 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 87 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 858 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "CFA.ag" #-}
                   S.insert (nodeStmt (new _lhsIlabel) _self) (S.filter (isLabelOfThisLoop ident_) _bodyIbreaks)
                   {-# LINE 863 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 89 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                      insEdge (intraEdge self _bodyIinit)                                              $
                      insEdges (intraEdges _bodyIfinal self)                                           $
                      insEdges (intraEdges (S.filter (isLabelOfThisLoop ident_) _bodyIcontinues) self) $
                      insNode self _bodyIcfg
                   {-# LINE 872 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 98 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIbreaks
                   {-# LINE 877 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 99 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIcontinues
                   {-# LINE 882 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 100 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 887 "Complete.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 101 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 892 "Complete.hs" #-}
                   )
              _self =
                  While' ident_ exp_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIbreaks,_bodyIcfg,_bodyIcontinues,_bodyIfinal,_bodyIinit,_bodyIlabel,_bodyIself) =
                  body_ _bodyOcfg _bodyOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_BasicFor' :: (Maybe Ident) ->
                       (Maybe ForInit) ->
                       (Maybe Exp) ->
                       (Maybe [Exp]) ->
                       (T_Stmt') ->
                       (T_Stmt')
sem_Stmt'_BasicFor' ident_ init_ guard_ update_ body_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _bodyOlabel :: Node
              _bodyOcfg :: CFG
              _lhsOself :: Stmt'
              _bodyIbreaks :: CFGNodes
              _bodyIcfg :: CFG
              _bodyIcontinues :: CFGNodes
              _bodyIfinal :: CFGNodes
              _bodyIinit :: CFGNode
              _bodyIlabel :: Node
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 103 "CFA.ag" #-}
                   _bodyIlabel
                   {-# LINE 929 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 104 "CFA.ag" #-}
                   maybe (nodeStmt (new _lhsIlabel) _self) (nodeForInit . new $ new _lhsIlabel) init_
                   {-# LINE 934 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 105 "CFA.ag" #-}
                   S.insert (nodeStmt (new _lhsIlabel) _self) (S.filter (isLabelOfThisLoop ident_) _bodyIbreaks)
                   {-# LINE 939 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 106 "CFA.ag" #-}
                   let self   = nodeStmt      (new _lhsIlabel) _self
                       init   = nodeForInit   (new $ new _lhsIlabel)                   <$> init_
                       update = nodeForUpdate (new $ new $ newIfJust init_ _lhsIlabel) <$> update_ in
                         insEdge (intraEdge self _bodyIinit)                                                                $
                         insEdges (intraEdges _bodyIfinal (maybe self id update))                                           $
                         maybe id (\ init' -> insEdge $ intraEdge init' self) init                                          $
                         maybe id (\ update' -> insEdge $ intraEdge update' self) update                                    $
                         insEdges (intraEdges (S.filter (isLabelOfThisLoop ident_) _bodyIcontinues) (maybe self id update)) $
                         maybe id insNode init                                                                              $
                         maybe id insNode update                                                                            $
                         insNode self _bodyIcfg
                   {-# LINE 954 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 125 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIbreaks
                   {-# LINE 959 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 126 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIcontinues
                   {-# LINE 964 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 127 "CFA.ag" #-}
                   new $ newIfJust init_ $ newIfJust update_ _lhsIlabel
                   {-# LINE 969 "Complete.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 128 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 974 "Complete.hs" #-}
                   )
              _self =
                  BasicFor' ident_ init_ guard_ update_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIbreaks,_bodyIcfg,_bodyIcontinues,_bodyIfinal,_bodyIinit,_bodyIlabel,_bodyIself) =
                  body_ _bodyOcfg _bodyOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Empty' :: (T_Stmt')
sem_Stmt'_Empty' =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 130 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 997 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 131 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1002 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 132 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1007 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 133 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1012 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1017 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1022 "Complete.hs" #-}
                   )
              _self =
                  Empty'
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_ExpStmt' :: Exp ->
                      (T_Stmt')
sem_Stmt'_ExpStmt' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 135 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1044 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 136 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1049 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 137 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1054 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 138 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1059 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1064 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1069 "Complete.hs" #-}
                   )
              _self =
                  ExpStmt' exp_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Assert' :: Exp ->
                     (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Assert' exp_ error_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 140 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1092 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 141 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1097 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 142 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1102 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 143 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1107 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1112 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1117 "Complete.hs" #-}
                   )
              _self =
                  Assert' exp_ error_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Assume' :: Exp ->
                     (T_Stmt')
sem_Stmt'_Assume' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 145 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1139 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 146 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1144 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 147 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1149 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 148 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1154 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1159 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1164 "Complete.hs" #-}
                   )
              _self =
                  Assume' exp_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Break' :: (Maybe Ident) ->
                    (T_Stmt')
sem_Stmt'_Break' ident_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 150 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1186 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 151 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1191 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 152 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1196 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 153 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1201 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 154 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1206 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1211 "Complete.hs" #-}
                   )
              _self =
                  Break' ident_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Continue' :: (Maybe Ident) ->
                       (T_Stmt')
sem_Stmt'_Continue' ident_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsOcontinues :: CFGNodes
              _lhsObreaks :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 156 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1233 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 157 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1238 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 158 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1243 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 159 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1248 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 160 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1253 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1258 "Complete.hs" #-}
                   )
              _self =
                  Continue' ident_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Return' :: (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Return' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 162 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1280 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 163 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1285 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 164 "CFA.ag" #-}
                   S.singleton $ nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1290 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 165 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1295 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 28 "CFA.ag" #-}
                   mempty
                   {-# LINE 1300 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 29 "CFA.ag" #-}
                   mempty
                   {-# LINE 1305 "Complete.hs" #-}
                   )
              _self =
                  Return' exp_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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