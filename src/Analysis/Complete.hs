

-- UUAGC 0.9.52.1 (Complete.ag)
module Analysis.Complete where

import Language.Java.Syntax
import Phase

import           Data.Maybe
import qualified Data.Map   as M
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

type CFGNodes = [CFGNode]

type CFGEdge = UEdge

type CFGEdges = [CFGEdge]

type CFGContext = (Adj (), CFGNode, CFGNodeValue, Adj ())

type CFG = Gr CFGNodeValue ()
{-# LINE 35 "Complete.hs" #-}

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
intraEdges xs y = map ((flip intraEdge) y) xs

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
{-# LINE 83 "Complete.hs" #-}

{-# LINE 123 "Syntax.ag" #-}

transformBlock :: Block -> PhaseResult Block'
transformBlock (Block ss) = Block' <$> transformBlockStmts ss

transformBlockStmts :: [BlockStmt] -> PhaseResult  BlockStmts'
transformBlockStmts []     = return $ Single' $ BlockStmt' $ Empty'
transformBlockStmts (s:[]) = Single' <$> transformBlockStmt s
transformBlockStmts (s:ss) = Seq' <$> transformBlockStmt s <*> transformBlockStmts ss

transformBlockStmt :: BlockStmt -> PhaseResult BlockStmt'
transformBlockStmt (BlockStmt s)        = BlockStmt' <$> transformStmt s
transformBlockStmt (LocalVars ms ty vs) = LocalVars' ms ty <$> (mapM transformVarDecl vs)

transformVarDecl :: VarDecl -> PhaseResult VarDecl'
transformVarDecl (VarDecl name Nothing)     = return $ VarDecl' name Nothing
transformVarDecl (VarDecl name (Just init)) = do
    init' <- transformVarInit init
    return $ VarDecl' name (Just init')

transformVarInit :: VarInit -> PhaseResult VarInit'
transformVarInit (InitExp e)      = return $ InitExp' e
transformVarInit (InitArray init) = InitArray' <$> transformArrayInit init

transformArrayInit :: ArrayInit -> PhaseResult ArrayInit'
transformArrayInit (ArrayInit inits) = ArrayInit' <$> mapM transformVarInit inits

transformStmt :: Stmt -> PhaseResult Stmt'
transformStmt (StmtBlock b)        = StmtBlock' <$> transformBlock b
transformStmt (IfThen e s)         = IfThen' e <$> transformStmt s
transformStmt (IfThenElse e s1 s2) = IfThenElse' e <$> transformStmt s1 <*> transformStmt s2 
transformStmt (While e b)          = While' e <$> transformStmt b
transformStmt (BasicFor i g u b)   = BasicFor' i g u <$> transformStmt b
transformStmt Empty                = return Empty'
transformStmt (ExpStmt expr)       = return $ ExpStmt' expr
transformStmt (Assert expr err)    = return $ Assert' expr err
--transformStmt (Break Nothing)      = return Break'
--transformStmt (Continue Nothing)   = return Continue' 
transformStmt (Return e)           = return $ Return' e 
transformStmt (Labeled l s)        = Labeled' l <$> transformStmt s 
transformStmt s                    = Left $ SyntaxTransformation $  "The statment " ++ show s ++ " is not supported"
{-# LINE 126 "Complete.hs" #-}
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
                ( CFG,CFGNodes,CFGNode,Node,Block')
data Inh_Block' = Inh_Block' {cfg_Inh_Block' :: CFG,label_Inh_Block' :: Node}
data Syn_Block' = Syn_Block' {cfg_Syn_Block' :: CFG,final_Syn_Block' :: CFGNodes,init_Syn_Block' :: CFGNode,label_Syn_Block' :: Node,self_Syn_Block' :: Block'}
wrap_Block' :: (T_Block') ->
               (Inh_Block') ->
               (Syn_Block')
wrap_Block' sem (Inh_Block' _lhsIcfg _lhsIlabel) =
    (let ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_Block' _lhsOcfg _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_Block'_Block' :: (T_BlockStmts') ->
                     (T_Block')
sem_Block'_Block' blocks_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOself :: Block'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _blocksOcfg :: CFG
              _blocksOlabel :: Node
              _blocksIcfg :: CFG
              _blocksIfinal :: CFGNodes
              _blocksIinit :: CFGNode
              _blocksIlabel :: Node
              _blocksIself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 29 "CFA.ag" #-}
                   _blocksIlabel
                   {-# LINE 197 "Complete.hs" #-}
                   )
              _self =
                  Block' _blocksIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _blocksIcfg
                   {-# LINE 206 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 24 "CFA.ag" #-}
                   _blocksIfinal
                   {-# LINE 211 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 23 "CFA.ag" #-}
                   _blocksIinit
                   {-# LINE 216 "Complete.hs" #-}
                   )
              _blocksOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 221 "Complete.hs" #-}
                   )
              _blocksOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 226 "Complete.hs" #-}
                   )
              ( _blocksIcfg,_blocksIfinal,_blocksIinit,_blocksIlabel,_blocksIself) =
                  blocks_ _blocksOcfg _blocksOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
                    ( CFG,CFGNodes,CFGNode,Node,BlockStmt')
data Inh_BlockStmt' = Inh_BlockStmt' {cfg_Inh_BlockStmt' :: CFG,label_Inh_BlockStmt' :: Node}
data Syn_BlockStmt' = Syn_BlockStmt' {cfg_Syn_BlockStmt' :: CFG,final_Syn_BlockStmt' :: CFGNodes,init_Syn_BlockStmt' :: CFGNode,label_Syn_BlockStmt' :: Node,self_Syn_BlockStmt' :: BlockStmt'}
wrap_BlockStmt' :: (T_BlockStmt') ->
                   (Inh_BlockStmt') ->
                   (Syn_BlockStmt')
wrap_BlockStmt' sem (Inh_BlockStmt' _lhsIcfg _lhsIlabel) =
    (let ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_BlockStmt' _lhsOcfg _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_BlockStmt'_BlockStmt' :: (T_Stmt') ->
                             (T_BlockStmt')
sem_BlockStmt'_BlockStmt' stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOself :: BlockStmt'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _statOcfg :: CFG
              _statOlabel :: Node
              _statIcfg :: CFG
              _statIfinal :: CFGNodes
              _statIinit :: CFGNode
              _statIlabel :: Node
              _statIself :: Stmt'
              _self =
                  BlockStmt' _statIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _statIcfg
                   {-# LINE 278 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 24 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 283 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 23 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 288 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 293 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 298 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 303 "Complete.hs" #-}
                   )
              ( _statIcfg,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
              _lhsOself :: BlockStmt'
              _varsIself :: VarDecls'
              _lhsOlabel =
                  ({-# LINE 44 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 324 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 46 "CFA.ag" #-}
                   nodeVarDecl (new _lhsIlabel) _self
                   {-# LINE 329 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 47 "CFA.ag" #-}
                   [nodeVarDecl (new _lhsIlabel) _self]
                   {-# LINE 334 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 48 "CFA.ag" #-}
                   insNode (nodeVarDecl (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 339 "Complete.hs" #-}
                   )
              _self =
                  LocalVars' modifiers_ ty_ _varsIself
              _lhsOself =
                  _self
              ( _varsIself) =
                  vars_
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
                     ( CFG,CFGNodes,CFGNode,Node,BlockStmts')
data Inh_BlockStmts' = Inh_BlockStmts' {cfg_Inh_BlockStmts' :: CFG,label_Inh_BlockStmts' :: Node}
data Syn_BlockStmts' = Syn_BlockStmts' {cfg_Syn_BlockStmts' :: CFG,final_Syn_BlockStmts' :: CFGNodes,init_Syn_BlockStmts' :: CFGNode,label_Syn_BlockStmts' :: Node,self_Syn_BlockStmts' :: BlockStmts'}
wrap_BlockStmts' :: (T_BlockStmts') ->
                    (Inh_BlockStmts') ->
                    (Syn_BlockStmts')
wrap_BlockStmts' sem (Inh_BlockStmts' _lhsIcfg _lhsIlabel) =
    (let ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_BlockStmts' _lhsOcfg _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
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
              _lhsOself :: BlockStmts'
              _stat1Icfg :: CFG
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: BlockStmt'
              _stat2Icfg :: CFG
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: BlockStmts'
              _lhsOlabel =
                  ({-# LINE 32 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 399 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 33 "CFA.ag" #-}
                   _stat1Iinit
                   {-# LINE 404 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 34 "CFA.ag" #-}
                   _stat2Ifinal
                   {-# LINE 409 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 35 "CFA.ag" #-}
                   _stat2Icfg
                   {-# LINE 414 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 36 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 419 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 37 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 424 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 38 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 429 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 39 "CFA.ag" #-}
                   insEdges (intraEdges _stat1Ifinal _stat2Iinit) _stat1Icfg
                   {-# LINE 434 "Complete.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Icfg,_stat1Ifinal,_stat1Iinit,_stat1Ilabel,_stat1Iself) =
                  stat1_ _stat1Ocfg _stat1Olabel
              ( _stat2Icfg,_stat2Ifinal,_stat2Iinit,_stat2Ilabel,_stat2Iself) =
                  stat2_ _stat2Ocfg _stat2Olabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_BlockStmts'_Single' :: (T_BlockStmt') ->
                           (T_BlockStmts')
sem_BlockStmts'_Single' s_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOcfg :: CFG
              _lhsOself :: BlockStmts'
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _sOcfg :: CFG
              _sOlabel :: Node
              _sIcfg :: CFG
              _sIfinal :: CFGNodes
              _sIinit :: CFGNode
              _sIlabel :: Node
              _sIself :: BlockStmt'
              _lhsOcfg =
                  ({-# LINE 41 "CFA.ag" #-}
                   _sIcfg
                   {-# LINE 465 "Complete.hs" #-}
                   )
              _self =
                  Single' _sIself
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 24 "CFA.ag" #-}
                   _sIfinal
                   {-# LINE 474 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 23 "CFA.ag" #-}
                   _sIinit
                   {-# LINE 479 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _sIlabel
                   {-# LINE 484 "Complete.hs" #-}
                   )
              _sOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 489 "Complete.hs" #-}
                   )
              _sOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 494 "Complete.hs" #-}
                   )
              ( _sIcfg,_sIfinal,_sIinit,_sIlabel,_sIself) =
                  s_ _sOcfg _sOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
-- Stmt' -------------------------------------------------------
data Stmt' = StmtBlock' (Block')
           | IfThen' (Exp) (Stmt')
           | IfThenElse' (Exp) (Stmt') (Stmt')
           | While' (Exp) (Stmt')
           | BasicFor' ((Maybe ForInit)) ((Maybe Exp)) ((Maybe [Exp])) (Stmt')
           | Empty'
           | ExpStmt' (Exp)
           | Assert' (Exp) ((Maybe Exp))
           | Assume' (Exp)
           | Break'
           | Continue'
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
sem_Stmt' (Assume' _exp) =
    (sem_Stmt'_Assume' _exp)
sem_Stmt' (Break') =
    (sem_Stmt'_Break')
sem_Stmt' (Continue') =
    (sem_Stmt'_Continue')
sem_Stmt' (Return' _exp) =
    (sem_Stmt'_Return' _exp)
sem_Stmt' (Labeled' _ident _stat) =
    (sem_Stmt'_Labeled' _ident (sem_Stmt' _stat))
-- semantic domain
type T_Stmt' = CFG ->
               Node ->
               ( CFG,CFGNodes,CFGNode,Node,Stmt')
data Inh_Stmt' = Inh_Stmt' {cfg_Inh_Stmt' :: CFG,label_Inh_Stmt' :: Node}
data Syn_Stmt' = Syn_Stmt' {cfg_Syn_Stmt' :: CFG,final_Syn_Stmt' :: CFGNodes,init_Syn_Stmt' :: CFGNode,label_Syn_Stmt' :: Node,self_Syn_Stmt' :: Stmt'}
wrap_Stmt' :: (T_Stmt') ->
              (Inh_Stmt') ->
              (Syn_Stmt')
wrap_Stmt' sem (Inh_Stmt' _lhsIcfg _lhsIlabel) =
    (let ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_Stmt' _lhsOcfg _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_Stmt'_StmtBlock' :: (T_Block') ->
                        (T_Stmt')
sem_Stmt'_StmtBlock' block_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOself :: Stmt'
              _lhsOcfg :: CFG
              _lhsOfinal :: CFGNodes
              _lhsOinit :: CFGNode
              _lhsOlabel :: Node
              _blockOcfg :: CFG
              _blockOlabel :: Node
              _blockIcfg :: CFG
              _blockIfinal :: CFGNodes
              _blockIinit :: CFGNode
              _blockIlabel :: Node
              _blockIself :: Block'
              _self =
                  StmtBlock' _blockIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _blockIcfg
                   {-# LINE 579 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 24 "CFA.ag" #-}
                   _blockIfinal
                   {-# LINE 584 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 23 "CFA.ag" #-}
                   _blockIinit
                   {-# LINE 589 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _blockIlabel
                   {-# LINE 594 "Complete.hs" #-}
                   )
              _blockOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 599 "Complete.hs" #-}
                   )
              _blockOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 604 "Complete.hs" #-}
                   )
              ( _blockIcfg,_blockIfinal,_blockIinit,_blockIlabel,_blockIself) =
                  block_ _blockOcfg _blockOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
              _lhsOself :: Stmt'
              _statIcfg :: CFG
              _statIfinal :: CFGNodes
              _statIinit :: CFGNode
              _statIlabel :: Node
              _statIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 51 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 630 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 52 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 635 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 53 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self : _statIfinal
                   {-# LINE 640 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 54 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                      insEdge (intraEdge self _statIinit) $
                      insNode self _statIcfg
                   {-# LINE 647 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 59 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 652 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 60 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 657 "Complete.hs" #-}
                   )
              _self =
                  IfThen' exp_ _statIself
              _lhsOself =
                  _self
              ( _statIcfg,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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
              _lhsOself :: Stmt'
              _stat1Icfg :: CFG
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: Stmt'
              _stat2Icfg :: CFG
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 62 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 695 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 63 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 700 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 64 "CFA.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 705 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 65 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                     insEdge (intraEdge self _stat1Iinit) $
                     insEdge (intraEdge self _stat2Iinit) $
                     insNode self _stat2Icfg
                   {-# LINE 713 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 72 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 718 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 73 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 723 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 74 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 728 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 75 "CFA.ag" #-}
                   _stat1Icfg
                   {-# LINE 733 "Complete.hs" #-}
                   )
              _self =
                  IfThenElse' exp_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Icfg,_stat1Ifinal,_stat1Iinit,_stat1Ilabel,_stat1Iself) =
                  stat1_ _stat1Ocfg _stat1Olabel
              ( _stat2Icfg,_stat2Ifinal,_stat2Iinit,_stat2Ilabel,_stat2Iself) =
                  stat2_ _stat2Ocfg _stat2Olabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_While' :: Exp ->
                    (T_Stmt') ->
                    (T_Stmt')
sem_Stmt'_While' exp_ body_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _bodyOlabel :: Node
              _bodyOcfg :: CFG
              _lhsOself :: Stmt'
              _bodyIcfg :: CFG
              _bodyIfinal :: CFGNodes
              _bodyIinit :: CFGNode
              _bodyIlabel :: Node
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 77 "CFA.ag" #-}
                   _bodyIlabel
                   {-# LINE 765 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 770 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 79 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 775 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 80 "CFA.ag" #-}
                   let self = nodeStmt (new _lhsIlabel) _self in
                      insEdge (intraEdge self _bodyIinit)    $
                      insEdges (intraEdges _bodyIfinal self) $
                      insNode self _bodyIcfg
                   {-# LINE 783 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 87 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 788 "Complete.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 88 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 793 "Complete.hs" #-}
                   )
              _self =
                  While' exp_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIcfg,_bodyIfinal,_bodyIinit,_bodyIlabel,_bodyIself) =
                  body_ _bodyOcfg _bodyOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_BasicFor' :: (Maybe ForInit) ->
                       (Maybe Exp) ->
                       (Maybe [Exp]) ->
                       (T_Stmt') ->
                       (T_Stmt')
sem_Stmt'_BasicFor' init_ guard_ update_ body_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOcfg :: CFG
              _bodyOlabel :: Node
              _bodyOcfg :: CFG
              _lhsOself :: Stmt'
              _bodyIcfg :: CFG
              _bodyIfinal :: CFGNodes
              _bodyIinit :: CFGNode
              _bodyIlabel :: Node
              _bodyIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 90 "CFA.ag" #-}
                   _bodyIlabel
                   {-# LINE 825 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 91 "CFA.ag" #-}
                   maybe (nodeStmt (new _lhsIlabel) _self) (nodeForInit . new $ new _lhsIlabel) init_
                   {-# LINE 830 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 92 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 835 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 93 "CFA.ag" #-}
                   let self   = nodeStmt      (new _lhsIlabel) _self
                       init   = nodeForInit   (new $ new _lhsIlabel)                   <$> init_
                       update = nodeForUpdate (new $ new $ newIfJust init_ _lhsIlabel) <$> update_ in
                      insEdge (intraEdge self _bodyIinit) $
                      insEdges (intraEdges _bodyIfinal (maybe self id update)) $
                      maybe id (\ init' -> insEdge $ intraEdge init' self) init $
                      maybe id (\ update' -> insEdge $ intraEdge update' self) update $
                      maybe id insNode init $
                      maybe id insNode update $
                      insNode self _bodyIcfg
                   {-# LINE 849 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 110 "CFA.ag" #-}
                   new $ newIfJust init_ $ newIfJust update_ _lhsIlabel
                   {-# LINE 854 "Complete.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 111 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 859 "Complete.hs" #-}
                   )
              _self =
                  BasicFor' init_ guard_ update_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIcfg,_bodyIfinal,_bodyIinit,_bodyIlabel,_bodyIself) =
                  body_ _bodyOcfg _bodyOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Empty' :: (T_Stmt')
sem_Stmt'_Empty' =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 127 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 880 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 128 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 885 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 129 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 890 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 130 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 895 "Complete.hs" #-}
                   )
              _self =
                  Empty'
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_ExpStmt' :: Exp ->
                      (T_Stmt')
sem_Stmt'_ExpStmt' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 132 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 915 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 133 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 920 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 134 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 925 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 135 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 930 "Complete.hs" #-}
                   )
              _self =
                  ExpStmt' exp_
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Assert' :: Exp ->
                     (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Assert' exp_ error_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 137 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 951 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 138 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 956 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 139 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 961 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 140 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 966 "Complete.hs" #-}
                   )
              _self =
                  Assert' exp_ error_
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Assume' :: Exp ->
                     (T_Stmt')
sem_Stmt'_Assume' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 142 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 986 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 143 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 991 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 144 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 996 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 145 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 1001 "Complete.hs" #-}
                   )
              _self =
                  Assume' exp_
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Break' :: (T_Stmt')
sem_Stmt'_Break' =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 147 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1020 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 148 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1025 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 149 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1030 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 150 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 1035 "Complete.hs" #-}
                   )
              _self =
                  Break'
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Continue' :: (T_Stmt')
sem_Stmt'_Continue' =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 152 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1054 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 153 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1059 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 154 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1064 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 155 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 1069 "Complete.hs" #-}
                   )
              _self =
                  Continue'
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Return' :: (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Return' exp_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 157 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1089 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 158 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1094 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 159 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1099 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 160 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 1104 "Complete.hs" #-}
                   )
              _self =
                  Return' exp_
              _lhsOself =
                  _self
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Labeled' :: Ident ->
                      (T_Stmt') ->
                      (T_Stmt')
sem_Stmt'_Labeled' ident_ stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsOlabel :: Node
              _lhsOcfg :: CFG
              _lhsOinit :: CFGNode
              _lhsOfinal :: CFGNodes
              _lhsOself :: Stmt'
              _statOcfg :: CFG
              _statOlabel :: Node
              _statIcfg :: CFG
              _statIfinal :: CFGNodes
              _statIinit :: CFGNode
              _statIlabel :: Node
              _statIself :: Stmt'
              _lhsOlabel =
                  ({-# LINE 162 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1132 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 163 "CFA.ag" #-}
                   insNode (nodeStmt (new _lhsIlabel) _self) _lhsIcfg
                   {-# LINE 1137 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 164 "CFA.ag" #-}
                   nodeStmt (new _lhsIlabel) _self
                   {-# LINE 1142 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 165 "CFA.ag" #-}
                   [nodeStmt (new _lhsIlabel) _self]
                   {-# LINE 1147 "Complete.hs" #-}
                   )
              _self =
                  Labeled' ident_ _statIself
              _lhsOself =
                  _self
              _statOcfg =
                  ({-# LINE 25 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 1156 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 26 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 1161 "Complete.hs" #-}
                   )
              ( _statIcfg,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsOcfg,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
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