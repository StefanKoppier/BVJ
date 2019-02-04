

-- UUAGC 0.9.52.1 (Complete.ag)
module Analysis.Complete where

import           Language.Java.Syntax
import           Data.Monoid
import qualified Data.Set                          as S
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

{-# LINE 1 "CFA.ag" #-}

type CFGNodeValue = CompoundStmt'
    
type CFGNode = LNode CFGNodeValue

instance {-# OVERLAPS #-} Ord CFGNode where
    (x,_) <= (y,_) = x <= y

type CFGNodes = S.Set CFGNode

data CFGEdgeValue
    = Edge
    | ConditionalEdge Bool
    deriving (Show, Eq, Ord)

type CFGEdge = LEdge CFGEdgeValue

type CFGEdges = [CFGEdge]

type CFGAdj = Adj CFGEdgeValue

type CFGContext = (CFGAdj, Node, CFGNodeValue, CFGAdj)

type CFG = Gr CFGNodeValue CFGEdgeValue
{-# LINE 38 "Complete.hs" #-}

{-# LINE 229 "CFA.ag" #-}

new :: Node -> Node
new = (1+)

newIfJust :: Maybe a -> Node -> Node
newIfJust = maybe id (const new)

node :: Node -> CompoundStmt' -> CFGNode
node l s = (l, s)

newEdge :: CFGNode -> CFGNode -> CFGEdge
newEdge (x,_) (y,_) = (x, y, Edge)

newEdges :: CFGNodes -> CFGNode -> CFGEdges
newEdges xs y = S.toList $ S.map ((flip newEdge) y) xs

newCondEdge :: CFGNode -> CFGNode -> Bool -> CFGEdge
newCondEdge (x,_) (y,_) b = (x, y, ConditionalEdge b)

newCondEdges :: CFGNodes -> CFGNode -> Bool -> CFGEdges
newCondEdges xs y b = S.toList $ S.map (\ x -> newCondEdge x y b) xs

isLabelOfThisLoop :: Maybe Ident -> CFGNode -> Bool
isLabelOfThisLoop _        (_, Stmt' (Break' Nothing))      = True
isLabelOfThisLoop _        (_, Stmt' (Continue' Nothing))   = True
isLabelOfThisLoop Nothing  (_, Stmt' (Break' (Just _)))     = False
isLabelOfThisLoop Nothing  (_, Stmt' (Continue' (Just _)))  = False
isLabelOfThisLoop (Just l) (_, Stmt' (Break' (Just l')))    = l == l'
isLabelOfThisLoop (Just l) (_, Stmt' (Continue' (Just l'))) = l == l'
isLabelOfThisLoop _        _                                   = error "Not a break or continue statement."

finalOfStmt :: CompoundStmt' -> CFGNodes
finalOfStmt stat
    = final_Syn_CompoundStmt' $ wrap_CompoundStmt' (sem_CompoundStmt' stat) initial
    where
        initial = Inh_CompoundStmt' { label_Inh_CompoundStmt' = 0
                                    , cfg_Inh_CompoundStmt'   = empty }

initOfStmt :: CompoundStmt' -> CFGNode
initOfStmt stat
    = init_Syn_CompoundStmt' $ wrap_CompoundStmt' (sem_CompoundStmt' stat) initial
    where
        initial = Inh_CompoundStmt' { label_Inh_CompoundStmt' = 0
                                    , cfg_Inh_CompoundStmt'   = empty }

cfgOfStmt :: CompoundStmt' -> CFG
cfgOfStmt stat
    = cfg_Syn_CompoundStmt' $ wrap_CompoundStmt' (sem_CompoundStmt' stat) initial
    where
        initial = Inh_CompoundStmt' { label_Inh_CompoundStmt' = 0
                                    , cfg_Inh_CompoundStmt'   = empty }
{-# LINE 92 "Complete.hs" #-}
-- CompoundStmt' -----------------------------------------------
data CompoundStmt' = Seq' (CompoundStmt') (CompoundStmt')
                   | Block' (CompoundStmt')
                   | IfThenElse' (Exp) (CompoundStmt') (CompoundStmt')
                   | While' ((Maybe Ident)) (Exp) (CompoundStmt')
                   | Stmt' (Stmt')
                   deriving ( Eq,Show)
-- cata
sem_CompoundStmt' :: (CompoundStmt') ->
                     (T_CompoundStmt')
sem_CompoundStmt' (Seq' _stat1 _stat2) =
    (sem_CompoundStmt'_Seq' (sem_CompoundStmt' _stat1) (sem_CompoundStmt' _stat2))
sem_CompoundStmt' (Block' _stat) =
    (sem_CompoundStmt'_Block' (sem_CompoundStmt' _stat))
sem_CompoundStmt' (IfThenElse' _exp _stat1 _stat2) =
    (sem_CompoundStmt'_IfThenElse' _exp (sem_CompoundStmt' _stat1) (sem_CompoundStmt' _stat2))
sem_CompoundStmt' (While' _ident _exp _body) =
    (sem_CompoundStmt'_While' _ident _exp (sem_CompoundStmt' _body))
sem_CompoundStmt' (Stmt' _stat) =
    (sem_CompoundStmt'_Stmt' (sem_Stmt' _stat))
-- semantic domain
type T_CompoundStmt' = CFG ->
                       Node ->
                       ( CFGNodes,CFG,CFGNodes,CFGNodes,CFGNode,Node,CompoundStmt')
data Inh_CompoundStmt' = Inh_CompoundStmt' {cfg_Inh_CompoundStmt' :: CFG,label_Inh_CompoundStmt' :: Node}
data Syn_CompoundStmt' = Syn_CompoundStmt' {breaks_Syn_CompoundStmt' :: CFGNodes,cfg_Syn_CompoundStmt' :: CFG,continues_Syn_CompoundStmt' :: CFGNodes,final_Syn_CompoundStmt' :: CFGNodes,init_Syn_CompoundStmt' :: CFGNode,label_Syn_CompoundStmt' :: Node,self_Syn_CompoundStmt' :: CompoundStmt'}
wrap_CompoundStmt' :: (T_CompoundStmt') ->
                      (Inh_CompoundStmt') ->
                      (Syn_CompoundStmt')
wrap_CompoundStmt' sem (Inh_CompoundStmt' _lhsIcfg _lhsIlabel) =
    (let ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself) = sem _lhsIcfg _lhsIlabel
     in  (Syn_CompoundStmt' _lhsObreaks _lhsOcfg _lhsOcontinues _lhsOfinal _lhsOinit _lhsOlabel _lhsOself))
sem_CompoundStmt'_Seq' :: (T_CompoundStmt') ->
                          (T_CompoundStmt') ->
                          (T_CompoundStmt')
sem_CompoundStmt'_Seq' stat1_ stat2_ =
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
              _lhsOself :: CompoundStmt'
              _stat1Ibreaks :: CFGNodes
              _stat1Icfg :: CFG
              _stat1Icontinues :: CFGNodes
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: CompoundStmt'
              _stat2Ibreaks :: CFGNodes
              _stat2Icfg :: CFG
              _stat2Icontinues :: CFGNodes
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: CompoundStmt'
              _lhsOlabel =
                  ({-# LINE 36 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 159 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 37 "CFA.ag" #-}
                   _stat1Iinit
                   {-# LINE 164 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 38 "CFA.ag" #-}
                   _stat2Ifinal
                   {-# LINE 169 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 39 "CFA.ag" #-}
                   _stat2Icfg
                   {-# LINE 174 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 40 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 179 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 41 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 184 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 42 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 189 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 43 "CFA.ag" #-}
                   case _stat1Iself of
                      Stmt' (Break' _)
                          -> _stat1Icfg
                      Stmt' (Continue' _)
                          -> _stat1Icfg
                      While' _ _ _
                          -> let conditional = _stat1Iinit
                                 normal      = _stat1Ifinal S.\\ S.singleton conditional in
                                  insEdge (newCondEdge conditional _stat2Iinit False) $
                                  insEdges (newEdges normal _stat2Iinit) _stat1Icfg
                      _   -> insEdges (newEdges _stat1Ifinal _stat2Iinit) _stat1Icfg
                   {-# LINE 204 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   _stat1Ibreaks <> _stat2Ibreaks
                   {-# LINE 209 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   _stat1Icontinues <> _stat2Icontinues
                   {-# LINE 214 "Complete.hs" #-}
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
sem_CompoundStmt'_Block' :: (T_CompoundStmt') ->
                            (T_CompoundStmt')
sem_CompoundStmt'_Block' stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: CompoundStmt'
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
              _statIself :: CompoundStmt'
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   _statIbreaks
                   {-# LINE 249 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   _statIcontinues
                   {-# LINE 254 "Complete.hs" #-}
                   )
              _self =
                  Block' _statIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 32 "CFA.ag" #-}
                   _statIcfg
                   {-# LINE 263 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 29 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 268 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 28 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 273 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 33 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 278 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 32 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 283 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 33 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 288 "Complete.hs" #-}
                   )
              ( _statIbreaks,_statIcfg,_statIcontinues,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_CompoundStmt'_IfThenElse' :: Exp ->
                                 (T_CompoundStmt') ->
                                 (T_CompoundStmt') ->
                                 (T_CompoundStmt')
sem_CompoundStmt'_IfThenElse' exp_ stat1_ stat2_ =
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
              _lhsOself :: CompoundStmt'
              _stat1Ibreaks :: CFGNodes
              _stat1Icfg :: CFG
              _stat1Icontinues :: CFGNodes
              _stat1Ifinal :: CFGNodes
              _stat1Iinit :: CFGNode
              _stat1Ilabel :: Node
              _stat1Iself :: CompoundStmt'
              _stat2Ibreaks :: CFGNodes
              _stat2Icfg :: CFG
              _stat2Icontinues :: CFGNodes
              _stat2Ifinal :: CFGNodes
              _stat2Iinit :: CFGNode
              _stat2Ilabel :: Node
              _stat2Iself :: CompoundStmt'
              _lhsOlabel =
                  ({-# LINE 57 "CFA.ag" #-}
                   _stat2Ilabel
                   {-# LINE 328 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "CFA.ag" #-}
                   node (new _lhsIlabel) _self
                   {-# LINE 333 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 59 "CFA.ag" #-}
                   _stat1Ifinal `S.union` _stat2Ifinal
                   {-# LINE 338 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 60 "CFA.ag" #-}
                   let self = node (new _lhsIlabel) _self in
                      insEdge (newCondEdge self _stat1Iinit True)  $
                      insEdge (newCondEdge self _stat2Iinit False) $
                      insNode self _stat2Icfg
                   {-# LINE 346 "Complete.hs" #-}
                   )
              _stat1Olabel =
                  ({-# LINE 67 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 351 "Complete.hs" #-}
                   )
              _stat1Ocfg =
                  ({-# LINE 68 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 356 "Complete.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 69 "CFA.ag" #-}
                   _stat1Ilabel
                   {-# LINE 361 "Complete.hs" #-}
                   )
              _stat2Ocfg =
                  ({-# LINE 70 "CFA.ag" #-}
                   _stat1Icfg
                   {-# LINE 366 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   _stat1Ibreaks <> _stat2Ibreaks
                   {-# LINE 371 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   _stat1Icontinues <> _stat2Icontinues
                   {-# LINE 376 "Complete.hs" #-}
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
sem_CompoundStmt'_While' :: (Maybe Ident) ->
                            Exp ->
                            (T_CompoundStmt') ->
                            (T_CompoundStmt')
sem_CompoundStmt'_While' ident_ exp_ body_ =
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
              _lhsOself :: CompoundStmt'
              _bodyIbreaks :: CFGNodes
              _bodyIcfg :: CFG
              _bodyIcontinues :: CFGNodes
              _bodyIfinal :: CFGNodes
              _bodyIinit :: CFGNode
              _bodyIlabel :: Node
              _bodyIself :: CompoundStmt'
              _lhsOlabel =
                  ({-# LINE 72 "CFA.ag" #-}
                   _bodyIlabel
                   {-# LINE 413 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 73 "CFA.ag" #-}
                   node (new _lhsIlabel) _self
                   {-# LINE 418 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 74 "CFA.ag" #-}
                   S.insert (node (new _lhsIlabel) _self) (S.filter (isLabelOfThisLoop ident_) _bodyIbreaks)
                   {-# LINE 423 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 75 "CFA.ag" #-}
                   let self = node (new _lhsIlabel) _self in
                      insEdge (newCondEdge self _bodyIinit True)                                              $
                      insEdges (newEdges _bodyIfinal self)                                           $
                      insEdges (newEdges (S.filter (isLabelOfThisLoop ident_) _bodyIcontinues) self) $
                      insNode self _bodyIcfg
                   {-# LINE 432 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 84 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIbreaks
                   {-# LINE 437 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 85 "CFA.ag" #-}
                   S.filter (not . isLabelOfThisLoop ident_) _bodyIcontinues
                   {-# LINE 442 "Complete.hs" #-}
                   )
              _bodyOlabel =
                  ({-# LINE 86 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 447 "Complete.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 87 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 452 "Complete.hs" #-}
                   )
              _self =
                  While' ident_ exp_ _bodyIself
              _lhsOself =
                  _self
              ( _bodyIbreaks,_bodyIcfg,_bodyIcontinues,_bodyIfinal,_bodyIinit,_bodyIlabel,_bodyIself) =
                  body_ _bodyOcfg _bodyOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_CompoundStmt'_Stmt' :: (T_Stmt') ->
                           (T_CompoundStmt')
sem_CompoundStmt'_Stmt' stat_ =
    (\ _lhsIcfg
       _lhsIlabel ->
         (let _lhsObreaks :: CFGNodes
              _lhsOcontinues :: CFGNodes
              _lhsOself :: CompoundStmt'
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
                  ({-# LINE 30 "CFA.ag" #-}
                   _statIbreaks
                   {-# LINE 485 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   _statIcontinues
                   {-# LINE 490 "Complete.hs" #-}
                   )
              _self =
                  Stmt' _statIself
              _lhsOself =
                  _self
              _lhsOcfg =
                  ({-# LINE 32 "CFA.ag" #-}
                   _statIcfg
                   {-# LINE 499 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 29 "CFA.ag" #-}
                   _statIfinal
                   {-# LINE 504 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 28 "CFA.ag" #-}
                   _statIinit
                   {-# LINE 509 "Complete.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 33 "CFA.ag" #-}
                   _statIlabel
                   {-# LINE 514 "Complete.hs" #-}
                   )
              _statOcfg =
                  ({-# LINE 32 "CFA.ag" #-}
                   _lhsIcfg
                   {-# LINE 519 "Complete.hs" #-}
                   )
              _statOlabel =
                  ({-# LINE 33 "CFA.ag" #-}
                   _lhsIlabel
                   {-# LINE 524 "Complete.hs" #-}
                   )
              ( _statIbreaks,_statIcfg,_statIcontinues,_statIfinal,_statIinit,_statIlabel,_statIself) =
                  stat_ _statOcfg _statOlabel
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
-- Exp' --------------------------------------------------------
data Exp' = LitInt' (Integer)
          | LitFloat' (Double)
          | LitDouble' (Double)
          | LitBool' (Bool)
          | LitNull'
          | PreNot' (Exp')
          | BinOp' (Exp') (Op) (Exp')
          deriving ( Eq,Show)
-- cata
sem_Exp' :: (Exp') ->
            (T_Exp')
sem_Exp' (LitInt' _value) =
    (sem_Exp'_LitInt' _value)
sem_Exp' (LitFloat' _value) =
    (sem_Exp'_LitFloat' _value)
sem_Exp' (LitDouble' _value) =
    (sem_Exp'_LitDouble' _value)
sem_Exp' (LitBool' _value) =
    (sem_Exp'_LitBool' _value)
sem_Exp' (LitNull') =
    (sem_Exp'_LitNull')
sem_Exp' (PreNot' _exp) =
    (sem_Exp'_PreNot' (sem_Exp' _exp))
sem_Exp' (BinOp' _exp1 _op _exp2) =
    (sem_Exp'_BinOp' (sem_Exp' _exp1) _op (sem_Exp' _exp2))
-- semantic domain
type T_Exp' = ( Exp')
data Inh_Exp' = Inh_Exp' {}
data Syn_Exp' = Syn_Exp' {self_Syn_Exp' :: Exp'}
wrap_Exp' :: (T_Exp') ->
             (Inh_Exp') ->
             (Syn_Exp')
wrap_Exp' sem (Inh_Exp') =
    (let ( _lhsOself) = sem
     in  (Syn_Exp' _lhsOself))
sem_Exp'_LitInt' :: Integer ->
                    (T_Exp')
sem_Exp'_LitInt' value_ =
    (let _lhsOself :: Exp'
         _self =
             LitInt' value_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Exp'_LitFloat' :: Double ->
                      (T_Exp')
sem_Exp'_LitFloat' value_ =
    (let _lhsOself :: Exp'
         _self =
             LitFloat' value_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Exp'_LitDouble' :: Double ->
                       (T_Exp')
sem_Exp'_LitDouble' value_ =
    (let _lhsOself :: Exp'
         _self =
             LitDouble' value_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Exp'_LitBool' :: Bool ->
                     (T_Exp')
sem_Exp'_LitBool' value_ =
    (let _lhsOself :: Exp'
         _self =
             LitBool' value_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Exp'_LitNull' :: (T_Exp')
sem_Exp'_LitNull' =
    (let _lhsOself :: Exp'
         _self =
             LitNull'
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Exp'_PreNot' :: (T_Exp') ->
                    (T_Exp')
sem_Exp'_PreNot' exp_ =
    (let _lhsOself :: Exp'
         _expIself :: Exp'
         _self =
             PreNot' _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Exp'_BinOp' :: (T_Exp') ->
                   Op ->
                   (T_Exp') ->
                   (T_Exp')
sem_Exp'_BinOp' exp1_ op_ exp2_ =
    (let _lhsOself :: Exp'
         _exp1Iself :: Exp'
         _exp2Iself :: Exp'
         _self =
             BinOp' _exp1Iself op_ _exp2Iself
         _lhsOself =
             _self
         ( _exp1Iself) =
             exp1_
         ( _exp2Iself) =
             exp2_
     in  ( _lhsOself))
-- Stmt' -------------------------------------------------------
data Stmt' = VarDecl' (([Modifier])) (Type) (([VarDecl]))
           | Empty'
           | ExpStmt' (Exp)
           | Assert' (Exp) ((Maybe Exp))
           | Assume' (Exp) ((Maybe Exp))
           | Break' ((Maybe Ident))
           | Continue' ((Maybe Ident))
           | Return' ((Maybe Exp))
           deriving ( Eq,Show)
-- cata
sem_Stmt' :: (Stmt') ->
             (T_Stmt')
sem_Stmt' (VarDecl' _modifiers _ty _vars) =
    (sem_Stmt'_VarDecl' _modifiers _ty _vars)
sem_Stmt' (Empty') =
    (sem_Stmt'_Empty')
sem_Stmt' (ExpStmt' _exp) =
    (sem_Stmt'_ExpStmt' _exp)
sem_Stmt' (Assert' _exp _error) =
    (sem_Stmt'_Assert' _exp _error)
sem_Stmt' (Assume' _exp _error) =
    (sem_Stmt'_Assume' _exp _error)
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
sem_Stmt'_VarDecl' :: ([Modifier]) ->
                      Type ->
                      ([VarDecl]) ->
                      (T_Stmt')
sem_Stmt'_VarDecl' modifiers_ ty_ vars_ =
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
                  ({-# LINE 90 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 696 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 91 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 701 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 92 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 706 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 93 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 711 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 716 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 721 "Complete.hs" #-}
                   )
              _self =
                  VarDecl' modifiers_ ty_ vars_
              _lhsOself =
                  _self
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
                  ({-# LINE 95 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 742 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 96 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 747 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 752 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 98 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 757 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 762 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 767 "Complete.hs" #-}
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
                  ({-# LINE 100 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 789 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 101 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 794 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 102 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 799 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 103 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 804 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 809 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 814 "Complete.hs" #-}
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
                  ({-# LINE 105 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 837 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 106 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 842 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 107 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 847 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 108 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 852 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 857 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 862 "Complete.hs" #-}
                   )
              _self =
                  Assert' exp_ error_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))
sem_Stmt'_Assume' :: Exp ->
                     (Maybe Exp) ->
                     (T_Stmt')
sem_Stmt'_Assume' exp_ error_ =
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
                  ({-# LINE 110 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 885 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 111 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 890 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 112 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 895 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 113 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 900 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 905 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 910 "Complete.hs" #-}
                   )
              _self =
                  Assume' exp_ error_
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
                  ({-# LINE 115 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 932 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 116 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 937 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 117 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 942 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 118 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 947 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 119 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 952 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 957 "Complete.hs" #-}
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
                  ({-# LINE 121 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 979 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 122 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 984 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 123 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 989 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 124 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 994 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 125 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 999 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 1004 "Complete.hs" #-}
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
                  ({-# LINE 127 "CFA.ag" #-}
                   new _lhsIlabel
                   {-# LINE 1026 "Complete.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 128 "CFA.ag" #-}
                   node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 1031 "Complete.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 129 "CFA.ag" #-}
                   S.singleton $ node (new _lhsIlabel) (Stmt' _self)
                   {-# LINE 1036 "Complete.hs" #-}
                   )
              _lhsOcfg =
                  ({-# LINE 130 "CFA.ag" #-}
                   insNode (node (new _lhsIlabel) (Stmt' _self)) _lhsIcfg
                   {-# LINE 1041 "Complete.hs" #-}
                   )
              _lhsObreaks =
                  ({-# LINE 30 "CFA.ag" #-}
                   mempty
                   {-# LINE 1046 "Complete.hs" #-}
                   )
              _lhsOcontinues =
                  ({-# LINE 31 "CFA.ag" #-}
                   mempty
                   {-# LINE 1051 "Complete.hs" #-}
                   )
              _self =
                  Return' exp_
              _lhsOself =
                  _self
          in  ( _lhsObreaks,_lhsOcfg,_lhsOcontinues,_lhsOfinal,_lhsOinit,_lhsOlabel,_lhsOself)))