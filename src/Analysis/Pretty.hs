{-|
Module      : Analysis.Pretty
Description : Pretty instances of the data types declared in Analysis.CFG.
-}
module Analysis.Pretty where

import Data.Graph.Inductive.Graph (Node, context, nodes)
import Text.PrettyPrint
import Analysis.CFG
import Parsing.Pretty()
import Auxiliary.Pretty

instance Pretty CFG where
    pretty CFG{cfg} = foldr (($+$) . pretty) empty contexts
        where
            contexts = map (context cfg) . nodes $ cfg

instance Pretty CFGContext where
    pretty (_,n,v,ns) = int n <> text "->" <> pretty ns <+> pretty v

instance Pretty CFGNodeValue where
    pretty (StatNode s)    
        = pretty s

    pretty (ForInitNode forInit)
        = pretty forInit

    pretty (ForUpdateNode update)
        = commas update

    pretty (CatchNode c)    
        = pretty c
        
    pretty (FinallyNode s)    
        = "finally" $+$ lbrace $+$ tab (pretty s) $+$ rbrace

    pretty (CallNode s n _) 
        = "call of" <+> quotes (pretty s) <+> "belonging to" <+> pretty n

    pretty (MethodEntryNode s)    
        = "entry of" <+> quotes (pretty s)

    pretty (MethodExitNode s)
        = "exit of"  <+> quotes (pretty s)

instance Pretty CFGAdj where
    pretty = brackets . commas

instance Pretty (CFGEdgeValue, Node) where
    pretty (_, n) = pretty n
    