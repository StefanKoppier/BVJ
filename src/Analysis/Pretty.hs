module Analysis.Pretty where

import Data.Graph.Inductive.Graph (Node, context, nodes)
import Text.PrettyPrint
import Analysis.CFG
import Parsing.Pretty
import Auxiliary.Pretty

instance Pretty CFG where
    pretty CFG{cfg} = foldr (($+$) . pretty) empty contexts
        where
            contexts = map (context cfg) . nodes $ cfg

instance Pretty CFGContext where
    pretty (_,n,v,ns) = int n <> text "->" <> pretty ns <+> pretty v

instance Pretty CFGNodeValue where
    pretty (Block s) = pretty s
    pretty (Call n)  = "call of" <+> quotes (dots n)
    pretty (Entry n) = "entry of" <+> quotes (dots n)
    pretty (Exit n)  = "exit of" <+> quotes (dots n)

instance Pretty CFGAdj where
    pretty = brackets . commas

instance Pretty (CFGEdgeValue, Node) where
    pretty (_,n) = pretty n