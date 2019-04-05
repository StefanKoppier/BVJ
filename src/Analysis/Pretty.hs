module Analysis.Pretty where

import Data.Graph.Inductive.Graph (Node, context, nodes)
import Text.PrettyPrint
import Analysis.CFG
import Parsing.Syntax
import Parsing.Utility
import Parsing.Pretty
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
    pretty (v, n) = parens (pretty v <> comma <> pretty n)

instance Pretty CFGEdgeValue where
    pretty (InterEdge _) 
        = "inter edge"

    pretty IntraEdge     
        = "intra edge"

    pretty (BlockEntryEdge entryType)
        = "scope entry edge of" <+> quotes (pretty entryType)

    pretty (BlockExitEdge entryType)
        = "scope exit edge of" <+> quotes (pretty entryType)
        
    pretty (BlockExitEntryEdge exitType entryType)
        = "scope exit and entry edge of" 
            <+> quotes (pretty exitType) <+> "to" <+> quotes (pretty entryType)

    pretty (BlockExitsEdge exits)
        = "scope exits"

instance Pretty BlockEntryType where
    pretty TryEntryType = "try"

    pretty (CatchEntryType exception)
        = "catch" <+> maybe empty pretty exception

    pretty (ConditionalEntryType exp)
        = "conditional" <+> maybe empty pretty exp
        
    pretty FinallyEntryType = "finally"

    pretty BlockEntryType = "block"