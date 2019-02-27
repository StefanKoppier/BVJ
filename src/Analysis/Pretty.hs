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
    pretty (Block s)    = pretty s
    pretty (Call s n _) = "call of"  <+> quotes (pretty s) <+> "belonging to" <+> pretty n
    pretty (Entry s)    = "entry of" <+> quotes (pretty s)
    pretty (Exit s)     = "exit of"  <+> quotes (pretty s)

instance Pretty Scope where
    pretty (Scope scopePackage scopeClass scopeMember)
        = maybe empty (const (package' <> dot)) scopePackage <> dots [class', member']
        where
            package' = maybe empty dots scopePackage
            class'   = text scopeClass
            member'  = text scopeMember

instance Pretty CFGAdj where
    pretty = brackets . commas

instance Pretty (CFGEdgeValue, Node) where
    pretty (_,n) = pretty n