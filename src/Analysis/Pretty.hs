module Analysis.Pretty where

import Data.Graph.Inductive.Graph (ufold)
import Text.PrettyPrint
import Analysis.CFG
import Parsing.Pretty
import Auxiliary.Pretty

instance Pretty CFG where
    pretty (CFG gr initial) =   text "initial node" <+> int initial 
                            $+$ ufold (($+$) . pretty) empty gr

instance Pretty CFGContext where
    pretty (_,n,v,ns) = int n <> text "->" <> pretty ns <+> pretty v

instance Pretty CFGAdj where
    pretty = brackets . hcat . punctuate comma . map (int . snd)

instance Pretty ECFG where
    pretty = ufold (($+$) . pretty) empty

instance Pretty ECFGNodeValue where
    pretty (name, cfg) = name' <+> text "with" <+> pretty cfg
        where
            name' = text "method" <+> quotes ((hcat . punctuate dot . map pretty) name)

instance Pretty ECFGContext where
    pretty (_,n,v,ns) = int n <> text "->" <> pretty ns <+> pretty v
    
instance Pretty ECFGAdj where
    pretty = brackets . hcat . punctuate comma . map (int . snd)