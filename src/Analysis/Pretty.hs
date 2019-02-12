module Analysis.Pretty where

import Data.Graph.Inductive.Graph (ufold)
import Text.PrettyPrint
import Analysis.CFG
import Parsing.Pretty
import Auxiliary.Pretty

instance Pretty CFG where
    pretty = ufold (($+$) . pretty) empty

instance Pretty CFGContext where
    pretty (_,n,v,ns) = int n <> text "->" <> pretty ns <+> pretty v

instance Pretty CFGAdj where
    pretty = brackets . hcat . punctuate comma . map (int . snd)