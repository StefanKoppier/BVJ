module Translation.Pretty where

import qualified Language.C.Pretty   as C
import           Text.PrettyPrint
import           Auxiliary.Pretty
import           Translation.Program

instance Pretty Program where
    pretty = C.pretty

instance Pretty Programs where
    pretty = vcat . punctuate (char '\n') . map pretty