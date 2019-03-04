module Compilation.Pretty where

import qualified Language.C.Pretty     as C
import           Text.PrettyPrint
import           Auxiliary.Pretty
import           Compilation.CProgram

instance Pretty CProgram where
    pretty = C.pretty

instance Pretty CPrograms where
    pretty = vcat . punctuate (char '\n') . map pretty