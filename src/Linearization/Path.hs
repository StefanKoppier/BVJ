module Linearization.Path where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty

type ProgramPath = [Stmt']

type ProgramPaths = [ProgramPath]

instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty ProgramPaths where
    pretty = foldr (($+$) . (brackets . pretty)) empty