module Linearization.Pretty where

import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty
import Analysis.Pretty
import Linearization.Path

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)
    
instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty PathStmt where
    pretty (stat,_) = pretty stat

instance Pretty PathType where
    pretty (PathStmt stat)  
        = pretty stat

    pretty (PathEntry entry) 
        = char '{'

    pretty (PathExit exit)  
        = char '}'
