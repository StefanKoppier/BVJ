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
        = char '{' <+> quotes (pretty entry)
        --text "entry of" <+> pretty entry

    pretty (PathExit exit)  
        = char '}' <+> quotes (pretty exit)
            --text "exit of" <+> pretty exit
{-
    pretty (PathExitEntry exit entry)  
        = char '}' <+> char '{'
        --text "exit and entry of" <+> pretty entry <+> pretty exit
-}