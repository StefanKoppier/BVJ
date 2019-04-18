{-|
Module      : Linearization.Pretty
Description : Pretty instances of the data types declared in Linearization.Path.
-}
module Linearization.Pretty where

import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Pretty()
import Analysis.Pretty()
import Linearization.Path

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)
    
instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty PathStmt where
    pretty (stat, _) = pretty stat

instance Pretty PathType where
    pretty (PathStmt stat)  
        = pretty stat

    pretty (PathEntry TryEntryType) 
        = "try" <+> lbrace
        
    pretty (PathEntry (CatchEntryType (Just e))) 
        = "catch" <+> parens (pretty e) <+> lbrace
    
    pretty (PathEntry FinallyEntryType) 
        = "finally" <+> lbrace
        
    pretty (PathEntry _) 
        = lbrace

    pretty (PathExit _)  
        = rbrace
