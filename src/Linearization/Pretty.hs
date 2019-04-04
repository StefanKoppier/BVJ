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
        = text "entry of" <+> pretty entry

    pretty (PathExit exit)  
        = text "exit of" <+> pretty exit

    pretty (PathExitEntry entry exit)  
        = text "exit and entry of" <+> pretty entry <+> pretty exit

{-
instance Pretty EntryType where
    pretty ETBlock        = text "block"
    pretty ETTry          = text "try"
    pretty (ETCatch ty)   = text "catch" <+> pretty ty
    pretty ETFinally      = text "finally"
    pretty (ETMethod s _) = text "method" <+> quotes (text s)
-}