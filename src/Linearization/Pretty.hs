module Linearization.Pretty where

import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty
import Linearization.Path

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)
    
instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty PathStmt where
    pretty (s,_) = pretty s

instance Pretty PathType where
    pretty (PathStmt s)  = pretty s
    pretty (PathEntry t) = text "entry of" <+> pretty t
    pretty (PathExit t)  = text "exit of" <+> pretty t

instance Pretty EntryType where
    pretty ETBlock        = text "block"
    pretty ETTry          = text "try"
    pretty (ETCatch ty)   = text "catch" <+> pretty ty
    pretty ETFinally      = text "finally"
    pretty (ETMethod s _) = text "method" <+> quotes (text s)