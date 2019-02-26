module Linearization.Path where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty

type ScopedStmt = (Name', Stmt')

type PathStmt = (Stmt', StmtPathInfo)

data StmtPathInfo = StmtPathInfo {
      callName   :: String
    , methodName :: String
    , className  :: String
    }

type ProgramPath = [ScopedStmt]

type ProgramPaths = [ProgramPath]

instance Pretty ScopedStmt where
    pretty (scope, stmt) = parens (dots scope <> comma <> pretty stmt)

instance Pretty PathStmt where
    pretty (s,_) = pretty s

instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)