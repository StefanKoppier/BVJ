module Linearization.Path where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty

type PathStmt = (Stmt', PathStmtInfo)

data PathStmtInfo = PathStmtInfo {
      callName :: String -- ^ The name of the method the statement belongs to,
                         --   which is unique for every method call.
    , original :: Scope  -- ^ The original scope the statement belongs to.
    }

type ProgramPath = [PathStmt]

type ProgramPaths = [ProgramPath]

instance Pretty PathStmt where
    pretty (s,_) = pretty s

instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)