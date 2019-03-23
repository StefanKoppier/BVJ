module Linearization.Path where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax
import Parsing.Pretty

data PathStmtInfo = PathStmtInfo {
      callName :: String
    , origin   :: Scope
    , depth    :: Int
} deriving (Show)

type PathStmt = (Stmt', PathStmtInfo)

type ProgramPath = [PathStmt]

type ProgramPaths = [ProgramPath]

instance Pretty PathStmt where
    pretty (s,_) = pretty s

instance Pretty ProgramPath where
    pretty = hsep . map pretty

instance Pretty ProgramPaths where
    pretty = foldr ($+$) empty . punctuate newline . map (brackets . pretty)