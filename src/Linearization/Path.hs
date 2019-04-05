module Linearization.Path(
      module Analysis.CFG
    , ProgramPaths
    , ProgramPath
    , PathStmt
    , PathMetaInfo
    , PathType(..)
) where
    
import Text.PrettyPrint
import Analysis.CFG     (BlockEntryType(..))
import Parsing.Syntax

type ProgramPaths = [ProgramPath]

type ProgramPath = [PathStmt]

type PathStmt = (PathType, PathMetaInfo)

type PathMetaInfo = (Scope, String)

data PathType
    = PathStmt      Stmt'
    | PathEntry     BlockEntryType
    | PathExit      BlockEntryType
--    | PathExitEntry BlockEntryType BlockEntryType
    deriving (Show, Eq)
