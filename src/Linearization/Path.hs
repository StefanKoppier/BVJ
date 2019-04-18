{-|
Module      : Auxiliary.Phase
Description : Module containing data types representing a program path.
-}
module Linearization.Path(
      module Analysis.CFG
    , ProgramPaths
    , ProgramPath
    , PathStmt
    , PathMetaInfo
    , PathType(..)
) where
    
import Analysis.CFG   (BlockEntryType(..))
import Parsing.Syntax

-- | Type declaration of multiple program paths.
type ProgramPaths = [ProgramPath]

-- | Type declaration of one program path.
type ProgramPath = [PathStmt]

-- | Type declaration of a single statement, a block entry or a block exit, 
-- in a program path.
type PathStmt = (PathType, PathMetaInfo)

-- | Type declaration of extra information of a PathStmt.
type PathMetaInfo = (Scope, String)

-- | Data type representing the a statement, a block entry, or a block exit.
data PathType
    = PathStmt      Stmt'
    | PathEntry     BlockEntryType
    | PathExit      BlockEntryType
    deriving (Show, Eq)
