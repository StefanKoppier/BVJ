module Linearization.Path where
    
import Text.PrettyPrint
import Parsing.Syntax

type ProgramPaths = [ProgramPath]

type ProgramPath = [PathStmt]

type PathStmt = (PathType, PathMetaInfo)

type PathMetaInfo = (Scope, String)

data PathType
    = PathStmt  Stmt'
    | PathEntry EntryType
    | PathExit  EntryType
    deriving (Show, Eq)

data EntryType
    = ETBlock   
    | ETTry     
    | ETCatch   FormalParam'
    | ETFinally 
    | ETMethod  String Scope
    deriving (Show, Eq)

hasMethodEntryType :: PathStmt -> Bool
hasMethodEntryType (PathEntry (ETMethod _ _), _) = False
hasMethodEntryType (PathExit  (ETMethod _ _), _) = False
hasMethodEntryType _                             = True

{-
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
-}