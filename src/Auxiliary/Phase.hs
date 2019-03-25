module Auxiliary.Phase(
      module Control.Monad.Trans.Except
    , module Control.Monad.IO.Class
    , module Auxiliary.Arguments

    , PhaseResult
    , PhaseError(..)
    , SemanticalError(..)
    , throwParsingError
    , throwSemanticalError
    , throwSyntacticalError
    , throwResultError
    , throwExternalError
    , workingDir
    , Phase
    , Subphase
) where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Parsing.Syntax
import Analysis.CFG
import Auxiliary.Arguments

workingDir :: FilePath
workingDir = "tmp_verification_folder"

--------------------------------------------------------------------------------
-- Phasing
--------------------------------------------------------------------------------

type PhaseResult a = ExceptT PhaseError IO a

data PhaseError
    = ParsingError     String
    | SemanticalError  SemanticalError
    | SyntacticalError String
    | ResultError      String
    | ExternalError    String
    deriving (Show, Eq)

data SemanticalError
    = UndefinedMethodReference Name'
    | UndefinedClassReference  String
    deriving (Show, Eq)

throwParsingError :: String -> PhaseResult a
throwParsingError = throwE . ParsingError

throwSemanticalError :: SemanticalError -> PhaseResult a
throwSemanticalError = throwE . SemanticalError

throwSyntacticalError :: String -> PhaseResult a
throwSyntacticalError = throwE . SyntacticalError

throwResultError :: String -> PhaseResult a
throwResultError = throwE . ResultError

throwExternalError :: String -> PhaseResult a
throwExternalError = throwE . ExternalError

type Phase a b = Arguments -> a -> PhaseResult b

type Subphase a b = Phase a b
