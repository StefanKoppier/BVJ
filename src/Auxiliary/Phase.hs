{-|
Module      : Auxiliary.Phase
Description : Module containing the phasing infrastructure.
-}
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

-- | The directory to compile and verify the program paths in.
workingDir :: FilePath
workingDir = "tmp_verification_folder"

--------------------------------------------------------------------------------
-- Phasing
--------------------------------------------------------------------------------

-- | Type declaration of a phase result, allowing for exceptions in the IO monad.
type PhaseResult a = ExceptT PhaseError IO a

-- | Type declaration of a phase.
type Phase a b = Arguments -> a -> PhaseResult b

-- | Type declaration of a subphase.
type Subphase a b = Phase a b

-- | Data type containing the different errors.
data PhaseError
    = ParsingError     String
    | SemanticalError  SemanticalError
    | SyntacticalError String
    | ResultError      String
    | ExternalError    String
    deriving (Show, Eq)

-- | Data type containing the different semantical errors.
data SemanticalError
    = UndefinedMethodReference Name'
    | UndefinedClassReference  String
    deriving (Show, Eq)

-- | Throws a parsing error.
throwParsingError :: String -> PhaseResult a
throwParsingError = throwE . ParsingError

-- | Throws a semantical error.
throwSemanticalError :: SemanticalError -> PhaseResult a
throwSemanticalError = throwE . SemanticalError

-- | Throws a syntactical error.
throwSyntacticalError :: String -> PhaseResult a
throwSyntacticalError = throwE . SyntacticalError

-- | Throws a result error.
throwResultError :: String -> PhaseResult a
throwResultError = throwE . ResultError

-- | Throws an external error.
throwExternalError :: String -> PhaseResult a
throwExternalError = throwE . ExternalError
