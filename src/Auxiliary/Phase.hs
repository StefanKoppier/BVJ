module Auxiliary.Phase(
      module Control.Monad.Trans.Except
    , module Control.Monad.IO.Class
    , module Auxiliary.Arguments

    , PhaseResult
    , PhaseError
    , SemanticalError(..)
    , parsingError
    , semanticalError
    , syntacticalError
    , resultError
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
    deriving (Show, Eq)

data SemanticalError
    = UndefinedMethodReference Name'
    | UndefinedClassReference  String
    deriving (Show, Eq)

parsingError :: String -> PhaseResult a
parsingError = throwE . ParsingError

semanticalError :: SemanticalError -> PhaseResult a
semanticalError = throwE . SemanticalError

syntacticalError :: String -> PhaseResult a
syntacticalError = throwE . SyntacticalError

resultError :: String -> PhaseResult a
resultError = throwE . ResultError

type Phase a b = Arguments -> a -> PhaseResult b

type Subphase a b = Phase a b
