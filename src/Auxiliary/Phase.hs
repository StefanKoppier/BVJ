module Auxiliary.Phase(
      module Control.Monad.Trans.Either
    , module Auxiliary.Arguments
    , module Auxiliary.Verbosity

    , PhaseResult
    , PhaseError
    , SemanticalError(..)
    , parsingError
    , semanticalError
    , syntacticalError
    , resultError

    , Phase
    , Subphase
) where

import Control.Monad.Trans.Either
import Parsing.Syntax
import Analysis.CFG
import Auxiliary.Verbosity
import Auxiliary.Arguments

--------------------------------------------------------------------------------
-- Phasing
--------------------------------------------------------------------------------

type PhaseResult a = EitherT PhaseError IO a

data PhaseError
    = ParsingError       String
    | SemanticalError  SemanticalError
    | SyntacticalError String
    | ResultError      String
    deriving (Show, Eq)

data SemanticalError
    = UndefinedMethodReference Scope
    | UndefinedClassReference  String
    deriving (Show, Eq)

parsingError :: String -> PhaseResult a
parsingError = left . ParsingError

semanticalError :: SemanticalError -> PhaseResult a
semanticalError = left . SemanticalError

syntacticalError :: String -> PhaseResult a
syntacticalError = left . SyntacticalError

resultError :: String -> PhaseResult a
resultError = left . ResultError

type Phase a b = Arguments -> a -> PhaseResult b

type Subphase a b = Phase a b
