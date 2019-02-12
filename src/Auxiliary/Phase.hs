module Auxiliary.Phase(
      module Control.Monad.Trans.Either
    , module Auxiliary.Arguments
    , module Auxiliary.Verbosity
    , PhaseResult
    , PhaseError(..)
    , Phase
    , Subphase
) where

import Control.Monad.Trans.Either
import Auxiliary.Verbosity
import Auxiliary.Arguments

--------------------------------------------------------------------------------
-- Phasing
--------------------------------------------------------------------------------

type PhaseResult a = EitherT PhaseError IO a

data PhaseError
    = ParseError        String
    | MethodNotFound    String
    | UnsupportedSyntax String
    | ResultParseError  String
    deriving (Show, Eq)

type Phase a b = Arguments -> a -> PhaseResult b

type Subphase a b = Phase a b
