module Auxiliary.Arguments(
      module Parsing.Syntax
    , Arguments(..)
    , defaultArgs
) where
    
import Auxiliary.Pretty (Verbosity(..))
import Parsing.Syntax   (Scope(..))

data Arguments = Arguments {
      verbosity         :: Verbosity
    , numberOfThreads   :: Int
    , keepOutputFiles   :: Bool
    , maximumDepth      :: Int
    , maximumUnwind     :: Maybe Int
    , enableAssertions  :: Bool
  }

defaultArgs :: Arguments
defaultArgs = Arguments {
      verbosity = Informative
    , numberOfThreads = 4
    , keepOutputFiles = False
    , maximumDepth = 100
    , maximumUnwind = Nothing
    , enableAssertions = True
}