module Auxiliary.Arguments(
      module Parsing.Syntax
    , module Linearization.Path  
    , Arguments(..)
    , defaultArgs
) where
    
import Auxiliary.Pretty   (Verbosity(..))
import Parsing.Syntax     
import Linearization.Path

data Arguments = Arguments {
      verbosity            :: Verbosity
    , numberOfThreads      :: Int
    , keepOutputFiles      :: Bool
    , maximumDepth         :: Int
    , pathFilter           :: CompilationUnit' -> ProgramPaths -> ProgramPaths
    , jbmcEnableAssertions :: Bool
    , jbmcDepth            :: Maybe Int
    , jbmcUnwind           :: Maybe Int
  }

defaultArgs :: Arguments
defaultArgs = Arguments {
      verbosity            = Informative
    , numberOfThreads      = 4
    , keepOutputFiles      = False
    , maximumDepth         = 100
    , pathFilter           = const id
    , jbmcEnableAssertions = True
    , jbmcDepth            = Nothing
    , jbmcUnwind           = Nothing
}