module Auxiliary.Arguments(
      module Parsing.Syntax
    , Arguments(..)
    , defaultArgs
) where
    
import Auxiliary.Verbosity
import Parsing.Syntax      (Scope(..))

data Arguments = Arguments {
      method                   :: Scope
    , verbosity                :: Verbosity
    , numberOfThreads          :: Int
    , keepOutputFiles          :: Bool
    , maximumDepth             :: Int
    , enableAssertions         :: Bool
    , enableArrayBoundsCheck   :: Bool
    , enablePointerChecks      :: Bool
    , enableDivByZeroCheck     :: Bool
    , enableIntOverflowCheck   :: Bool
    , enableShiftCheck         :: Bool
    , enableFloatOverflowCheck :: Bool
    , enableNaNCheck           :: Bool
    , includePaths             :: [FilePath]
}

defaultArgs :: Arguments
defaultArgs = Arguments {
      method = Scope Nothing "main" "main"
    , verbosity = Informative
    , numberOfThreads = 4
    , keepOutputFiles = False
    , maximumDepth = 100
    , enableAssertions = True
    , enableArrayBoundsCheck = True
    , enablePointerChecks = True
    , enableDivByZeroCheck = True
    , enableIntOverflowCheck = True
    , enableShiftCheck = True
    , enableFloatOverflowCheck = True
    , enableNaNCheck = True
    , includePaths = []
}