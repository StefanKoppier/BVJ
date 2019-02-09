module Arguments where
    
import Control.Verbosity

data Arguments = Arguments {
      method                   :: String
    , verbosity                :: Verbosity
    , maximumDepth             :: Int
    , enableAssertions         :: Bool
    , enableArrayBoundsCheck   :: Bool
    , enablePointerChecks      :: Bool
    , enableDivByZeroCheck     :: Bool
    , enableIntOverflowCheck   :: Bool
    , enableShiftCheck         :: Bool
    , enableFloatOverflowCheck :: Bool
    , enableNaNCheck           :: Bool
}

defaultArgs :: Arguments
defaultArgs = Arguments {
      method = "main"
    , verbosity = Everything
    , maximumDepth = 100
    , enableAssertions = True
    , enableArrayBoundsCheck = True
    , enablePointerChecks = True
    , enableDivByZeroCheck = True
    , enableIntOverflowCheck = True
    , enableShiftCheck = True
    , enableFloatOverflowCheck = True
    , enableNaNCheck = True
}