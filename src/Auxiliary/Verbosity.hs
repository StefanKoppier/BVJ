module Auxiliary.Verbosity(
        Verbosity(..)
) where
    
data Verbosity
    = Quiet
    | Phases
    | Subphases
    | Everything
    deriving (Eq, Ord)