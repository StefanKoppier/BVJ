module Auxiliary.Verbosity(
        Verbosity(..)
) where
    
data Verbosity
    = Compact
    | Informative
    deriving (Eq, Ord)