module Auxiliary.Verbosity(
        Verbosity(..)
) where
    
data Verbosity
    = Quiet
    | Phases
    | Subphases
    | Everything
    deriving (Eq, Ord)

{-
whenFR :: Verbosity -> IO () -> IO ()
whenFR = whenVerbosity FinalResult

whenP :: Verbosity -> IO () -> IO ()
whenP = whenVerbosity Phases

whenS :: Verbosity -> IO () -> IO ()
whenS = whenVerbosity Subphases

whenE :: Verbosity -> IO () -> IO ()
whenE = whenVerbosity Everything

whenVerbosity :: Verbosity -> Verbosity -> IO () -> IO ()
whenVerbosity verbosity actual = when (verbosity <= actual)
-}