module Linearization.Renaming where

import Analysis.CFG     (Scope(..))
import Analysis.Pretty  ()
import Auxiliary.Pretty 

renameMethodCall :: Scope -> Int -> String
renameMethodCall (Scope package className methodName) numberOfCallsMade
    = package' ++ className ++ "_" ++ methodName ++ "$" ++ show numberOfCallsMade
    where
        package' = maybe "" (\ p -> toString (dots p) ++ "_") package