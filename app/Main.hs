module Main where

import Auxiliary.Phase
import Auxiliary.Pretty (Verbosity(..))
import Complete

perform :: Arguments -> FilePath -> IO ()
perform args file = do
    content <- readFile file
    result  <- runExceptT $ allPhases args content
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right _       -> return ()

arguments :: Arguments
arguments = defaultArgs {
      verbosity       = Informative {-Compact-}
    , numberOfThreads = 4
    , maximumDepth    = 20
    , maximumUnwind   = Nothing
    , keepOutputFiles = True
    }

main :: IO ()
main = perform arguments "examples/Test.java"