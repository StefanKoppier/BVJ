module Main where

import Auxiliary.Phase
import Complete

perform :: Arguments -> FilePath -> IO ()
perform args file = do
    content <- readFile file
    result  <- runEitherT $ allPhases args content
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right _       -> return ()

arguments :: Arguments
arguments = defaultArgs {
      method          = ["main"]
    , maximumDepth    = 50
    , keepOutputFiles = False
    }

main :: IO ()
main = perform arguments "examples/Test.java"