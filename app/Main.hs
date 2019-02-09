module Main where

import Control.Phase
import Complete

perform :: Arguments -> FilePath -> IO ()
perform args file = do
    content <- readFile file
    result  <- runEitherT $ allPhases args content
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right results -> do
            putStrLn "Final result: \n"
            mapM_ print results

arguments :: Arguments
arguments = defaultArgs {
      method       = "main"
    , maximumDepth = 50
    }

main :: IO ()
main = perform arguments "examples/Test.java"