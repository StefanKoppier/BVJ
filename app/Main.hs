module Main where

import Control.Phase
import Control.Verbosity
import Complete

perform :: Verbosity -> String -> FilePath -> Int -> IO ()
perform verbosity methodName file n = do
    content <- readFile file
    result  <- runEitherT $ allPhases verbosity (methodName, content, n)
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right results -> do
            putStrLn "Final result: \n"
            mapM_ print results

main :: IO ()
main = perform Everything "main" "examples/Test.java" 500