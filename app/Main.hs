module Main where

import Control.Phase
import Control.Verbosity
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase

perform :: Verbosity -> String -> FilePath -> IO ()
perform verbosity methodName file = do
    content     <- readFile file
    parseResult <- runEitherT $ parsingPhase verbosity content
    case parseResult of 
        Left  e       -> putStrLn $ "An error occurred: " ++ show e
        Right program -> do
            cfg <- runEitherT $ analysisPhase verbosity (methodName, program)
            case cfg of
                Left e  -> putStrLn $ "An error occurred: " ++ show e
                Right r -> do 
                    let start = 1
                    let n = 10
                    pPaths <- runEitherT $ linearizationPhase verbosity (start, r, n)
                    case pPaths of
                        Left e  -> putStrLn $ "An error occurred: " ++ show e
                        Right r -> putStrLn $ "Final result\n" ++ show r

main :: IO ()
main = perform Everything "main" "examples/Test.java"