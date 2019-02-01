module Main where

import Control.Phase
import Control.Verbosity
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Analysis.Utility

perform :: Verbosity -> FilePath -> IO ()
perform verbosity file = do
    content     <- readFile file
    parseResult <- runEitherT $ parsingPhase verbosity content
    case parseResult of 
        Left  e       -> putStrLn $ "An error occurred: " ++ show e
        Right program -> do
            cfg <- runEitherT $ analysisPhase verbosity program
            case cfg of
                Left e  -> putStrLn $ "An error occurred: " ++ show e
                Right r -> do 
                    let start = 1
                    let n = 1
                    pPaths <- runEitherT $ linearizationPhase verbosity (start, r, n)
                    case pPaths of
                        Left e  -> putStrLn $ "An error occurred: " ++ show e
                        Right r -> putStrLn $ "Final result\n" ++ show r

main :: IO ()
main = perform Everything "examples/Test.java"