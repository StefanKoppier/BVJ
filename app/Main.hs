module Main where

import Control.Phase
import Control.Verbosity
import Parsing.Phase
import Analysis.Phase
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
                    printTitled "Final result" (prettify r)
                    return ()

main :: IO ()
main = perform Everything "examples/Test.java"