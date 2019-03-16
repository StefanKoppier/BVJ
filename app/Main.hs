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
      method          = Scope Nothing "Main" "main"
    , verbosity       = Informative {-Compact-}
    , numberOfThreads = 4
    , maximumDepth    = 15
    , maximumUnwind   = Nothing
    , keepOutputFiles = True
    , includePaths    = -- For PC:
                        [ "C:\\MinGW\\lib\\gcc\\mingw32\\6.3.0\\include"
                        , "C:\\MinGW\\include" ]
    }

main :: IO ()
main = perform arguments "examples/Test.java"