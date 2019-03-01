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
    , maximumDepth    = 10
    , keepOutputFiles = False
    , includePaths    = -- For PC:
                        [ "C:\\MinGW\\lib\\gcc\\mingw32\\6.3.0\\include"
                        , "C:\\MinGW\\include"
                        ]
    }

main :: IO ()
main = perform arguments "examples/Test.java"