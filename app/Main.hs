module Main where

import Language.Java.Parser
import Language.Java.Pretty

open :: FilePath -> IO ()
open file = do
    content <- readFile file
    let ast = parser compilationUnit content
    case ast of
        Right p -> (putStrLn . prettyPrint) p
        Left _  -> putStrLn "Parsing error"

main :: IO ()
main = open "examples/Test.java"