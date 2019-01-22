module Main where

import Parsing.Parser
import Parsing.Pretty

import Analysis.Flow
import Analysis.Complete

perform :: FilePath -> IO ()
perform file = do
    content <- readFile file
    let result = parse content
    case result of
        Left e  -> print e
        Right p -> do 
            putStrLn "Program to be verified: "
            putStrLn $ show p

            putStrLn "\nNodes of Program:"
            let block = transformBlock $ getMainMethod  p
            print $ nodesOfBlock block
            
            putStrLn "\nFlow of Program:"
            let block = transformBlock $ getMainMethod  p
            let flow  = flowOfBlock block
            print flow

getMainMethod :: CompilationUnit -> Block
getMainMethod (CompilationUnit _ _ 
                (ClassTypeDecl 
                  (ClassDecl _ _ _ _ _ 
                    (ClassBody [MemberDecl (MethodDecl _ _ _ _ _ _ _ 
                      (MethodBody (Just block)))])) : _))
    = block

main :: IO ()
main = perform "examples/Test.java"