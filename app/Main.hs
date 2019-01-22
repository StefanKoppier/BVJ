module Main where

import Parsing.Parser
import Parsing.Pretty

import Analysis.Flow
import Analysis.Complete

perform :: FilePath -> IO ()
perform file = do
    content <- readFile file
    case parse content of
        Left e        -> print e
        Right program -> do 
            putStrLn "Program to be verified: "
            print program

            case transformBlock $ getMainMethod  program of 
                Left e      -> print e
                Right block -> do
                    putStrLn "\nNodes of Program:"
                    print $ nodesOfBlock block
                    
                    putStrLn "\nFlow of Program:"
                    let flow = flowOfBlock block
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