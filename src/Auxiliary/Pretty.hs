module Auxiliary.Pretty(
      Pretty(..)
    , spaces
    , printHeader
    , printPretty
    , printTitled
    , printText
) where

import Text.PrettyPrint
import Auxiliary.Phase

class Pretty a where
    pretty      :: a -> Doc
    toString    :: a -> String
    toString = render . pretty
    
spaces :: Int -> Doc
spaces n = text $ replicate n ' '

stars :: Int -> Doc
stars n = text $ replicate n '*'

printHeader :: String -> IO (Either PhaseError ())
printHeader header = do
    let width   = 80
    let filling = width - (length header + 6)
    let doc     =   stars width
                $+$ text "** " <> text header <> spaces filling <> text  " **"
                $+$ stars width $+$ space
    print doc
    return $ Right ()

printPretty :: Pretty a => a -> IO (Either PhaseError ())
printPretty x = do
    putStrLn $ toString x ++ "\n"
    return $ Right ()

printTitled :: String -> String -> IO (Either PhaseError ())
printTitled title content = do
    putStrLn title
    putStrLn $ content ++ "\n"
    return $ Right ()

printText :: String -> IO (Either PhaseError ())
printText content = do
    putStrLn $ content ++ "\n"
    return $ Right ()
