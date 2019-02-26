module Auxiliary.Pretty(
      Pretty(..)
    , spaces
    , newline
    , newlines
    , tab
    , dot
    , commas
    , dots
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

instance Pretty Doc where
    pretty = id

instance Pretty String where
    pretty = text

instance Pretty Int where
    pretty = int
    
spaces :: Int -> Doc
spaces n = text $ replicate n ' '

newline :: Doc
newline = char '\n'

newlines :: Doc -> Doc
newlines d = newline <> d <> newline

tab :: Doc -> Doc
tab = nest 4

dot :: Doc
dot = char '.'

commas :: Pretty a => [a] -> Doc
commas = hcat . punctuate comma . map pretty

dots :: Pretty a => [a] -> Doc
dots = hcat . punctuate dot . map pretty

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
