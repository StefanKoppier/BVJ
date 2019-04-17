module Auxiliary.Pretty(
      Verbosity(..)  
    , Pretty(..)
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
    , ProgressBar
    , progressBar
    , incProgress
) where

import           Text.PrettyPrint
import qualified System.ProgressBar as PB

--------------------------------------------------------------------------------
-- Verbosity
--------------------------------------------------------------------------------

data Verbosity
    = Compact
    | Informative
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------

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

--printHeader :: String -> IO (Either PhaseError ())
printHeader :: String -> IO ()
printHeader header = do
    let width   = 80
    let filling = width - (length header + 6)
    let doc     =   stars width
                $+$ text "** " <> text header <> spaces filling <> text  " **"
                $+$ stars width $+$ space
    print doc
    return ()

printPretty :: Pretty a => a -> IO ()
printPretty x = do
    putStrLn $ toString x ++ "\n"
    return ()

printTitled :: String -> String -> IO ()
printTitled title content = do
    putStrLn title
    putStrLn $ content ++ "\n"
    return ()

printText :: String -> IO ()
printText content = do
    putStrLn $ content ++ "\n"
    return ()

--------------------------------------------------------------------------------
-- Progress bar
--------------------------------------------------------------------------------

type ProgressBar = PB.ProgressBar ()

progressBar :: Int -> IO ProgressBar
progressBar count
    = let pbStyle = PB.defStyle{PB.styleWidth=PB.ConstantWidth 80} 
          initial = PB.Progress{PB.progressDone=0, PB.progressTodo=count, PB.progressCustom = ()}
       in PB.newProgressBar pbStyle 2 initial

incProgress :: ProgressBar -> IO ()
incProgress bar = PB.incProgress bar 1