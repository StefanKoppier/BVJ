{-|
Module      : Auxiliary.Arguments
Description : Module containing the verification arguments of the tool.
-}
module Auxiliary.Arguments(
      module Parsing.Syntax
    , module Linearization.Path  
    , Arguments(..)
) where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax     
import Linearization.Path

-- | The arguments for the verification tool.
data Arguments = Arguments {
    -- | The path to the program to verify.
      program              :: FilePath 
    -- | The function to verify, default to `main` if it is Nothing.
    , function             :: Maybe String
    -- | The amount of information that is printed to the screen.
    , verbosity            :: Verbosity
    -- | The number of threads to use when compiling and verifying.
    , numberOfThreads      :: Int
    -- | The mark bool which states that the compiled files will be removed, if 
    -- set to true.
    , removeOutputFiles    :: Bool
    -- | The maximum depth to generate program paths for.
    , maximumDepth         :: Int
    -- | The program path filter function. When this function returns Nothing, 
    -- for a specific path, this path will not be verified.
    , pathFilter           :: CompilationUnit' -> Maybe CompilationUnit'
    -- | Enable assertions in JBMC.
    , jbmcEnableAssertions :: Bool
    -- | Sets the maximum depth in JBMC, if Just.
    , jbmcDepth            :: Maybe Int
    -- | Sets the maximum loop unwinding in JBMC, if Just.
    , jbmcUnwind           :: Maybe Int
  }

instance Pretty Arguments where
    pretty Arguments{program, function, numberOfThreads, maximumDepth}
        = "Verifying the program" <+> quotes (pretty program) <+> function'
        <+> "using" <+> pretty numberOfThreads <+> "threads" <> comma 
        <+> "and maximum depth" <+> pretty maximumDepth <> dot
        where
            function' = maybe empty (\ f -> "and function" <+> quotes (pretty f)) function 
