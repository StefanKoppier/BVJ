module Auxiliary.Arguments(
      module Parsing.Syntax
    , module Linearization.Path  
    , Arguments(..)
) where
    
import Text.PrettyPrint
import Auxiliary.Pretty
import Parsing.Syntax     
import Linearization.Path

data Arguments = Arguments {
      program              :: FilePath
    , function             :: Maybe String
    , verbosity            :: Verbosity
    , numberOfThreads      :: Int
    , removeOutputFiles    :: Bool
    , maximumDepth         :: Int
    , pathFilter           :: CompilationUnit' -> Maybe CompilationUnit'
    , jbmcEnableAssertions :: Bool
    , jbmcDepth            :: Maybe Int
    , jbmcUnwind           :: Maybe Int
  }

instance Pretty Arguments where
    pretty Arguments{program, function, numberOfThreads, maximumDepth}
        = "Verifying the program" <+> quotes (pretty program) <+> function'
        <+> "using" <+> pretty numberOfThreads <+> "threads" <> comma 
        <+> "and maximum depth" <+> pretty maximumDepth <> dot
        where
            function' = maybe empty (\ f -> "and function" <+> quotes (pretty f)) function 
