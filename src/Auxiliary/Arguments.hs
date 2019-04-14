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
    , verbosity            :: Verbosity
    , numberOfThreads      :: Int
    , removeOutputFiles    :: Bool
    , maximumDepth         :: Int
    , pathFilter           :: CompilationUnit' -> ProgramPaths -> ProgramPaths
    , jbmcEnableAssertions :: Bool
    , jbmcDepth            :: Maybe Int
    , jbmcUnwind           :: Maybe Int
  }

instance Pretty Arguments where
    pretty Arguments{program, numberOfThreads, maximumDepth}
        = "Program to be verified" <+> quotes (pretty program)
        <+> "using" <+> pretty numberOfThreads <+> "threads" <> comma
        <+> "and maximum depth" <+> pretty maximumDepth <> dot
