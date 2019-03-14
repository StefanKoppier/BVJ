module Compilation.Phase(
      compilationPhase
) where

import Auxiliary.Phase
import Auxiliary.Pretty
import Linearization.Path
import Compilation.CProgram
import Compilation.Compiler.Path
import Parsing.Syntax
import Data.Accumulator

compilationPhase :: Phase (CompilationUnit', ProgramPaths) CPrograms
compilationPhase Arguments{verbosity} (unit, paths) = do
    newEitherT $ printInformation verbosity paths
    return $ map (\ path -> snd $ runAccumulator (translatePath unit path) (0, [])) paths

printInformation :: Verbosity -> ProgramPaths -> IO (Either PhaseError ())
printInformation verbosity paths = do
    printHeader "4. TRANSLATION"
    printText $ "Translating " ++ show (length paths) ++ " program paths"
    case verbosity of
        Informative 
            -> printPretty paths
        _
            -> return $ Right ()