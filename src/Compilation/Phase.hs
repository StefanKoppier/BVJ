module Compilation.Phase(
      compilationPhase
) where

import Auxiliary.Phase
import Auxiliary.Pretty
import Linearization.Path
import Compilation.CProgram
import Compilation.Compiler.Path
import Parsing.Syntax

compilationPhase :: Phase (CompilationUnit', ProgramPaths) CPrograms
compilationPhase Arguments{verbosity} (unit, paths) = do
    newEitherT $ printInformation verbosity paths
    return $ map (translatePath unit) paths

printInformation :: Verbosity -> ProgramPaths -> IO (Either PhaseError ())
printInformation verbosity paths = do
    printHeader "4. TRANSLATION"
    printText $ "Generated " ++ show (length paths) ++ " program paths"
    case verbosity of
        Informative 
            -> printPretty paths
        _
            -> return $ Right ()