{-|
Module      : Complete
Description : Module containing the interface to perform verification.
-}
module Complete(
      module Auxiliary.Phase
    , module Auxiliary.Pretty
    , run
) where

import Control.Monad
import System.Directory
import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Compilation.Phase
import Verification.Phase
import Verification.JBMCResult
import Verification.Pretty()

-- | Run the verification tool using the given arguments.
run :: Arguments -> IO ()
run args@Arguments{program} = do
    putStrLn "Verification arguments: "
    printPretty args
    content <- readFile program
    result  <- runExceptT $ allPhases args content
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right _       -> return ()

-- | Phase running all individual phases.
allPhases :: Phase String CProverResults
allPhases args@Arguments{removeOutputFiles} file = do
    ast      <- parsingPhase args file
    cfg      <- analysisPhase args ast
    paths    <- linearizationPhase args (ast, cfg)
    liftIO createWorkingDir
    programs <- compilationPhase args (ast, paths)
    results  <- verificationPhase args programs
    liftIO $ printHeader "FINAL RESULT"
    liftIO $ printPretty results
    when removeOutputFiles
        (liftIO removeWorkingDir)
    return results
    
-- | Removes the working directory of the verification tool.
removeWorkingDir :: IO ()
removeWorkingDir = removeDirectoryRecursive workingDir

-- | Creates the working directory of the verification tool.
createWorkingDir :: IO ()
createWorkingDir = do 
    createDirectoryIfMissing False workingDir
    return ()