module Verification.Phase(
    verificationPhase
) where
    
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent
import Control.Monad
import System.Command
import System.Directory
import System.FilePath.Posix
import System.IO
import Auxiliary.Phase
import Auxiliary.Pretty
import Verification.CBMCResult
import Parsing.Utility
import Compilation.CompiledUnit

import Debug.Trace

verificationPhase :: Phase CompiledUnits CProverResults
verificationPhase args@Arguments{keepOutputFiles,verbosity} programs = do
    liftIO $ printInformation verbosity
    let results = liftIO $ runAsync args programs
    unless keepOutputFiles
        (liftIO removeWorkingDir)
    ExceptT results
    
printInformation :: Verbosity -> IO ()
printInformation _ = printHeader "5. VERIFICATION"

runAsync :: Arguments -> CompiledUnits -> IO (Either PhaseError CProverResults)
runAsync args@Arguments{numberOfThreads} programs = do
    progress <- liftIO $ progressBar (length programs)
    let tasks = map (return . verify args progress) programs
    results <- withPool numberOfThreads (\ pool -> parallel pool tasks)
    results' <- mapM runExceptT results
    liftIO $ putStrLn ""
    return (sequence results')

verify :: Arguments -> ProgressBar -> CompiledUnit -> PhaseResult CProverResult
verify args progress (program, file) =
    case findMainClass program of 
        Just mainClass -> do
            result <- jbmc file mainClass args
            liftIO $ incProgress progress
            parseXML result
        Nothing -> return undefined

removeWorkingDir :: IO ()
removeWorkingDir = removeDirectoryRecursive workingDir
    
jbmc :: FilePath -> String -> Arguments -> PhaseResult String
jbmc file mainClass args = do
    (Exit _, Stdout result) <- liftIO $ command [] "jbmc" jbmcArgs 
    return result
    where
        dir      = dropFileName file
        jbmcArgs = [ file
                   , "--xml-ui"
                   , "--main-class", mainClass
                   ]
                   ++ ["--no-assertions" | not $ enableAssertions args]
                   ++ unwindArg (maximumUnwind args)

unwindArg :: Maybe Int -> [String]
unwindArg Nothing  = []
unwindArg (Just n) = ["--unwind", show n]