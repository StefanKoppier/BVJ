module Verification.Phase(
    verificationPhase
) where
    
import Control.Concurrent.ParallelIO.Local
import Control.Monad
import System.Command
import System.Directory
import Auxiliary.Phase
import Auxiliary.Pretty
import Verification.JBMCResult
import Parsing.Utility
import Compilation.CompiledUnit

verificationPhase :: Phase CompiledUnits CProverResults
verificationPhase args@Arguments{removeOutputFiles,verbosity} programs = do
    liftIO $ printInformation verbosity programs
    let results = liftIO $ runAsync args programs
    when removeOutputFiles
        (liftIO removeWorkingDir)
    ExceptT results
    
printInformation :: Verbosity -> CompiledUnits -> IO ()
printInformation _ compiledUnits = do
    printHeader "5. VERIFICATION"
    printText ("Verifying " ++ show (length compiledUnits) ++ " program(s).")

runAsync :: Arguments -> CompiledUnits -> IO (Either PhaseError CProverResults)
runAsync args@Arguments{numberOfThreads} programs = do
    progress <- progressBar (length programs)
    let tasks = map (verify args progress) programs
    results <- withPool numberOfThreads (\ pool -> parallel pool tasks)
    putStrLn ""
    return (sequence results)

verify :: Arguments -> ProgressBar -> CompiledUnit -> IO (Either PhaseError CProverResult)
verify args progress (program, file) =
    case findMainClass program of 
        Just mainClass -> do
            result <- jbmc file mainClass args
            incProgress progress
            runExceptT $ parseXML result
        Nothing -> return undefined

removeWorkingDir :: IO ()
removeWorkingDir = removeDirectoryRecursive workingDir
    
jbmc :: FilePath -> String -> Arguments -> IO String
jbmc file mainClass args = do
    (Stdout result, Exit _, Stderr _) <- command [] "jbmc" jbmcArgs
    return result
    where
        jbmcArgs = [ file
                   , "--xml-ui"
                   , "--classpath", "./lib/core-models.jar"
                   , "--main-class", mainClass
                   ]
                   ++ ["--no-assertions" | not $ jbmcEnableAssertions args]
                   ++ depthArg  (jbmcDepth args)
                   ++ unwindArg (jbmcUnwind args)

depthArg :: Maybe Int -> [String]
depthArg Nothing  = []
depthArg (Just n) = ["--depth", show n]

unwindArg :: Maybe Int -> [String]
unwindArg Nothing  = []
unwindArg (Just n) = ["--unwind", show n]