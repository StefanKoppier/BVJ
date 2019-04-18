{-|
Module      : Verification.Phase
Description : Module containing the verification phase.
-}
module Verification.Phase(
    verificationPhase
) where
    
import Control.Concurrent.ParallelIO.Local
import System.Command
import Auxiliary.Phase
import Auxiliary.Pretty
import Verification.JBMCResult
import Parsing.Utility
import Compilation.CompiledUnit

-- | Verifies the compiled units.
verificationPhase :: Phase CompiledUnits CProverResults
verificationPhase args@Arguments{verbosity} programs = do
    liftIO $ printInformation verbosity programs
    let results = liftIO $ runAsync args programs
    ExceptT results
    
-- | Prints information about the verification phase to the screen.
printInformation :: Verbosity -> CompiledUnits -> IO ()
printInformation _ programs = do
    printHeader "5. VERIFICATION"
    printText ("Verifying " ++ show (length programs) ++ " program(s).")

-- | Runs the verify function on the compiled units in parallel.
runAsync :: Arguments -> CompiledUnits -> IO (Either PhaseError CProverResults)
runAsync args@Arguments{numberOfThreads} programs = do
    progress <- progressBar (length programs)
    let tasks = map (verify args progress) programs
    results <- withPool numberOfThreads (\ pool -> parallel pool tasks)
    putStrLn ""
    return (sequence results)

-- | Verify the given compiled unit and updates the progress bar.
verify :: Arguments -> ProgressBar -> CompiledUnit -> IO (Either PhaseError CProverResult)
verify args@Arguments{function} progress (CompiledUnit program file) = do
    result <- case function of
                Just function'
                    -> jbmc file (Left function') args
                Nothing
                    -> case findMainClass program of 
                        Just mainClass -> jbmc file (Right mainClass) args
                        Nothing        -> return undefined
    incProgress progress
    runExceptT $ parseXML result
    
-- | Runs the command `jbmc`.
jbmc :: FilePath -> Either String String -> Arguments -> IO String
jbmc file target args = do
    (Stdout result, Exit _, Stderr _) <- command [] "jbmc" jbmcArgs
    return result
    where
        jbmcArgs = [ file
                   , "--xml-ui"
                   , "--classpath", "./lib/core-models.jar"
                   ]
                   ++ targetArg target
                   ++ ["--no-assertions" | not $ jbmcEnableAssertions args]
                   ++ depthArg  (jbmcDepth args)
                   ++ unwindArg (jbmcUnwind args)

-- | Create the target argument for jbmc.
targetArg :: Either String String -> [String]
targetArg (Right mainClass) = ["--main-class", mainClass]
targetArg (Left function)   = ["--function", function]

-- | Create the depth argument for jbmc.
depthArg :: Maybe Int -> [String]
depthArg Nothing  = []
depthArg (Just n) = ["--depth", show n]

-- | Create the unwind argument for jbmc.
unwindArg :: Maybe Int -> [String]
unwindArg Nothing  = []
unwindArg (Just n) = ["--unwind", show n]