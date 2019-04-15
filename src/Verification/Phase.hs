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

verificationPhase :: Phase CompiledUnits CProverResults
verificationPhase args@Arguments{verbosity} programs = do
    liftIO $ printInformation verbosity programs
    let results = liftIO $ runAsync args programs
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
verify args@Arguments{function} progress (program, file) = do
    result <- case function of
                Just function'
                    -> jbmc file (Left function') args
                Nothing
                    -> case findMainClass program of 
                        Just mainClass -> jbmc file (Right mainClass) args
                        Nothing        -> return undefined
    incProgress progress
    runExceptT $ parseXML result
    
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

targetArg :: Either String String -> [String]
targetArg (Right mainClass) = ["--main-class", mainClass]
targetArg (Left function)   = ["--function", function]

depthArg :: Maybe Int -> [String]
depthArg Nothing  = []
depthArg (Just n) = ["--depth", show n]

unwindArg :: Maybe Int -> [String]
unwindArg Nothing  = []
unwindArg (Just n) = ["--unwind", show n]