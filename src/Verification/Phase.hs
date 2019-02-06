module Verification.Phase(
    verificationPhase
) where
    
import System.Directory
import System.IO
import System.Process (readProcessWithExitCode)
import Data.List
import Control.Phase
import Translation.Phase
import Verification.Result

verificationPhase :: Phase Programs VerificationResults
verificationPhase verbosity programs = do
    newEitherT $ printHeader "3. VERIFICATION"
    newEitherT $ printTitled "Input Programs" $ intercalate "\n" programs
    results <- mapM (newEitherT . verify) programs
    newEitherT removeWorkingDir
    return results

verify :: Program -> IO (Either PhaseError VerificationResult)
verify program = do
    createDirectoryIfMissing False workingDir
    putStrLn $ "\n\n" ++ program ++ "\n\n"
    (path, handle) <- openTempFileWithDefaultPermissions workingDir "main.c"
    hPutStr handle program
    hClose handle
    (_,result,_) <- readProcessWithExitCode "cbmc" (cbmcArgc path) program
    runEitherT $ parseOutput result

removeWorkingDir :: IO (Either PhaseError ())
removeWorkingDir = do 
    removeDirectoryRecursive workingDir
    return $ Right ()

workingDir :: FilePath
workingDir = "tmp_verification_folder"

cbmcArgc :: FilePath -> [String]
cbmcArgc path = [ path
                , "--xml-ui" -- Print result in XML
                -- , "--trace"  -- Give the counter example 
                ]