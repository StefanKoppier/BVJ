module Verification.Phase(
    verificationPhase
) where
    
import Language.C.Pretty
import Control.Concurrent.Async
import System.Directory
import System.IO
import System.Process (readProcessWithExitCode)
import Data.List
import Control.Phase
import Translation.Phase
import Verification.Result

verificationPhase :: Phase Programs VerificationResults
verificationPhase args programs = do
    newEitherT $ printHeader "3. VERIFICATION"
    newEitherT $ printTitled "Input Programs" $ intercalate "\n" (map (show . pretty) programs)
    results <- newEitherT $ runAsync args programs
    newEitherT removeWorkingDir
    return results

runAsync :: Arguments -> Programs -> IO (Either PhaseError VerificationResults)
runAsync args programs = do
    processes <- mapM (async . verify args) programs
    results <- mapM wait processes
    return $ sequence results

verify :: Arguments -> Program -> IO (Either PhaseError VerificationResult)
verify args program = do
    createDirectoryIfMissing False workingDir
    (path, handle) <- openTempFileWithDefaultPermissions workingDir "main.c"
    hPutStr handle (show $ pretty program)
    hClose handle
    (_,result,_) <- readProcessWithExitCode "cbmc" (cbmcArgs path args) ""
    runEitherT $ parseOutput result

removeWorkingDir :: IO (Either PhaseError ())
removeWorkingDir = do 
    removeDirectoryRecursive workingDir
    return $ Right ()

workingDir :: FilePath
workingDir = "tmp_verification_folder"

cbmcArgs :: FilePath -> Arguments -> [String]
cbmcArgs path args
    =  [path , "--xml-ui"]
    ++ ["--no-assertions"         | not $ enableAssertions args  ]
    ++ ["--bounds-check"          | enableArrayBoundsCheck args  ]
    ++ ["--pointer-check"         | enablePointerChecks args     ]
    ++ ["-div-by-zero-check"      | enableDivByZeroCheck args    ]
    ++ ["--signed-overflow-check" | enableIntOverflowCheck args  ]
    ++ ["--undefined-shift-check" | enableShiftCheck args        ]
    ++ ["--float-overflow-check"  | enableFloatOverflowCheck args]
    ++ ["--nan-check"             | enableNaNCheck args          ]