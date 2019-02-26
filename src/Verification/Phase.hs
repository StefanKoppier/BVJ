module Verification.Phase(
    verificationPhase
) where
    
import System.Process           (readProcessWithExitCode)
import Auxiliary.Pretty
import Control.Concurrent.Async
import System.Directory
import System.IO
import Auxiliary.Phase
import Translation.Phase
import Translation.Program
import Verification.Result

verificationPhase :: Phase Programs VerificationResults
verificationPhase args@Arguments{keepOutputFiles} programs = do
    newEitherT $ printHeader "5. VERIFICATION"
    newEitherT $ printPretty programs
    newEitherT createWorkingDir
    results <- newEitherT $ runAsync args programs
    if keepOutputFiles
        then return ()
        else newEitherT removeWorkingDir
    return results

runAsync :: Arguments -> Programs -> IO (Either PhaseError VerificationResults)
runAsync args programs = do
    processes <- mapM (async . verify args) programs
    results <- mapM wait processes
    return $ sequence results

verify :: Arguments -> Program -> IO (Either PhaseError VerificationResult)
verify args program = do
    (path, handle) <- openTempFileWithDefaultPermissions workingDir "main.c"
    let program' = "#include <stdlib.h>\n" ++ toString program
    hPutStr handle program'
    hClose handle
    (_,result,_) <- readProcessWithExitCode "./tools/cbmc/cbmc" (cbmcArgs path args) ""
    runEitherT $ parseOutput result

createWorkingDir :: IO (Either PhaseError ())
createWorkingDir = do 
    createDirectoryIfMissing False workingDir
    return $ Right ()

removeWorkingDir :: IO (Either PhaseError ())
removeWorkingDir = do 
    removeDirectoryRecursive workingDir
    return $ Right ()

workingDir :: FilePath
workingDir = "tmp_verification_folder"

cbmcArgs :: FilePath -> Arguments -> [String]
cbmcArgs path args
    =  [ path , "--xml-ui"
        -- TODO: find a nice way to set these include paths.
       , "-I", "C:\\MinGW\\lib\\gcc\\mingw32\\6.3.0\\include"
       , "-I", "C:\\MinGW\\include"
       ]
    ++ ["--no-assertions"         | not $ enableAssertions args  ]
    ++ ["--bounds-check"          | enableArrayBoundsCheck args  ]
    ++ ["--pointer-check"         | enablePointerChecks args     ]
    ++ ["-div-by-zero-check"      | enableDivByZeroCheck args    ]
    ++ ["--signed-overflow-check" | enableIntOverflowCheck args  ]
    ++ ["--undefined-shift-check" | enableShiftCheck args        ]
    ++ ["--float-overflow-check"  | enableFloatOverflowCheck args]
    ++ ["--nan-check"             | enableNaNCheck args          ]