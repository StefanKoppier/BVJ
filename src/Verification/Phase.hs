module Verification.Phase(
    verificationPhase
) where
    
import System.Process           (readProcessWithExitCode)
import Auxiliary.Pretty
import Control.Concurrent.Async
import System.Directory
import System.IO
import Auxiliary.Phase
import Compilation.CProgram
import Verification.Result
import Compilation.Pretty

verificationPhase :: Phase CPrograms VerificationResults
verificationPhase args@Arguments{keepOutputFiles, verbosity} programs = do
    newEitherT $ printInformation verbosity programs
    newEitherT createWorkingDir
    results <- newEitherT $ runAsync args programs
    if keepOutputFiles
        then return ()
        else newEitherT removeWorkingDir
    return results
    
printInformation :: Verbosity -> CPrograms -> IO (Either PhaseError ())
printInformation verbosity programs = do
    printHeader "5. VERIFICATION"
    case verbosity of
        Informative 
            -> printPretty programs
        _
            -> return $ Right ()

runAsync :: Arguments -> CPrograms -> IO (Either PhaseError VerificationResults)
runAsync args programs = do
    processes <- mapM (async . verify args) programs
    results <- mapM wait processes
    return $ sequence results

verify :: Arguments -> CProgram -> IO (Either PhaseError VerificationResult)
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
    =  [path , "--xml-ui"]
    ++ includes (includePaths args)
    ++ ["--no-assertions"         | not $ enableAssertions args  ]
    ++ ["--bounds-check"          | enableArrayBoundsCheck args  ]
    ++ ["--pointer-check"         | enablePointerChecks args     ]
    ++ ["-div-by-zero-check"      | enableDivByZeroCheck args    ]
    ++ ["--signed-overflow-check" | enableIntOverflowCheck args  ]
    ++ ["--undefined-shift-check" | enableShiftCheck args        ]
    ++ ["--float-overflow-check"  | enableFloatOverflowCheck args]
    ++ ["--nan-check"             | enableNaNCheck args          ]

includes :: [FilePath] -> [String]
includes = concatMap (\ path -> ["-I", path])