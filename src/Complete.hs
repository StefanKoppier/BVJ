module Complete(
      module Auxiliary.Phase
    , module Auxiliary.Pretty
    , run
) where

import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Compilation.Phase
import Verification.Phase
import Verification.JBMCResult
import Verification.Pretty

run :: Arguments -> IO ()
run args@Arguments{program} = do
    putStrLn "Verification arguments: "
    printPretty args
    content <- readFile program
    result  <- runExceptT $ allPhases args content
    case result of
        Left  failure -> putStrLn $ "An error occurred: " ++ show failure
        Right _       -> return ()

allPhases :: Phase String CProverResults
allPhases args file = do
    ast      <- parsingPhase args file
    cfg      <- analysisPhase args ast
    paths    <- linearizationPhase args (ast, cfg)
    programs <- compilationPhase args (ast, paths)
    results  <- verificationPhase args programs
    liftIO $ printHeader "FINAL RESULT"
    liftIO $ printPretty results
    return results
    