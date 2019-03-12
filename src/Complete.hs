module Complete where

import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Compilation.Phase
import Verification.Phase
import Verification.CBMCResult
import Verification.Pretty

allPhases :: Phase String CProverResults
allPhases args file = do
    ast      <- parsingPhase args file
    cfg      <- analysisPhase args ast
    paths    <- linearizationPhase args cfg
    programs <- compilationPhase args (ast, paths)
    results  <- verificationPhase args programs
    newEitherT $ printHeader "FINAL RESULT"
    newEitherT $ printPretty results
    return results