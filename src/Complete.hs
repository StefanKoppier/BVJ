module Complete where

import Auxiliary.Phase
import Auxiliary.Pretty
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Translation.Phase
import Verification.Phase
import Verification.Result
import Verification.Pretty

allPhases :: Phase String VerificationResults
allPhases args file = do
    ast      <- parsingPhase args file
    cfg      <- analysisPhase args ast
    paths    <- linearizationPhase args (1, cfg)
    programs <- translationPhase args paths
    results  <- verificationPhase args programs
    newEitherT $ printHeader "FINAL RESULT"
    newEitherT $ printPretty results
    right results