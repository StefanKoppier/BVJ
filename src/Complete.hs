module Complete where

import Control.Phase
import Parsing.Phase
import Analysis.Phase
import Linearization.Phase
import Translation.Phase
import Verification.Phase
import Verification.Result

allPhases :: Phase (String, String, Int) VerificationResults
allPhases verbosity (methodName, file, n) = do
    ast      <- parsingPhase verbosity file
    cfg      <- analysisPhase verbosity (methodName, ast)
    paths    <- linearizationPhase verbosity (1, cfg, n)
    programs <- translationPhase verbosity paths
    verificationPhase verbosity programs