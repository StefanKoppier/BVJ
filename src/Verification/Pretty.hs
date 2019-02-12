module Verification.Pretty where

import qualified Data.ByteString.UTF8 as UTF8
import           Text.PrettyPrint
import           Auxiliary.Pretty
import           Verification.Result

instance Pretty VerificationResults where
    pretty rs = text "Verified" <+> int (length rs) <+> "paths"
              <+> text "of which" <+> int nSuccesses <+> isAre nSuccesses <+> text "correct"
              <+> text "and" <+> int nFailures <+> isAre nFailures <+> text "incorrect." $+$
              if null failures
                then empty
                else text "\n" <> text "Failures:" $+$ (vcat . map pretty) failures
              
              -- <+>
         {-     if null failures
                then empty
                else text "because" $+$ foldr ($+$) empty . map (nest 4 . prettyFailure) failures-}
              where
                    nSuccesses = length successes
                    successes = [s | s@Success     <- rs]
                    nFailures = length failures
                    failures  = [f | f@(Failure _) <- rs]
                    --prettyFailure VerificationFailure{property,reason} 
               --      = 

instance Pretty VerificationResult where
    pretty Success = text "success"
    pretty (Failure vs) = foldr (($+$) . nest 4 . pretty) empty vs

instance Pretty VerificationFailure where
    pretty VerificationFailure{property,reason} = text (UTF8.toString property) <+> text "with reason" <+> text (UTF8.toString reason)

isAre :: Int -> Doc
isAre 1 = text "is"
isAre _ = text "are"