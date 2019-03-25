module Verification.Pretty where

import qualified Data.ByteString.UTF8 as UTF8
import           Text.PrettyPrint
import           Auxiliary.Pretty
import           Verification.JBMCResult

instance Pretty CProverResults where
    pretty results
        = text "Verified" <+> int (length results) <+> text "paths"
        <+> text "of which" <+> int nSuccesses <+> isAre nSuccesses <+> text "correct"
        <> comma <+> int nFailures <+> isAre nFailures <+> text "incorrect, and" 
        <+> int nErroneous <+> isAre nErroneous <+> text "erroneous." <> newline
        $+$ foldr (($+$) . pretty) empty results
        where
            nFailures = length failures
            failures   = [r | r@(CProverResult _ _ (Just Failure)) <- results]
            nSuccesses = length successes
            successes  = [r | r@(CProverResult _ _ (Just Success)) <- results]
            nErroneous = length erroneous
            erroneous  = [r | r@(CProverResult _ _ Nothing)        <- results]
            
instance Pretty CProverResult where
    pretty CProverResult{_results, _status, _messages}
        | (Just Success) <- _status 
            = empty
        | (Just Failure) <- _status
            = foldr (($+$) . pretty) empty _results
        | Nothing        <- _status
            = newline <> foldr (($+$) . pretty) empty errorMessages
        where
            errorMessages = [m | m@(JBMCMessage JBMCError _) <- _messages]

instance Pretty JBMCMessage where
    pretty JBMCMessage{_text} = pretty _text

instance Pretty JBMCResult where
    pretty JBMCResult{_resultFailures}
        = foldr (($+$) . pretty) empty _resultFailures

instance Pretty JBMCFailure where
    pretty JBMCFailure{_file,_function,_line,_property,_reason}
        = text "Failed property" <+> property <+> text "with reason" <+> reason
        $+$ tab (text "at file" <+> file <> comma 
        <+> text "function" <+> function <> comma
        <+> text "line" <+> line) <> newline
        where
            property = quotes (pretty _property)
            reason   = quotes (pretty _reason)
            file     = quotes (pretty _file)
            function = quotes (pretty _function)
            line     = quotes (pretty _line)

instance Pretty UTF8.ByteString where
    pretty = text . UTF8.toString

isAre :: Int -> Doc
isAre 1 = text "is"
isAre _ = text "are"