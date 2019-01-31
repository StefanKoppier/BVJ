module Parsing.Phase where

import Language.Java.Parser
import Language.Java.Syntax

import Control.Phase
import Control.Verbosity

parsingPhase :: Phase String CompilationUnit
parsingPhase verbosity content = do
    newEitherT $ printHeader "1. PARSING"
    newEitherT $ printTitled "Input program" content
    case parser compilationUnit content of 
        Left  e -> left $ ParseError (show e)
        Right r -> return r