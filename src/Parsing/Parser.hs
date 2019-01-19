module Parsing.Parser(
      module Language.Java.Syntax
    , parse
) where

import Language.Java.Parser
import Language.Java.Syntax

import Phase

parse :: String -> Either PhaseError CompilationUnit
parse content 
    = case parser compilationUnit content of
        Left _       -> Left ParseError
        Right result -> Right result