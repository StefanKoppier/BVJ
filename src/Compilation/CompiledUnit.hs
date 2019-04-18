{-|
Module      : Compilation.CompiledUnit
Description : Module containing the data declarations of compiled units.
-}
module Compilation.CompiledUnit where

import Parsing.Syntax
import Data.Accumulator

-- | Data type representing a compiled unit, or filter compiled unit.
data CompiledUnit
    = CompiledUnit CompilationUnit' FilePath
    | FilteredUnit
    deriving (Show, Eq)

-- | Type declaration representing compiled units.
type CompiledUnits = [CompiledUnit]

-- | Accumulator containing the declared variables.
type MethodAccumulator a = Accumulator [VarDeclId'] a