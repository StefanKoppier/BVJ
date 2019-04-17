module Compilation.CompiledUnit where

import Parsing.Syntax
import Data.Accumulator

data CompiledUnit
    = CompiledUnit CompilationUnit' FilePath
    | FilteredUnit
    deriving (Show, Eq)

type CompiledUnits = [CompiledUnit]

type MethodAccumulator a = Accumulator [VarDeclId'] a