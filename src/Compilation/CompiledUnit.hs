module Compilation.CompiledUnit where

import Parsing.Syntax
import Data.Accumulator

type CompiledUnit  = (CompilationUnit', FilePath)

type CompiledUnits = [CompiledUnit]

type MethodAccumulator a = Accumulator [VarDeclId'] a