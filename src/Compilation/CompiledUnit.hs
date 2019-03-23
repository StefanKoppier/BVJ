module Compilation.CompiledUnit where

import Parsing.Syntax

type CompiledUnit  = (CompilationUnit', FilePath)

type CompiledUnits = [CompiledUnit]
