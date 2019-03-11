module Compilation.CProgram where

import Language.C.Syntax.AST

type CProgram = CTranslUnit

type CPrograms = [CProgram]

-- | Local information containing the current class and the local variables declared.
type LocalInformation = (String, [String])