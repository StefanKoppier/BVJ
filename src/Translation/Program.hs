module Translation.Program where

import Language.C.Syntax.AST
import Language.C.Pretty
    
type Program = CTranslUnit

type Programs = [Program]
