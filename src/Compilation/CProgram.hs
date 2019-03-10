module Compilation.CProgram where

import Language.C.Syntax.AST

type CProgram = CTranslUnit

type CPrograms = [CProgram]

-- | Local information containing the current class and the local variables declared.
type LocalInformation = (String, [String])

{-
data Accumulator = Accumulator LocalInformation [CExtDecl]

type TranslationResult a = (Accumulator, a)

original :: Accumulator -> a -> TranslationResult a
original acc x = (acc, x)

merges :: [Accumulator] -> Accumulator
merges = foldl merge (Accumulator [] [])

merge :: Acummulator -> Accumulator -> Accumulator
merge (Accumulator xInfo xDecls) (Accumulator yInfo yDecls) 
    = Accumulator (xInfo ++ yInfo) (xDecls ++ yDecls)

accs :: [TranslationResult a] -> [Accumulator]
accs

values :: [TranslationResult a] -> [a]
values = map snd

value :: TranslationResult a -> a
value (_, a) = a
-}