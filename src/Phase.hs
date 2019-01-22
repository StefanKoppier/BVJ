module Phase where

type PhaseResult a = Either PhaseError a

data PhaseError
    = ParseError
    | SyntaxTransformation String
    deriving (Show, Eq)