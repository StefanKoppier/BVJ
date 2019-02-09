module Control.Phase(
      module Control.Monad.Trans.Either
    , module Arguments
    , module Control.Verbosity
    , PhaseResult
    , PhaseError(..)
    , Phase
    , Subphase
    , printHeader
    , printTitled
    , printText
) where

import Text.PrettyPrint
import Control.Verbosity
import Arguments
import Control.Monad.Trans.Either

type PhaseResult a = EitherT PhaseError IO a

data PhaseError
    = ParseError        String
    | MethodNotFound    String
    | UnsupportedSyntax String
    | ResultParseError  String
    deriving (Show, Eq)

type Phase a b = Arguments -> a -> PhaseResult b

type Subphase a b = Phase a b

printHeader :: String -> IO (Either PhaseError ())
printHeader header = do
    let width   = 80
    let filling = width - (length header + 6)
    let doc     =   stars width
                $+$ text "** " <> text header <> spaces filling <> text  " **"
                $+$ stars width $+$ space
    print doc
    return $ Right ()

printTitled :: String -> String -> IO (Either PhaseError ())
printTitled title content = do
    putStrLn title
    putStrLn $ content ++ "\n"
    return $ Right ()

printText :: String -> IO (Either PhaseError ())
printText content = do
    putStrLn $ content ++ "\n"
    return $ Right ()

spaces :: Int -> Doc
spaces n = text $ replicate n ' '

stars :: Int -> Doc
stars n = text $ replicate n '*'