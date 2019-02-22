module Verification.Result(
      VerificationResult(..)
    , VerificationFailure(..)
    , VerificationResults
    , parseOutput
) where
    
import Data.Maybe
import Auxiliary.Phase
import Data.List
import Data.ByteString.UTF8
import Xeno.DOM

data VerificationResult
    = Success
    | Failure [VerificationFailure]
    deriving (Show)

data VerificationFailure = VerificationFailure {
      property   :: ByteString
    , reason     :: ByteString
    } deriving (Show)

type VerificationResults = [VerificationResult]

parseOutput :: String -> PhaseResult VerificationResult
parseOutput output = do
    root   <- (parseRoot . fromString) output
    status <- getStatus root
    if status == "SUCCESS"
        then return Success
        else do
            results  <- getResults root
            traces   <- mapM getGotoTrace results
            failures <- mapM getFailures (catMaybes traces)
            Failure <$> mapM getFailure (concat failures)

parseRoot :: ByteString -> PhaseResult Node
parseRoot output =
    case parse output of
        Left  _ -> failure "parseRoot"
        Right r -> right r

getStatus :: Node -> PhaseResult ByteString
getStatus node = do
    status <- (try (show node) . find (hasName "cprover-status") . children) node
    case head $ contents status of
        Text s -> return s
        _      -> failure "getStatus"

getResults :: Node -> PhaseResult [Node]
getResults = return . filter (hasName "result") . children

getGotoTrace :: Node -> PhaseResult (Maybe Node)
getGotoTrace = return . find (hasName "goto_trace") . children

getFailures :: Node -> PhaseResult [Node]
getFailures = return . filter (hasName "failure") . children

getFailure :: Node -> PhaseResult VerificationFailure
getFailure node = do
    let nodeAttributes = attributes node
    property <- (try "getFailure" . lookup "property") nodeAttributes
    let reason = fromJust $ lookup "reason" nodeAttributes
    return $ VerificationFailure property reason

hasName :: ByteString -> Node -> Bool
hasName s = (s ==) . name

failure :: String -> PhaseResult a
failure = left . ResultParseError

try :: String -> Maybe a -> PhaseResult a
try f = maybe (failure f) return