module Verification.CBMCResult(
      parseXML
    , CProverResults  
    , CProverResult(..)
    , CBMCMessage(..)
    , CProverStatus(..)
    , CBMCMessageType(..)
    , CBMCResult(..)
    , CBMCFailure(..)
) where

import Data.ByteString.UTF8
import Xeno.DOM
import Data.List
import Data.Maybe
import Auxiliary.Phase

type CProverResults = [CProverResult]

data CProverResult = CProverResult {
      _messages :: [CBMCMessage]
    , _results  :: [CBMCResult]
    , _status   :: Maybe CProverStatus
} deriving (Show)

data CProverStatus = Success | Failure 
                   deriving (Show)

data CBMCMessage = CBMCMessage {
      _type :: CBMCMessageType
    , _text :: ByteString
} deriving (Show)

data CBMCMessageType
    = CBMCStatusMessage
    | CBMCWarning
    | CBMCError 
    deriving (Show)

data CBMCResult = CBMCResult {
      _resultProperty :: ByteString
    , _resultFailures :: [CBMCFailure]
} deriving (Show)

data CBMCFailure = CBMCFailure {
      _file     :: ByteString
    , _function :: ByteString
    , _line     :: ByteString
    , _property :: ByteString
    , _reason   :: ByteString
} deriving (Show)

parseXML :: String -> PhaseResult CProverResult
parseXML input 
    = case parse (fromString input) of
        Left  _    -> parsingError "Failed to parse CBMC output."
        Right node -> return $ pCProverResult node

pCProverResult :: Node -> CProverResult
pCProverResult node 
    = let items     = children node
          messages  = (map pCBMCMessage . filter (withName "message")) items
          results   = (map pCBMCResult  . filter (withName "result")) items
          status    = (pCProverStatus . find (withName "cprover-status")) items
       in CProverResult { _messages = messages
                        , _results  = results
                        , _status   = status }   

pCBMCMessage :: Node -> CBMCMessage
pCBMCMessage node
    = let ty       = fromJust $ lookup "type" (attributes node)
          textNode = fromJust $ find (withName "text") (children node)
          text     = textFromNode textNode
       in CBMCMessage { _type = messageTypeFromString ty
                      , _text = text }

pCBMCResult :: Node -> CBMCResult
pCBMCResult node
    = let traces   = filter (withName "goto_trace") (children node)
          property = fromJust $ lookup "property" (attributes node)
          failures = (map pCBMCFailure . filter (withName "failure")) (concatMap children traces)
       in CBMCResult { _resultProperty = property
                     , _resultFailures = failures }

pCBMCFailure :: Node -> CBMCFailure
pCBMCFailure node
    = let location     = fromJust $ find (withName "location") (children node)
          locationAttr = attributes location
          nodeAttr     = attributes node
          file         = fromJust $ lookup "file" locationAttr
          function     = fromJust $ lookup "function" locationAttr
          line         = fromJust $ lookup "line" locationAttr
          property     = fromJust $ lookup "property" nodeAttr
          reason       = fromJust $ lookup "reason" nodeAttr
       in CBMCFailure { _file      = file
                      ,  _function = function
                      , _line      = line
                      , _property  = property
                      , _reason    = reason }

pCProverStatus :: Maybe Node -> Maybe CProverStatus
pCProverStatus Nothing     
    = Nothing
pCProverStatus (Just node) 
    = case textFromNode node of
        "FAILURE" -> Just Failure
        "SUCCESS" -> Just Success

textFromNode :: Node -> ByteString
textFromNode node
    = case head $ contents node of
        (Text s) -> s

messageTypeFromString :: ByteString -> CBMCMessageType
messageTypeFromString string 
    = case string of
         "STATUS-MESSAGE" -> CBMCStatusMessage
         "ERROR"          -> CBMCError
         "WARNING"        -> CBMCWarning

withName :: ByteString -> Node -> Bool
withName name' node = name node == name'