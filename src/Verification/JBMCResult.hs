module Verification.JBMCResult(
      parseXML
    , CProverResults  
    , CProverResult(..)
    , JBMCMessage(..)
    , CProverStatus(..)
    , JBMCMessageType(..)
    , JBMCResult(..)
    , JBMCFailure(..)
) where

import Data.ByteString.UTF8
import Xeno.DOM
import Data.List
import Data.Maybe
import Auxiliary.Phase

type CProverResults = [CProverResult]

data CProverResult = CProverResult {
      _messages :: [JBMCMessage]
    , _results  :: [JBMCResult]
    , _status   :: Maybe CProverStatus
} deriving (Show)

data CProverStatus = Success | Failure 
                   deriving (Show)

data JBMCMessage = JBMCMessage {
      _type :: JBMCMessageType
    , _text :: ByteString
} deriving (Show)

data JBMCMessageType
    = JBMCStatusMessage
    | JBMCWarning
    | JBMCError 
    deriving (Show)

data JBMCResult = JBMCResult {
      _resultProperty :: ByteString
    , _resultFailures :: [JBMCFailure]
} deriving (Show)

data JBMCFailure = JBMCFailure {
      _file     :: ByteString
    , _function :: ByteString
    , _line     :: ByteString
    , _property :: ByteString
    , _reason   :: ByteString
} deriving (Show)

parseXML :: String -> PhaseResult CProverResult
parseXML input 
    = case parse (fromString input) of
        Left  _    -> throwParsingError "Failed to parse JBMC output."
        Right node -> return $ pCProverResult node

pCProverResult :: Node -> CProverResult
pCProverResult node 
    = let items     = children node
          messages  = (map pJBMCMessage . filter (withName "message")) items
          results   = (map pJBMCResult  . filter (withName "result")) items
          status    = (pCProverStatus . find (withName "cprover-status")) items
       in CProverResult { _messages = messages
                        , _results  = results
                        , _status   = status }   

pJBMCMessage :: Node -> JBMCMessage
pJBMCMessage node
    = let ty       = fromJust $ lookup "type" (attributes node)
          textNode = fromJust $ find (withName "text") (children node)
          text     = textFromNode textNode
       in JBMCMessage { _type = messageTypeFromString ty
                      , _text = text }

pJBMCResult :: Node -> JBMCResult
pJBMCResult node
    = let traces   = filter (withName "goto_trace") (children node)
          property = fromJust $ lookup "property" (attributes node)
          failures = (map pJBMCFailure . filter (withName "failure")) (concatMap children traces)
       in JBMCResult { _resultProperty = property
                     , _resultFailures = failures }

pJBMCFailure :: Node -> JBMCFailure
pJBMCFailure node
    = let location     = fromJust $ find (withName "location") (children node)
          locationAttr = attributes location
          nodeAttr     = attributes node
          file         = fromJust $ lookup "file" locationAttr
          function     = fromJust $ lookup "function" locationAttr
          line         = fromJust $ lookup "line" locationAttr
          property     = fromJust $ lookup "property" nodeAttr
          reason       = fromJust $ lookup "reason" nodeAttr
       in JBMCFailure { _file      = file
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

messageTypeFromString :: ByteString -> JBMCMessageType
messageTypeFromString string 
    = case string of
         "STATUS-MESSAGE" -> JBMCStatusMessage
         "ERROR"          -> JBMCError
         "WARNING"        -> JBMCWarning

withName :: ByteString -> Node -> Bool
withName name' node = name node == name'