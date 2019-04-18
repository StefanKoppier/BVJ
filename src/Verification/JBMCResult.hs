{-|
Module      : Verification.JBMCResult
Description : Module containing the definition and parsing of the JBMC output.
-}
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

-- | Type definition of multiple JBMC results.
type CProverResults = [CProverResult]

-- | Type definition of a JBMC result.
data CProverResult = CProverResult {
      _messages :: [JBMCMessage]
    , _results  :: [JBMCResult]
    , _status   :: Maybe CProverStatus
} deriving (Show)

-- | Type definition of a result status of JBMC.
data CProverStatus = Success | Failure 
                   deriving (Show)

-- | Type definition of a message of JBMC.
data JBMCMessage = JBMCMessage {
      _type :: JBMCMessageType
    , _text :: ByteString
} deriving (Show)

-- | Type definition of the type of a message of JBMC.
data JBMCMessageType
    = JBMCStatusMessage
    | JBMCWarning
    | JBMCError 
    deriving (Show)

-- | Type definition of the assertion results of JBMC.
data JBMCResult = JBMCResult {
      _resultProperty :: ByteString
    , _resultFailures :: [JBMCFailure]
} deriving (Show)

-- | Type definition of an assertion failure of JBMC.
data JBMCFailure = JBMCFailure {
      _file     :: ByteString
    , _function :: ByteString
    , _line     :: ByteString
    , _property :: ByteString
    , _reason   :: ByteString
} deriving (Show)

-- | Parses the given JBMC XML string.
parseXML :: String -> PhaseResult CProverResult
parseXML input 
    = case parse (fromString input) of
        Left  _    -> throwParsingError "Failed to parse JBMC output."
        Right node -> return $ pCProverResult node

-- | Parses the given JBMC CProver result.
pCProverResult :: Node -> CProverResult
pCProverResult node 
    = let items     = children node
          messages  = (map pJBMCMessage . filter (withName "message")) items
          results   = (map pJBMCResult  . filter (withName "result")) items
          status    = (pCProverStatus . find (withName "cprover-status")) items
       in CProverResult { _messages = messages
                        , _results  = results
                        , _status   = status }   

-- | Parses the given JBMC message.
pJBMCMessage :: Node -> JBMCMessage
pJBMCMessage node
    = let ty       = fromJust $ lookup "type" (attributes node)
          textNode = fromJust $ find (withName "text") (children node)
          text     = textFromNode textNode
       in JBMCMessage { _type = messageTypeFromString ty
                      , _text = text }

-- | Parses the given JBMC result.
pJBMCResult :: Node -> JBMCResult
pJBMCResult node
    = let traces   = filter (withName "goto_trace") (children node)
          property = fromJust $ lookup "property" (attributes node)
          failures = (map pJBMCFailure . filter (withName "failure")) (concatMap children traces)
       in JBMCResult { _resultProperty = property
                     , _resultFailures = failures }

-- | Parses the given JBMC failure.
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

-- | Parses the given JBMC CProver status.
pCProverStatus :: Maybe Node -> Maybe CProverStatus
pCProverStatus Nothing     
    = Nothing
pCProverStatus (Just node) 
    = case textFromNode node of
        "FAILURE" -> Just Failure
        "SUCCESS" -> Just Success

-- | Returns the string content of an XML node.
textFromNode :: Node -> ByteString
textFromNode node
    = case head $ contents node of
        (Text s) -> s

-- | Maps the string to its corresponding JBMC message type.
messageTypeFromString :: ByteString -> JBMCMessageType
messageTypeFromString string 
    = case string of
         "STATUS-MESSAGE" -> JBMCStatusMessage
         "ERROR"          -> JBMCError
         "WARNING"        -> JBMCWarning

-- | Returns true if the name of the node equals the given name.
withName :: ByteString -> Node -> Bool
withName name' node = name node == name'