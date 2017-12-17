module LogAnalysis
    ( 
        parseMessage 
    ) where

import Text.Read
import Data.Maybe
import Log

-- exercise 1, could be done by pattern matching "I":ts:xs but read doesn't return Maybe, failed parsing returns error
lineHeaders :: [String]
lineHeaders = ["I", "W", "E"]

parseMessageWithTimestamp :: MessageType -> [String] -> LogMessage
parseMessageWithTimestamp mt args = 
    let maybeTimeStamp = readMaybe (args !! 1) :: Maybe TimeStamp in
        if isJust maybeTimeStamp then LogMessage mt (fromJust maybeTimeStamp) (unwords $ drop 2 args) else Unknown (unwords args) 

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage args = 
    let maybeSeverity = readMaybe (head args) :: Maybe Int 
        maybeTimeStamp = readMaybe (args !! 1) :: Maybe TimeStamp in
        if isJust maybeSeverity && isJust maybeTimeStamp then LogMessage (Error (fromJust maybeSeverity)) (fromJust maybeTimeStamp) (unwords $ drop 2 args)     
        else Unknown (unwords args)

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage s = 
    let tokens = words s
        lineBody = tail tokens 
        lineHeader = head tokens in
        if length tokens >= 2 && elem lineHeader lineHeaders then
        case lineHeader of
            "I" -> parseMessageWithTimestamp Info (words s) 
            "W" -> parseMessageWithTimestamp Warning (words s)
            "E" -> if length tokens >= 3 then parseErrorMessage lineBody else Unknown (unwords lineBody)
            _ -> Unknown (unwords lineBody)
        else
            Unknown s

    
            

    