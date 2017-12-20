module LogAnalysis
    ( 
        parseMessage,
        insert
    ) where

import Text.Read
import Data.Maybe
import Log

-- exercise 1, could be done by pattern matching "I":ts:xs but read doesn't return Maybe, failed parsing returns error
parseTimeStamp :: String -> Maybe TimeStamp
parseTimeStamp ts = readMaybe ts :: Maybe TimeStamp

parseMessageWithTimestamp :: MessageType -> [String] -> LogMessage
parseMessageWithTimestamp mt args = 
    let maybeTimeStamp =  parseTimeStamp $ args !! 1 in
        if isJust maybeTimeStamp then LogMessage mt (fromJust maybeTimeStamp) (unwords $ drop 2 args) else Unknown (unwords args) 

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage args = 
    let maybeSeverity = readMaybe (args !! 1) :: Maybe Int 
        maybeTimeStamp = parseTimeStamp $ args !! 2 in
        if isJust maybeSeverity && isJust maybeTimeStamp then LogMessage (Error (fromJust maybeSeverity)) (fromJust maybeTimeStamp) (unwords $ drop 3 args)     
        else Unknown (unwords args)

parseMessage :: String -> LogMessage
parseMessage s = 
    let tokens = words s in
        if length tokens >= 2 then
            case head tokens of
                "I" -> parseMessageWithTimestamp Info (words s) 
                "W" -> parseMessageWithTimestamp Warning (words s)
                "E" -> if length tokens >= 3 then parseErrorMessage (words s) else Unknown s
                _ -> Unknown s
        else
            Unknown s

-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s) mt = mt
insert (LogMessage mty ts s) Leaf = Node Leaf (LogMessage mty ts s) Leaf   
insert (LogMessage mty ts s) (Node mtl (LogMessage mtyi tsi si) mtr) = 
    if ts <= tsi then Node (insert (LogMessage mty ts s) mtl) (LogMessage mtyi tsi si) mtr else
        Node mtl (LogMessage mtyi tsi si) (insert (LogMessage mty ts s) mtr)
insert _ mt = mt

