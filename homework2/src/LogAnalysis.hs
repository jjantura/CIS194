module LogAnalysis
    ( 
        parseMessage,
        parse,
        insert,
        build,
        inOrder,
        whatWentWrong
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

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert (LogMessage messageType timeStamp string) Leaf = Node Leaf (LogMessage messageType timeStamp string) Leaf   
insert (LogMessage messageType timeStamp string) (Node left (LogMessage mTy tS sT) right) = 
    if timeStamp <= tS then Node (insert (LogMessage messageType timeStamp string) left) (LogMessage mTy tS sT) right else
        Node left (LogMessage mTy tS sT) (insert (LogMessage messageType timeStamp string) right)
insert _ mt = mt

-- exercise 3
build :: [LogMessage] -> MessageTree
build logMessages = foldl (\a e -> insert e a) Leaf logMessages

-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node _ (Unknown _) _) = []
inOrder (Node left (LogMessage messageType timeStamp string) right) = inOrder left ++ [LogMessage messageType timeStamp string] ++ inOrder right  


isSeverityGreaterThan :: Int -> LogMessage -> Bool
isSeverityGreaterThan n (LogMessage (Error s) _ _) = s > n
isSeverityGreaterThan _ (LogMessage _ _ _ ) = False
isSeverityGreaterThan _ (Unknown _) = False

getMessageString :: LogMessage -> String
getMessageString (LogMessage _ _ s) = s
getMessageString (Unknown s) = s

-- exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = map getMessageString $ filter (isSeverityGreaterThan 50) lm
