module LogAnalysis where
import Log

parseMessageType :: String -> MessageType
parseMessageType xs =  case messageType of 
                      "I" -> Info
                      "W" -> Warning
                      "E" -> (Error errorNumber)
                      where wordAry = words xs 
                            messageType = wordAry !! 0
                            errorNumber = read (wordAry !! 1) :: Int


parseMessage :: String -> LogMessage
parseMessage xs =  LogMessage messageType timestamp message
                   where wordsAry    = words xs
                         messageType = parseMessageType xs
                         timestamp   = case messageType of
                                         (Error n)   -> read (wordsAry !! 2) :: Int
                                         (Info)      -> read (wordsAry !! 1) :: Int
                                         (Warning)   -> read (wordsAry !! 1) :: Int
                         message     = case messageType of
                                         (Error n)    -> wordsAry !! 3
                                         (Info)       -> wordsAry !! 2
                                         (Warning)    -> wordsAry !! 2

{-parseMessage "E 2 526 help help" == LogMessage (Error 2) 562 "help help"-}
{-parseMessage "I 29 la la la " == LogMessage Info 29 "la la la"-}
{-parseMessage "This is not in the right format" == Unknown "This is not in the right format"-}



{-parse :: String -> [LogMessage]-}
