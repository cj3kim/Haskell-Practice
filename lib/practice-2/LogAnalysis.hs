module LogAnalysis where
import Log


parseMessage :: String -> LogMessage
parseMessage s = LogMessage messageType timestamp message
                 where  wordAry       = words s
                        firstChar     = wordAry !! 0
                        secondChar    = wordAry !! 1
                        messageType   = case firstChar of
                                          "I" -> Info
                                          "W" -> Warning
                                          "E" -> (Error (read secondChar :: Int))
                        timestamp     = case messageType of
                                          (Error n) -> read (wordAry !! 2) :: Int
                                          (Info)    -> read (wordAry !! 1) :: Int
                                          (Warning) -> read (wordAry !! 1) :: Int
                        message       = case messageType of
                                          (Error n) -> unwords (drop 3 wordAry)
                                          (Info)    -> unwords (drop 2 wordAry)
                                          (Warning) -> unwords (drop 2 wordAry)


{-This works but I'm not satisfied with it. Try to refactor and use take and drop.-}

{-parseMessage "E 2 526 help help" == LogMessage (Error 2) 562 "help help"-}
{-parseMessage "I 29 la la la " == LogMessage Info 29 "la la la"-}
{-parseMessage "This is not in the right format" == Unknown "This is not in the right format"-}



{-parse :: String -> [LogMessage]-}
