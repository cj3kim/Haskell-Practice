{-# LANGUAGE ViewPatterns #-}
module LogAnalysis where
import Log



parseMessage :: String -> LogMessage
parseMessage (words -> (x : y : h@(z : xs))) = LogMessage messageType timestamp message
                                               where messageType = case x of
                                                                     "I" -> Info
                                                                     "W" -> Warning
                                                                     "E" -> (Error (read y :: Int))
                                                     timestamp   = case messageType of
                                                                     (Error n) -> read z :: Int
                                                                     otherwise -> read y :: Int
                                                     message     = case messageType of
                                                                     (Error n) -> unwords xs
                                                                     otherwise -> unwords h

{-parseMessage x = Unknown "'" ++ x ++ "'" ++ " is not the log we're looking for."-}


{-parseMessage "E 2 526 help help" == LogMessage (Error 2) 562 "help help"-}
{-parseMessage "I 29 la la la " == LogMessage Info 29 "la la la"-}
{-parseMessage "This is not in the right format" == Unknown "This is not in the right format"-}



{-parse :: String -> [LogMessage]-}
