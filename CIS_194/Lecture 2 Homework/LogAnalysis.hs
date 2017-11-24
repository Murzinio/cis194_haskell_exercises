-- http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

isErrorMessage :: Char -> Bool
isErrorMessage x
    | x == 'E' = True
    | otherwise = False

messageTypeFromString :: String -> MessageType
messageTypeFromString [x,z]
    | x == 'I' = Info
    | x == 'W' = Warning
    | x == 'E' = Error (fromEnum(z))

parseMessage :: String -> LogMessage
parseMessage xs = 
    let filtered    = filter (/= ' ') xs
        messageType = messageTypeFromString([filtered !! 0, filtered !! 1])
        in
            if (isErrorMessage(head xs))
                then 
                    LogMessage messageType 22 xs
                else
                    LogMessage messageType 22 xs