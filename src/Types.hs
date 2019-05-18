{-# OPTIONS_GHC -Wall #-}

module Types where

data Message =
    Message
        { messageId :: String
        , text :: String
        , user :: String
        , channel :: String
        }
    deriving (Eq, Show)

message :: String -> String -> String -> String -> Message
message i t u c = Message {messageId = i, text = t, user = u, channel = c}
