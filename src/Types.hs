module Types where

import Data.Text (Text)

newtype Command =
    Command (Text, Text)
    deriving (Eq, Show)

data Message =
    Message
        { messageId :: String
        , messageText :: String
        , messageUser :: String
        , messageChannel :: String
        }
    deriving (Eq, Show)

message :: String -> String -> String -> String -> Message
message i t u c =
    Message
        {messageId = i, messageText = t, messageUser = u, messageChannel = c}

data Response
    = Websocket String
    | POST String
    deriving (Eq, Show)
