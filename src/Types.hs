module Types where

import Data.Text (Text)

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

data Command
    = Pipe (Text, Text)
    | Meta Text
    | Call Text
    deriving (Eq, Show)

data Response
    = Websocket Text
    | POST Text
    deriving (Eq, Show)
