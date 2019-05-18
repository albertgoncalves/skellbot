{-# OPTIONS_GHC -Wall #-}

module Bridge where

import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

data Message =
    Message
        { messageId :: String
        , text :: String
        , user :: String
        , channel :: String
        }
    deriving (Eq, Show)

extract :: String -> Maybe Message
extract x =
    case concat (matchRegex regex x) of
        [m, t, u, c] ->
            Just Message {messageId = m, text = t, user = u, channel = c}
        _ -> Nothing
  where
    regex =
        mkRegex
            ".*\"client_msg_id\":\"([^\"]*)\
            \.*\"text\":\"([^\"]*)\"\
            \.*\"user\":\"([^\"]*)\"\
            \.*\"channel\":\"([^\"]*)\".*"

validate :: String -> [String]
validate = concat . matchRegex (mkRegex "!bot (.+)")

relay :: String -> Int -> Message -> Maybe String
relay botId i message
    | botId == user message = Nothing
    | otherwise =
        case validate (text message) of
            [x] -> Just $ printf returnString i (channel message) x
            _ -> Nothing
  where
    returnString =
        "{\"id\":%d,\"type\":\"message\",\"channel\":\"%s\",\"text\":\"%s\"}"

example :: String
example =
    "{\"client_msg_id\":\"...\"\
    \,\"suppress_notification\":false\
    \,\"type\":\"message\"\
    \,\"text\":\"!bot hello\"\
    \,\"user\":\"...\"\
    \,\"team\":\"...\"\
    \,\"channel\":\"...\"\
    \,\"event_ts\":\"...\"\
    \,\"ts\":\"...\"}"

main :: IO ()
main = maybe (return ()) print (relay "" 1 =<< extract example)
