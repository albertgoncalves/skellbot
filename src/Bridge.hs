{-# OPTIONS_GHC -Wall #-}

module Bridge where

import Chat (echo)
import Text.Regex (matchRegex, mkRegex)
import Types (Message(Message, channel, messageId, text, user))

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
            [x] -> Just $ echo i (channel message) x
            _ -> Nothing
