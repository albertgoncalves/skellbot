{-# OPTIONS_GHC -Wall #-}

module Bridge where

import Chat (inject)
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
validate x = f y ++ ys
  where
    f = concat . matchRegex (mkRegex "!([a-z]+)")
    y:ys = words x

relay :: String -> Int -> Message -> Maybe String
relay botId i message
    | botId == user message = Nothing
    | otherwise =
        case validate (text message) of
            ("hello":_) -> f "Hello!"
            ("echo":xs) -> (f . unwords) xs
            _ -> Nothing
  where
    f = Just . inject i (channel message)
