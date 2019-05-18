{-# OPTIONS_GHC -Wall #-}

module Chat where

import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

data Message =
    Message
        { messageId :: String
        , text :: String
        , user :: String
        , channel :: String
        }
    deriving (Show)

extract :: String -> Maybe Message
extract x =
    case concat (matchRegex p x) of
        [a, b, c, d] ->
            Just Message {messageId = a, text = b, user = c, channel = d}
        _ -> Nothing
  where
    p =
        mkRegex
            ".*\"client_msg_id\":\"([^\"]*)\
            \.*\"text\":\"([^\"]*)\"\
            \.*\"user\":\"([^\"]*)\"\
            \.*\"channel\":\"([^\"]*)\".*"

validate :: String -> [String]
validate = concat . matchRegex (mkRegex "!bot ([a-zA-Z0-9]+)")

relay :: String -> Int -> Message -> Maybe String
relay u i m
    | u == user m = Nothing
    | otherwise =
        case validate (text m) of
            [x] -> Just $ printf s i (channel m) x
            _ -> Nothing
  where
    s =
        "{\"id\": %d\
        \, \"type\": \"message\"\
        \, \"channel\": \"%s\"\
        \, \"text\": \"%s\"}"

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
