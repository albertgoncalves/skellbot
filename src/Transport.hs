{-# LANGUAGE OverloadedStrings #-}

module Transport where

import Commands (parse)
import Control.Monad ((<=<))
import Data.Text (length, pack, unpack)
import Prelude hiding (length)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)
import Types
    ( Message(messageChannel, messageText, messageUser)
    , Response(Websocket)
    , message
    )

extract :: String -> Maybe Message
extract x =
    case concat (matchRegex regex x) of
        [i, t, u, c] -> Just (message i t u c)
        _ -> Nothing
  where
    regex =
        mkRegex
            ".*\"client_msg_id\":\"([^\"]*)\
            \.*\"text\":\"([^\"]*)\"\
            \.*\"user\":\"([^\"]*)\"\
            \.*\"channel\":\"([^\"]*)\".*"

inject :: Int -> String -> String -> String
inject =
    printf
        "{\"id\":%d\
        \,\"type\":\"message\"\
        \,\"channel\":\"%s\"\
        \,\"text\":\"%s\"}"

validate :: String -> Maybe String
validate = fmap concat . matchRegex (mkRegex "([^{};\\\"]*)")

relay :: String -> Int -> Message -> Maybe Response
relay botId i m
    | botId == messageUser m = Nothing
    | otherwise = (f <=< parse . pack <=< validate . messageText) m
  where
    f x
        | length x < 1 || length x > 1000 = Nothing
        | otherwise =
            (Just . Websocket . inject i (messageChannel m) . unpack) x
