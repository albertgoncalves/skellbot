{-# LANGUAGE OverloadedStrings #-}

module Transport where

import Commands (transform)
import Control.Monad ((<=<))
import Data.Text (length, pack, unpack)
import Prelude hiding (length)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)
import Types
    ( Message(messageChannel, messageText, messageUser)
    , Response(POST, Websocket)
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

feed :: Int -> String -> Response -> Maybe Response
feed i c (Websocket x)
    | length x < 1 || length x > 1000 = Nothing
    | otherwise = (Just . Websocket . pack . inject i c . unpack) x
feed _ _ (POST _) = Nothing -- awaiting implementation!

relay :: String -> Int -> Message -> Maybe Response
relay botId i m
    | botId == messageUser m = Nothing
    | otherwise =
        (feed i (messageChannel m) <=<
         transform . pack <=< validate . messageText)
            m
