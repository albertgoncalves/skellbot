{-# OPTIONS_GHC -Wall #-}

module Chat where

import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex, Regex)

data Message =
    Message
        { messageId :: String
        , text :: String
        , user :: String
        , channel :: String
        } deriving (Show)

toMessage :: String -> String -> String -> String -> Message
toMessage a b c d =
    Message
        { messageId = a
        , text = b
        , user = c
        , channel = d
        }

re :: Regex
re =
    mkRegex
        ".*\"client_msg_id\":\"([^\"]*)\
        \.*\"text\":\"([^\"]*)\"\
        \.*\"user\":\"([^\"]*)\"\
        \.*\"channel\":\"([^\"]*)\".*"

extract :: String -> Maybe Message
extract x =
    case matchRegex re x of
        Just [a, b, c, d] -> Just $ toMessage a b c d
        _ -> Nothing

example :: String
example =
    "{\"client_msg_id\":\"09ce85c3-b339-4ca0-803a-86b7c89743c7\",\"suppress_notification\":false,\"type\":\"message\",\"text\":\"hello\",\"user\":\"UFZ4K692M\",\"team\":\"TFX228M0Q\",\"channel\":\"DGYRV6E5S\",\"event_ts\":\"1558057277.002000\",\"ts\":\"1558057277.002000\"}"

respond :: Message -> String
respond m = printf x i c t
  where
    x =
        "{\"id\": %d\
        \, \"type\": \"message\"\
        \, \"channel\": \"%s\"\
        \, \"text\": \"%s\"}"
    c = channel m
    t = text m
    i = 1 :: Int

main :: IO ()
main = maybe (return ()) print (respond <$> extract example)
