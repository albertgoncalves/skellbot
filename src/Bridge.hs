{-# OPTIONS_GHC -Wall #-}

module Bridge where

import Chat (inject, options)
import Text.Regex (matchRegex, mkRegex)
import Types (Message(channel, text, user), message)

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

validate :: String -> [String]
validate x = f y ++ ys
  where
    f = concat . matchRegex (mkRegex "!([a-z]+)")
    y:ys = words x

relay :: String -> Int -> Message -> Maybe String
relay botId i m
    | botId == user m = Nothing
    | otherwise =
        case validate (text m) of
            ("hello":_) -> f "Hello!"
            ("echo":xs) -> (f . unwords) xs
            ("help":_) -> f options
            _ -> Nothing
  where
    f = Just . inject i (channel m)
