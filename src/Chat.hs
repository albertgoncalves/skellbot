{-# OPTIONS_GHC -Wall #-}

module Chat where

import Data.Char (toLower, toUpper)
import Data.Text (pack, splitOn, unpack)
import Text.Printf (printf)
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

split :: String -> [[String]]
split = map (validate . unpack) . splitOn (pack "|") . pack

validate :: String -> [String]
validate x =
    case words x of
        [] -> []
        (y:ys) -> f y ++ ys
  where
    f = concat . matchRegex (mkRegex "!([a-z]+)")

inject :: Int -> String -> String -> String
inject =
    printf
        "{\"id\":%d\
        \,\"type\":\"message\"\
        \,\"channel\":\"%s\"\
        \,\"text\":\"%s\"}"

options :: String
options =
    "`!hello`\\n\
    \`!echo ...`\\n\
    \`!help`"

switch :: String -> [String] -> String
switch _ ["hello"] = "Hello!"
switch _ ["help"] = options
switch _ ("echo":xs) = unwords xs
switch x ["rev"] = reverse x
switch _ ("rev":xs) = (reverse . unwords) xs
switch x ["upper"] = map toUpper x
switch _ ("upper":xs) = (map toUpper . unwords) xs
switch x ["lower"] = map toLower x
switch _ ("lower":xs) = (map toLower . unwords) xs
switch x _ = x

relay :: String -> Int -> Message -> Maybe String
relay botId i m
    | botId == user m = Nothing
    | otherwise = (f . foldl switch mempty . split . text) m
  where
    f = Just . inject i (channel m)
