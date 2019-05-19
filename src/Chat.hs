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

validate :: String -> [String]
validate x =
    case words x of
        [] -> []
        (y:ys) -> f y ++ ys
  where
    f = concat . matchRegex (mkRegex "!([a-zA-Z]+)")

tokenize :: String -> [[String]]
tokenize = f . map (validate . unpack) . splitOn (pack "|") . pack
  where
    f xs
        | [] `elem` xs = []
        | otherwise = xs

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
    \`!rev ...`\\n\
    \`!upper ...`\\n\
    \`!lower ...`\\n\
    \`!help`"

select :: String -> String -> String
select "hello" = const "Hello!"
select "echo" = id
select "rev" = reverse
select "upper" = map toUpper
select "lower" = map toLower
select "help" = const options
select _ = const ""

control :: String -> [String] -> String
control x [command] = select (map toLower command) x
control _ (command:args) = (select (map toLower command) . unwords) args
control x [] = x

foldCommands :: String -> String
foldCommands = foldl control "" . tokenize

relay :: String -> Int -> Message -> Maybe String
relay botId i m
    | botId == user m = Nothing
    | otherwise = (f . foldCommands . text) m
  where
    f x
        | null x = Nothing
        | otherwise = Just $ inject i (channel m) x
