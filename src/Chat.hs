{-# OPTIONS_GHC -Wall #-}

module Chat where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.List (isInfixOf)
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

sanitize :: String -> String
sanitize xs
    | all (\x -> isAlphaNum x || (x `elem` "! ")) xs = xs
    | otherwise = ""

validate :: String -> [String]
validate x =
    case words x of
        [] -> []
        (y:ys) -> f y ++ map sanitize ys
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
    \`!bernar`\\n\
    \`!echo ...`\\n\
    \`!rev ...`\\n\
    \`!upper ...`\\n\
    \`!lower ...`\\n\
    \`!help`"

careful :: (String -> String) -> String -> String
careful f x
    | any (`isInfixOf` x) [":", "\\n"] = x
    | otherwise = f x

select :: String -> String -> String
select "hello" = const "Hello!"
select "bernar" = const ":stache:"
select "echo" = id
select "rev" = careful reverse
select "upper" = careful (map toUpper)
select "lower" = careful (map toLower)
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
