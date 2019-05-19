{-# OPTIONS_GHC -Wall #-}

module Chat where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.List (isInfixOf)
import Data.Text (pack, splitOn, strip, unpack)
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

splitOnFirst :: Eq a => a -> [a] -> ([a], [a])
splitOnFirst x = fmap (drop 1) . break (x ==)

sanitize :: String -> String
sanitize xs
    | "! " `isInfixOf` xs = ""
    | all (\x -> isAlphaNum x || (x `elem` "!,.*_ ")) xs = xs
    | otherwise = ""

validate :: String -> [String]
validate x =
    case (splitOnFirst ' ' . sanitize) x of
        ("", "") -> []
        (y, ys) -> f y ++ words ys
  where
    f = concat . matchRegex (mkRegex " *!([a-z0-9]+)")

tokenize :: String -> [[String]]
tokenize = f . map (validate . unpack . strip) . splitOn (pack "|") . pack
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
    \`!righton`\\n\
    \`!echo ...`\\n\
    \`!rev ...`\\n\
    \`!upper ...`\\n\
    \`!lower ...`\\n\
    \`!ban ...`\\n\
    \`!2019 ...`\\n\
    \`!bold ...`\\n\
    \`!em ...`\\n\
    \`!help`"

careful :: (String -> String) -> String -> String
careful f x
    | any (`isInfixOf` x) [":", "\\n"] = x
    | otherwise = f x

select :: String -> String -> String
select "hello" = const "Hello!"
select "bernar" = const ":stache:"
select "righton" = const ":righton:x:100:"
select "echo" = id
select "rev" = careful reverse
select "upper" = careful (map toUpper)
select "lower" = careful (map toLower)
select "ban" = printf "%s has been *banned*."
select "2019" = printf "%s in 2019." . filter (/= '.')
select "bold" = printf "*%s*" . filter (/= '*')
select "em" = printf "_%s_" . filter (/= '_')
select "help" = const options
select _ = const ""

control :: String -> [String] -> String
control x [command] = select command x
control _ (command:args) = (select command . unwords) args
control x [] = x

foldControl :: String -> String
foldControl = foldl control "" . tokenize

relay :: String -> Int -> Message -> Maybe String
relay botId i m
    | botId == user m = Nothing
    | otherwise = (f . foldControl . text) m
  where
    f x
        | null x = Nothing
        | otherwise = Just $ inject i (channel m) x
