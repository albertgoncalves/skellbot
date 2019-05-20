{-# OPTIONS_GHC -Wall #-}

module Chat where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.List (isInfixOf)
import Data.Text (pack, splitOn, strip, unpack)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)
import Types
    ( Message(channel, text, user)
    , Response(None, POST, Websocket)
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

messageCapture :: (String -> String) -> String -> String
messageCapture f x
    | any (`isInfixOf` x) [":", "\\n"] = x
    | otherwise = f x

curlCheck :: String -> Bool
curlCheck = isInfixOf "[POST]"

curlCapture :: (String -> String) -> String -> String
curlCapture f x
    | curlCheck x = x
    | otherwise = f x

select :: String -> String -> String
select "hello" = const "Hello!"
select "bernar" = const ":stache:"
select "righton" = const ":righton:x:100:"
select "echo" = id
select "rev" = messageCapture reverse
select "upper" = messageCapture (map toUpper)
select "lower" = messageCapture (map toLower)
select "ban" = printf "%s has been *banned*." . filter (/= '.')
select "2019" = printf "%s in 2019." . filter (/= '.')
select "bold" = printf "*%s*" . filter (/= '*')
select "em" = printf "_%s_" . filter (/= '_')
select "help" = const options
select "post" = const "[POST] ..."
select _ = const ""

control :: String -> [String] -> String
control x [command] = curlCapture (select command) x
control _ (command:args) = (curlCapture (select command) . unwords) args
control x [] = x

foldControl :: String -> String
foldControl = foldl control "" . tokenize

relay :: String -> Int -> Message -> Response
relay botId i m
    | botId == user m = None
    | otherwise = (f . foldControl . text) m
  where
    f x
        | null x = None
        | curlCheck x = POST x
        | otherwise = Websocket $ inject i (channel m) x
