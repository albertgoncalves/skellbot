{-# OPTIONS_GHC -Wall #-}

module Chat where

import Text.Printf (printf)

inject :: Int -> String -> String -> String
inject =
    printf
        "{\"id\":%d\
        \,\"type\":\"message\"\
        \,\"channel\":\"%s\"\
        \,\"text\":\"%s\"}"

options :: String
options =
    "!hello\n\
    \!echo ..."
