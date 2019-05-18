{-# OPTIONS_GHC -Wall #-}

module Chat where

import Text.Printf (printf)

echo :: Int -> String -> String -> String
echo =
    printf
        "{\"id\":%d\
        \,\"type\":\"message\"\
        \,\"channel\":\"%s\"\
        \,\"text\":\"%s\"}"
