{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Map.Strict (Map, fromList, keys)
import Data.Text (Text, pack, reverse, splitOn, strip, unpack, unwords, words)
import Prelude hiding (reverse, unwords, words)
import Text.Printf (printf)

newtype Command =
    Command (Text, Text)
    deriving (Eq, Show)

tokenize :: Text -> [[Text]]
tokenize = map (words . strip) . splitOn "|"

convert :: [Text] -> Maybe Command
convert [] = Nothing
convert (x:xs) = (Just . Command) (x, unwords xs)

whitelist :: Command -> Maybe Command
whitelist xs@(Command (x, _))
    | x `elem` keys' = Just xs
    | otherwise = Nothing
  where
    keys' = keys commands

commands :: Map Text (Text -> Text)
commands =
    fromList
        [ ("!ban", pack . printf "%s has been banned" . unpack)
        , ("!bernar", const ":stache:")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!hello", const "Hello!")
        ]

main :: IO ()
main = mapM_ (print . mapM (whitelist <=< convert) . tokenize) xs
  where
    xs =
        [ "!bernar | !ban | !flip | !echo foo bar baz"
        , "!bernar | !ban | !hello | !echo foo bar baz"
        , "!bernar | !ban | !hlelo | !echo foo bar baz"
        , "!bernar | !ban | !flip | | !echo foo bar baz"
        , "!bernar | !ban | !flip | !echo foo bar baz |"
        ]
