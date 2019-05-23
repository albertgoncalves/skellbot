{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((??))
import Control.Monad ((<=<), foldM, join)
import Data.Map.Strict (Map, fromList, keys, lookup)
import Data.Text
    ( Text
    , null
    , pack
    , reverse
    , splitOn
    , strip
    , unpack
    , unwords
    , words
    )
import Prelude hiding (lookup, null, reverse, unwords, words)
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

format :: String -> Text -> Text
format x = pack . printf x . unpack

commands :: Map Text (Text -> Text)
commands =
    fromList
        [ ("!ban", format "%s has been banned")
        , ("!bernar", const ":stache:")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!hello", const "Hello!")
        ]

combine :: Text -> Command -> Maybe Text
combine x (Command (y, ys)) =
    if null ys
        then f ?? x
        else f ?? ys
  where
    f = lookup y commands

main :: IO ()
main = mapM_ f xs
  where
    f =
        print .
        join .
        mapM (foldM combine "") . mapM (whitelist <=< convert) . tokenize
    xs =
        [ "!bernar | !ban"
        , "!bernar | !ban | !flip"
        , "!bernar | !ban | !flip | !echo foo bar baz"
        , "!bernar | !ban | !flip | !echo foo bar baz | !hello"
        , "!ban | !flip | !echo foo bar baz | !hello"
        , "!bernar | !ban | !hlelo | !echo foo bar baz"
        , "!bernar | !ban | !flip | | !echo foo bar baz"
        , "!bernar | !ban | !flip | !echo foo bar baz |"
        ]
