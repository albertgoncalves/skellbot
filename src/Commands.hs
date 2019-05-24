{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((??))
import Control.Monad ((<=<), foldM)
import Data.Map.Strict (Map, fromList, keys, lookup)
import Data.Text
    ( Text
    , intercalate
    , null
    , pack
    , reverse
    , splitOn
    , strip
    , toLower
    , toUpper
    , unpack
    , unwords
    , words
    )
import Prelude hiding (lookup, null, reverse, unwords, words)
import Text.Printf (printf)

newtype Command =
    Command (Text, Text)
    deriving (Eq, Show)

format :: String -> Text -> Text
format x = pack . printf x . unpack

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

combine :: Text -> Command -> Maybe Text
combine x (Command (y, ys))
    | null ys = f ?? x
    | null x = f ?? ys
    | otherwise = Nothing
  where
    f = lookup y commands

parse :: Text -> Maybe Text
parse = foldM combine "" <=< mapM (whitelist <=< convert) . tokenize

commands :: Map Text (Text -> Text)
commands =
    fromList
        [ ("!2019", format "%s in 2019")
        , ("!ban", format "%s has been banned")
        , ("!bernar", const ":stache:")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!hello", const "Hello!")
        , ("!help", (const . intercalate " ~ " . keys) commands)
        , ("!lower", toLower)
        , ("!upper", toUpper)
        ]

main :: IO ()
main = mapM_ (print . parse) xs
  where
    xs =
        [ "!help"
        , "!help | !flip | !upper"
        , "!bernar | !ban"
        , "!bernar | !ban | !flip"
        , "!bernar | !ban | !flip | !upper"
        , "!hello | !flip | !lower"
        , "!bernar| !ban | !2019"
        , "!bernar | !ban | !flip | !echo foo bar baz"
        , "!bernar | !ban | !flip | !echo foo bar baz | !hello"
        , "!echo foo bar baz | !flip"
        , "!ban | !flip | !echo foo bar baz | !hello"
        , "!bernar | !ban | !hlelo | !echo foo bar baz"
        , "!bernar | !ban | !flip | | !echo foo bar baz"
        , "!bernar | !ban | !flip | !echo foo bar baz |"
        ]
