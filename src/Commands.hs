{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Lens ((??))
import Control.Monad ((<=<), foldM)
import Data.Map.Strict (Map, fromList, keys, lookup)
import Data.Text
    ( Text
    , concat
    , intercalate
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
import qualified Data.Text as T
import Prelude hiding (concat, lookup, reverse, unwords, words)
import Text.Printf (printf)
import Types (Command(Meta, Pipe))

format :: String -> Text -> Text
format x = pack . printf x . unpack

tokenize :: Text -> [[Text]]
tokenize = map (words . strip) . splitOn "|"

convert :: [Text] -> Maybe Command
convert [] = Nothing
convert (x:xs)
    | x `elem` keys metaCommands = f Meta
    | x `elem` keys pipeCommands = f Pipe
    | otherwise = Nothing
  where
    f t = (Just . t) (x, unwords xs)

combine :: Text -> Command -> Maybe Text
combine _ (Meta (y, ys)) = lookup y metaCommands ?? ys
combine x (Pipe (y, ys))
    | T.null ys = f ?? x
    | T.null x = f ?? ys
    | otherwise = Nothing
  where
    f = lookup y pipeCommands

filterCommands :: [Command] -> Maybe [Command]
filterCommands xs@[Meta _] = Just xs
filterCommands xs
    | null [x | x@(Meta _) <- xs] = Just xs
    | otherwise = Nothing

parse :: Text -> Maybe Text
parse = foldM combine "" <=< filterCommands <=< mapM convert . tokenize

metaCommands :: Map Text (Text -> Text)
metaCommands =
    fromList
        [("!help", (const . f . intercalate "`\\n`" . keys) pipeCommands)]
  where
    f x = concat ["`", x, "`"]

pipeCommands :: Map Text (Text -> Text)
pipeCommands =
    fromList
        [ ("!2019", format "_%s_ in 2019")
        , ("!ban", format "%s has been *banned*")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!lower", toLower)
        , ("!upper", toUpper)
        ]
