{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Monad ((<=<))
import Data.Map.Strict (Map, fromList, keys, lookup)
import Data.Text
    ( Text
    , concat
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
import Prelude hiding (concat, lookup, null, reverse, unwords, words)
import Text.Printf (printf)
import Types (Command(Call, Meta, Pipe), Response(POST, Websocket))

format :: String -> Text -> Text
format x = pack . printf x . unpack

tokenize :: Text -> [[Text]]
tokenize = map (words . strip) . splitOn "|"

convert :: [Text] -> Maybe Command
convert [] = Nothing
convert (x:xs)
    | x `elem` keys metaCommands = f Meta x
    | x `elem` keys callCommands = f Call x
    | x `elem` keys pipeCommands = f Pipe (x, unwords xs)
    | otherwise = Nothing
  where
    f t = Just . t

filterCommands :: [Command] -> Maybe [Command]
filterCommands xs@[Meta _] = Just xs
filterCommands xs@[Call _] = Just xs
filterCommands (Pipe (_, ""):_) = Nothing
filterCommands xs
    | (not . any f) xs = Just xs
    | otherwise = Nothing
  where
    f (Meta _) = True
    f (Call _) = True
    f (Pipe _) = False

combine :: Maybe Response -> Command -> Maybe Response
combine _ (Meta y) = lookup y metaCommands
combine _ (Call y) = lookup y callCommands
combine Nothing (Pipe (_, "")) = Nothing
combine Nothing (Pipe (y, ys)) = lookup y pipeCommands <*> Just ys
combine (Just (Websocket x)) (Pipe (y, ys))
    | null ys = lookup y pipeCommands <*> Just x
    | null x = lookup y pipeCommands <*> Just ys
    | otherwise = Nothing
combine _ _ = Nothing

parse :: Text -> Maybe Response
parse = foldl combine Nothing <=< filterCommands <=< mapM convert . tokenize

metaCommands :: Map Text Response
metaCommands =
    fromList
        [("!help", (Websocket . f . intercalate "`\\n`" . keys) pipeCommands)]
  where
    f x = concat ["`", x, "`"]

callCommands :: Map Text Response
callCommands = fromList [("!post", POST "sentinel")]

pipeCommands :: Map Text (Text -> Response)
pipeCommands =
    fromList $
    map (\(a, b) -> (a, Websocket . b))
        [ ("!2019", format "_%s_ in 2019")
        , ("!ban", format "%s has been *banned*")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!lower", toLower)
        , ("!upper", toUpper)
        ]
