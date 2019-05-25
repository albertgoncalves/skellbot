{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Lens ((??))
import Control.Monad ((<=<))
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
combine (Just (Websocket x)) (Pipe (y, ys))
    | T.null ys = f ?? x
    | T.null x = f ?? ys
    | otherwise = Nothing
  where
    f = lookup y pipeCommands
combine Nothing (Pipe (y, ys)) = lookup y pipeCommands ?? ys
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
