{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Monad ((<=<))
import Data.Map.Strict (Map, fromList, keys, lookup, member)
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

(??) :: Functor f => f (a -> b) -> a -> f b
f ?? x = fmap ($ x) f

format :: String -> Text -> Text
format x = pack . printf x . unpack

tokenize :: Text -> [[Text]]
tokenize = map (words . strip) . splitOn "|"

convert :: [Text] -> Maybe Command
convert [] = Nothing
convert (x:xs)
    | x `member` metaCommands = f Meta x
    | x `member` callCommands = f Call x
    | x `member` pipeCommands = f Pipe (x, unwords xs)
    | otherwise = Nothing
  where
    f = (Just .)

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
combine Nothing (Pipe (y, ys)) = lookup y pipeCommands ?? ys
combine (Just (Websocket x)) (Pipe (y, ys))
    | null ys = lookup y pipeCommands ?? x
    | null x = lookup y pipeCommands ?? ys
    | otherwise = Nothing
combine _ _ = Nothing

transform :: Text -> Maybe Response
transform =
    foldl combine Nothing <=< filterCommands <=< mapM convert . tokenize

metaCommands :: Map Text Response
metaCommands =
    fromList $
    map (\(a, b) -> (a, Websocket b))
        [ ("!help", ((\x -> concat ["`", x, "`"]) . intercalate "` `") xs)
        , ("!example", "Try this!\\n`!echo Bernar | !ban | !2019`")
        , ( "!sweetbot"
          , ">_The colour of my soul is iron-grey and *sad bats* wheel about the\
                \ steeple of my dreams._")
        ]
  where
    xs = keys pipeCommands ++ keys metaCommands

callCommands :: Map Text Response
callCommands = fromList [("!post", POST "sentinel")]

pipeCommands :: Map Text (Text -> Response)
pipeCommands =
    fromList $
    map (\(a, b) -> (a, Websocket . b))
        [ ("!2019", format "%s in 2019")
        , ("!ban", format "_%s_ has been *banned*")
        , ("!echo", id)
        , ("!flip", reverse)
        , ("!lower", toLower)
        , ("!upper", toUpper)
        ]
