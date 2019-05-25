{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands (convert, filterCommands, parse, pipeCommands, tokenize)
import Data.Map.Strict (keys)
import Data.Text (intercalate, pack)
import qualified Data.Text as T
import Test.HUnit (Counts, Test(TestCase, TestList), (@=?), runTestTT)
import Test.HUnit.Lang (Assertion)
import Transport (extract, inject, relay)
import Types (Command(Call, Meta, Pipe), Response(POST, Websocket), message)

testExtract :: Assertion
testExtract = extract input @=? Just (message "id" "hello" "user" "channel")
  where
    input =
        "{\"client_msg_id\":\"id\"\
        \,\"type\":\"message\"\
        \,\"text\":\"hello\"\
        \,\"user\":\"user\"\
        \,\"channel\":\"channel\"}"

testTokenize :: [Assertion]
testTokenize =
    [ tokenize "!echo hello|!flip" @=? [["!echo", "hello"], ["!flip"]]
    , tokenize "!echo hello | !flip" @=? [["!echo", "hello"], ["!flip"]]
    ]

testConvert :: [Assertion]
testConvert =
    [ convert ["!help"] @=? Just (Meta "!help")
    , convert ["!echo", "hello"] @=? Just (Pipe ("!echo", "hello"))
    , map convert [["!help"], ["!flip"]] @=?
      [Just (Meta "!help"), Just (Pipe ("!flip", ""))]
    ]

testFilterCommands :: [Assertion]
testFilterCommands =
    [ filterCommands [Meta "!help", Pipe ("!flip", "")] @=? Nothing
    , filterCommands [Pipe ("!echo", "hello"), Pipe ("!flip", "")] @=?
      Just [Pipe ("!echo", "hello"), Pipe ("!flip", "")]
    , filterCommands [Call "!post", Pipe ("!ban", "")] @=? Nothing
    , filterCommands [Call "!post"] @=? Just [Call "!post"]
    ]

testParse :: [Assertion]
testParse =
    [ parse "!echo bernar | !ban" @=? f "bernar has been *banned*"
    , parse "!echo bernar | !ban | !upper" @=? f "BERNAR HAS BEEN *BANNED*"
    , parse "!echo Hello! | !flip | !lower" @=? f "!olleh"
    , parse "!echo bernar| !ban | !2019" @=?
      f "_bernar has been *banned*_ in 2019"
    , parse "!echo bernar | !ban | !flip | !echo foo bar baz" @=? Nothing
    , parse "!echo foo bar baz | !flip" @=? f "zab rab oof"
    , parse "!ban | !flip | !echo foo bar baz | !echo hello" @=? Nothing
    , parse "!bernar | !ban | !flip | | !echo foo bar baz" @=? Nothing
    , parse "!bernar | !ban | !flip | !echo foo bar baz |" @=? Nothing
    , parse "!echo hello | !ban | !2019 | !flip" @=?
      f "9102 ni _*dennab* neeb sah olleh_"
    , parse "!echo :stache:" @=? f ":stache:"
    , parse "!help" @=?
      (f . (\x -> T.concat ["`", x, "`"]) . intercalate "`\\n`" . keys)
          pipeCommands
    , parse "!help | !flip" @=? Nothing
    , parse "!echo foo | !flip | !echo bar" @=? Nothing
    , parse "!post" @=? (Just . POST) "sentinel"
    , parse "!post | !echo baz" @=? Nothing
    ]
  where
    f = Just . Websocket

testRelay :: [Assertion]
testRelay =
    [ relay "A" 1 (message "1" "!echo hello" "A" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo foo | | !upper" "B" "channel") @=?
      Nothing
    , relay "A" 1 (message "1" "!echo {}" "B" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo hello" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "hello"))
    , relay "A" 1 (message "1" "!echo foo bar baz" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "foo bar baz"))
    , relay "A" 1 (message "1" "!echo foo | !flip | !upper" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "OOF"))
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs =
        testExtract :
        concat
            [ testTokenize
            , testConvert
            , testFilterCommands
            , testParse
            , testRelay
            ]
