{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands (convert, filterCommands, parse, pipeCommands, tokenize)
import Data.Map.Strict (keys)
import Data.Text (intercalate)
import qualified Data.Text as T
import Test.HUnit (Counts, Test(TestCase, TestList), (@=?), runTestTT)
import Test.HUnit.Lang (Assertion)
import Transport (extract, inject, relay)
import Types (Command(Meta, Pipe), Response(Websocket), message)

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
    [ convert ["!help"] @=? Just (Meta ("!help", ""))
    , convert ["!echo", "hello"] @=? Just (Pipe ("!echo", "hello"))
    , map convert [["!help"], ["!flip"]] @=?
      [Just (Meta ("!help", "")), Just (Pipe ("!flip", ""))]
    ]

testFilterCommands :: [Assertion]
testFilterCommands =
    [ filterCommands [Meta ("!help", ""), Pipe ("!flip", "")] @=? Nothing
    , filterCommands [Pipe ("!echo", "hello"), Pipe ("!flip", "")] @=?
      Just [Pipe ("!echo", "hello"), Pipe ("!flip", "")]
    ]

testParse :: [Assertion]
testParse =
    [ parse "!echo bernar | !ban" @=? Just "bernar has been *banned*"
    , parse "!echo bernar | !ban | !upper" @=? Just "BERNAR HAS BEEN *BANNED*"
    , parse "!echo Hello! | !flip | !lower" @=? Just "!olleh"
    , parse "!echo bernar| !ban | !2019" @=?
      Just "_bernar has been *banned*_ in 2019"
    , parse "!echo bernar | !ban | !flip | !echo foo bar baz" @=? Nothing
    , parse "!echo foo bar baz | !flip" @=? Just "zab rab oof"
    , parse "!ban | !flip | !echo foo bar baz | !echo hello" @=? Nothing
    , parse "!bernar | !ban | !flip | | !echo foo bar baz" @=? Nothing
    , parse "!bernar | !ban | !flip | !echo foo bar baz |" @=? Nothing
    , parse "!echo hello | !ban | !2019 | !flip" @=?
      Just "9102 ni _*dennab* neeb sah olleh_"
    , parse "!echo :stache:" @=? Just ":stache:"
    , parse "!help" @=? (Just . f . intercalate "`\\n`" . keys) pipeCommands
    , parse "!help | !flip" @=? Nothing
    , parse "!echo foo| !flip | !echo bar" @=? Nothing
    ]
  where
    f x = T.concat ["`", x, "`"]

testRelay :: [Assertion]
testRelay =
    [ relay "A" 1 (message "1" "!echo hello" "A" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo foo | | !upper" "B" "channel") @=?
      Nothing
    , relay "A" 1 (message "1" "!echo {}" "B" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo hello" "B" "channel") @=?
      (Just . Websocket) (inject 1 "channel" "hello")
    , relay "A" 1 (message "1" "!echo foo bar baz" "B" "channel") @=?
      (Just . Websocket) (inject 1 "channel" "foo bar baz")
    , relay "A" 1 (message "1" "!echo foo | !flip | !upper" "B" "channel") @=?
      (Just . Websocket) (inject 1 "channel" "OOF")
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs =
        testExtract :
        concat
            [ testTokenize
            , testConvert
            , testParse
            , testFilterCommands
            , testRelay
            ]
