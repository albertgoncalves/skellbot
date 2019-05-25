{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands (parse, tokenize)
import Test.HUnit (Counts, Test(TestCase, TestList), (@=?), runTestTT)
import Test.HUnit.Lang (Assertion)
import Transport (extract, inject, relay)
import Types (Response(Websocket), message)

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
    [ tokenize "!hello|!flip" @=? [["!hello"], ["!flip"]]
    , tokenize "!hello | !flip" @=? [["!hello"], ["!flip"]]
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
    , parse "!ban | !flip | !echo foo bar baz | !hello" @=? Nothing
    , parse "!bernar | !ban | !flip | | !echo foo bar baz" @=? Nothing
    , parse "!bernar | !ban | !flip | !echo foo bar baz |" @=? Nothing
    , parse "!echo hello | !ban | !2019 | !flip" @=?
      Just "9102 ni _*dennab* neeb sah olleh_"
    , parse "!hello | !ban | !2019 | !flip | !hello" @=? Nothing
    , parse "!echo :stache:" @=? Just ":stache:"
    ]

testRelay :: [Assertion]
testRelay =
    [ relay "A" 1 (message "1" "!hello" "A" "channel") @=? Nothing
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
    xs = testExtract : testTokenize ++ testParse ++ testRelay
