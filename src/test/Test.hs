{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands (parse, tokenize)
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Transport (extract, inject, relay)
import Types (Response(Websocket), message)

testExtract :: Assertion
testExtract =
    assertEqual
        "assertEqual extract"
        (extract input)
        (Just $ message "id" "hello" "user" "channel")
  where
    input =
        "{\"client_msg_id\":\"id\"\
        \,\"type\":\"message\"\
        \,\"text\":\"hello\"\
        \,\"user\":\"user\"\
        \,\"channel\":\"channel\"}"

testTokenize :: [Assertion]
testTokenize =
    [ assertEqual
          "tokenize !hello|!flip"
          (tokenize "!hello|!flip")
          [["!hello"], ["!flip"]]
    , assertEqual
          "tokenize !hello | !flip"
          (tokenize "!hello | !flip")
          [["!hello"], ["!flip"]]
    ]

testParse :: [Assertion]
testParse =
    [ assertEqual
          "parse !bernar | !ban"
          (parse "!bernar | !ban")
          (Just ":stache: has been *banned*")
    , assertEqual
          "parse !bernar | !ban | !upper"
          (parse "!bernar | !ban | !upper")
          (Just ":STACHE: HAS BEEN *BANNED*")
    , assertEqual
          "parse !hello | !flip | !lower"
          (parse "!hello | !flip | !lower")
          (Just "!olleh")
    , assertEqual
          "parse !bernar| !ban | !2019"
          (parse "!bernar| !ban | !2019")
          (Just "_:stache: has been *banned*_ in 2019")
    , assertEqual
          "parse !bernar | !ban | !flip | !echo foo bar baz"
          (parse "!bernar | !ban | !flip | !echo foo bar baz")
          Nothing
    , assertEqual
          "parse !echo foo bar baz | !flip"
          (parse "!echo foo bar baz | !flip")
          (Just "zab rab oof")
    , assertEqual
          "parse !ban | !flip | !echo foo bar baz | !hello"
          (parse "!ban | !flip | !echo foo bar baz | !hello")
          Nothing
    , assertEqual
          "parse !bernar | !ban | !flip | | !echo foo bar baz"
          (parse "!bernar | !ban | !flip | | !echo foo bar baz")
          Nothing
    , assertEqual
          "parse !bernar | !ban | !flip | !echo foo bar baz |"
          (parse "!bernar | !ban | !flip | !echo foo bar baz |")
          Nothing
    ]

testRelay :: [Assertion]
testRelay =
    [ assertEqual
          "assertEqual relay <no feedback loop>"
          (relay "A" 1 $ message "1" "!hello" "A" "channel")
          Nothing
    , assertEqual
          "assertEqual relay !hello"
          (relay "A" 1 $ message "1" "!hello" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "Hello!")
    , assertEqual
          "assertEqual relay !echo ..."
          (relay "A" 1 $ message "1" "!echo foo bar baz" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "foo bar baz")
    , assertEqual
          "assertEqual relay <pipe>"
          (relay "A" 1 $
           message "1" "!echo foo | !flip | !upper" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "OOF")
    , assertEqual
          "assertEqual relay <pipe error>"
          (relay "A" 1 $ message "1" "!echo foo | | !upper" "B" "channel")
          Nothing
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs = testExtract : testTokenize ++ testParse ++ testRelay
