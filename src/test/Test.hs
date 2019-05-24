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
          "parse !echo bernar | !ban"
          (parse "!echo bernar | !ban")
          (Just "bernar has been *banned*")
    , assertEqual
          "parse !echo bernar | !ban | !upper"
          (parse "!echo bernar | !ban | !upper")
          (Just "BERNAR HAS BEEN *BANNED*")
    , assertEqual
          "parse !echo Hello! | !flip | !lower"
          (parse "!echo Hello! | !flip | !lower")
          (Just "!olleh")
    , assertEqual
          "parse !echo bernar| !ban | !2019"
          (parse "!echo bernar| !ban | !2019")
          (Just "_bernar has been *banned*_ in 2019")
    , assertEqual
          "parse !echo bernar | !ban | !flip | !echo foo bar baz"
          (parse "!echo bernar | !ban | !flip | !echo foo bar baz")
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
    , assertEqual
          "parse !echo hello | !ban | !2019 | !flip"
          (parse "!echo hello | !ban | !2019 | !flip")
          (Just "9102 ni _*dennab* neeb sah olleh_")
    , assertEqual
          "parse !hello | !ban | !2019 | !flip | !hello"
          (parse "!hello | !ban | !2019 | !flip | !hello")
          Nothing
    , assertEqual
          "parse !echo :stache:"
          (parse "!echo :stache:")
          (Just ":stache:")
    ]

testRelay :: [Assertion]
testRelay =
    [ assertEqual
          "assertEqual relay <no feedback loop>"
          (relay "A" 1 $ message "1" "!hello" "A" "channel")
          Nothing
    , assertEqual
          "assertEqual relay !echo foo | | !upper"
          (relay "A" 1 $ message "1" "!echo foo | | !upper" "B" "channel")
          Nothing
    , assertEqual
          "assertEqual relay !echo {}"
          (relay "A" 1 $ message "1" "!echo {}" "B" "channel")
          Nothing
    , assertEqual
          "assertEqual relay !echo hello"
          (relay "A" 1 $ message "1" "!echo hello" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "hello")
    , assertEqual
          "assertEqual relay !echo foo bar baz"
          (relay "A" 1 $ message "1" "!echo foo bar baz" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "foo bar baz")
    , assertEqual
          "assertEqual relay !echo foo | !flip | !upper"
          (relay "A" 1 $
           message "1" "!echo foo | !flip | !upper" "B" "channel")
          (Just $ Websocket $ inject 1 "channel" "OOF")
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs = testExtract : testTokenize ++ testParse ++ testRelay
