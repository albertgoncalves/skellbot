{-# OPTIONS_GHC -Wall #-}

module Main where

import Bridge (extract, relay, validate)
import Chat (inject, options)
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Types (Message(Message, channel, messageId, text, user), message)

testValidate :: [Assertion]
testValidate =
    [ assertEqual "assertEqual validate !hello" (validate "!hello ") ["hello"]
    , assertEqual
          "assertEqual validate !foo ..."
          (validate "!foo bar baz")
          ["foo", "bar", "baz"]
    ]

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

testRelay :: [Assertion]
testRelay =
    [ assertEqual
          "assertEqual relay <feedback loop>"
          (relay "A" 1 $ message "1" "!hello" "A" "channel")
          Nothing
    , assertEqual
          "assertEqual relay !hello"
          (relay "A" 1 $ message "1" "!hello" "B" "channel")
          (Just $ inject 1 "channel" "Hello!")
    , assertEqual
          "assertEqual relay !echo ..."
          (relay "A" 1 $ message "1" "!echo foo bar baz" "B" "channel")
          (Just $ inject 1 "channel" "foo bar baz")
    , assertEqual
          "assertEqual relay !help"
          (relay "A" 1 $ message "1" "!help" "B" "channel")
          (Just $ inject 1 "channel" options)
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs = testExtract : (testRelay ++ testValidate)
