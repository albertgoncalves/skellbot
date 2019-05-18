{-# OPTIONS_GHC -Wall #-}

module Main where

import Bridge
    ( Message(Message, channel, messageId, text, user)
    , extract
    , relay
    )
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)

testExtract :: Assertion
testExtract = assertEqual "assertEqual extract" (extract input) (Just output)
  where
    input =
        "{\"client_msg_id\":\"...\"\
        \,\"suppress_notification\":false\
        \,\"type\":\"message\"\
        \,\"text\":\"!bot hello\"\
        \,\"user\":\"...\"\
        \,\"team\":\"...\"\
        \,\"channel\":\"...\"\
        \,\"event_ts\":\"...\"\
        \,\"ts\":\"...\"}"
    output =
        Message
            { messageId = "..."
            , text = "!bot hello"
            , user = "..."
            , channel = "..."
            }

testRelay :: Assertion
testRelay = assertEqual "assertEqual relay" (relay "" 1 input) (Just output)
  where
    input =
        Message
            { messageId = "..."
            , text = "!bot hello"
            , user = "..."
            , channel = "..."
            }
    output =
        "{\"id\":1\
        \,\"type\":\"message\"\
        \,\"channel\":\"...\"\
        \,\"text\":\"hello\"}"

main :: IO Counts
main = (runTestTT . TestList . map TestCase) [testExtract, testRelay]
