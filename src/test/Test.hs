{-# OPTIONS_GHC -Wall #-}

module Main where

import Bridge (extract, relay, validate)
import Chat (inject)
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Types (Message(Message, channel, messageId, text, user))

testValidate :: [Assertion]
testValidate =
    [ assertEqual "assertEqual validate !hello" (validate "!hello ") ["hello"]
    , assertEqual
          "assertEqual validate !foo ..."
          (validate "!foo bar baz")
          ["foo", "bar", "baz"]
    ]

testExtract :: Assertion
testExtract = assertEqual "assertEqual extract" (extract input) (Just output)
  where
    input =
        "{\"client_msg_id\":\"id\"\
        \,\"type\":\"message\"\
        \,\"text\":\"hello\"\
        \,\"user\":\"user\"\
        \,\"channel\":\"channel\"}"
    output =
        Message
            { messageId = "id"
            , text = "hello"
            , user = "user"
            , channel = "channel"
            }

testRelay :: [Assertion]
testRelay =
    [ assertEqual
          "assertEqual relay !hello"
          (relay "A" 1 inputHello)
          (Just $ inject 1 "channel" "Hello!")
    , assertEqual
          "assertEqual relay !echo ..."
          (relay "A" 1 inputEcho)
          (Just $ inject 1 "channel" "foo bar baz")
    ]
  where
    inputHello =
        Message
            { messageId = "..."
            , text = "!hello"
            , user = "B"
            , channel = "channel"
            }
    inputEcho =
        Message
            { messageId = "..."
            , text = "!echo foo bar baz"
            , user = "B"
            , channel = "channel"
            }

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs = testExtract : (testRelay ++ testValidate)
