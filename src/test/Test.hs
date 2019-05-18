{-# OPTIONS_GHC -Wall #-}

module Main where

import Chat (extract, inject, options, relay, validate)
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Types (message)

testValidate :: [Assertion]
testValidate =
    [ assertEqual "assertEqual validate !hello" (validate "!hello") ["hello"]
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
    [ f "assertEqual relay <no feedback loop>" "!hello" "A" "A" Nothing
    , f "assertEqual relay <syntax error>" "blah" "A" "B" Nothing
    , f "assertEqual relay <pipe error>" "!hello || !upper" "A" "B" Nothing
    , f "assertEqual relay !hello" "!hello" "A" "B" (f' "Hello!")
    , f "assertEqual relay !lower ..." "!lower FOO BAR" "A" "B" (f' "foo bar")
    , f "assertEqual relay !help" "!help" "A" "B" (f' options)
    , f "assertEqual relay !echo ..."
          "!echo foo bar baz"
          "A"
          "B"
          (f' "foo bar baz")
    , f "assertEqual relay !echo ... | !rev"
          "!echo foo bar baz | !rev"
          "A"
          "B"
          (f' "zab rab oof")
    , f "assertEqual relay !echo ... | !upper"
          "!echo foo bar | !upper"
          "A"
          "B"
          (f' "FOO BAR")
    , f "assertEqual relay !echo ... | !echo"
          "!echo foo bar | !echo"
          "A"
          "B"
          (f' "foo bar")
    ]
  where
    f label text botId userId =
        assertEqual label (relay botId 1 $ message "1" text userId "channel")
    f' = Just . inject 1 "channel"

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs = testExtract : (testRelay ++ testValidate)
