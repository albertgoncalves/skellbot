{-# OPTIONS_GHC -Wall #-}

module Main where

import Chat (control, extract, foldCommands, inject, options, relay, validate)
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

testControl :: [Assertion]
testControl =
    [ assertEqual "assertEqual control hello" (control "" ["hello"]) "Hello!"
    , assertEqual "assertEqual control HELLO" (control "" ["HELLO"]) "Hello!"
    ]

testFoldCommands :: [Assertion]
testFoldCommands =
    [ assertEqual
          "assertEqual foldCommands !hello|!upper"
          (foldCommands "!hello|!upper")
          "HELLO!"
    , assertEqual
          "assertEqual foldCommands !HELLO"
          (foldCommands "!HELLO")
          "Hello!"
    , assertEqual
          "assertEqual foldCommands !echo HELLO!|!lower|!rev"
          (foldCommands "!echo HELLO!|!lower|!rev")
          "!olleh"
    , assertEqual
          "assertEqual foldCommands !hello|rev"
          (foldCommands "!hello|rev")
          ""
    , assertEqual
          "assertEqual foldCommands !hello|!echo|!echo|!echo"
          (foldCommands "!hello|!echo|!echo|!echo")
          "Hello!"
    , assertEqual
          "assertEqual foldCommands !echo|!echo"
          (foldCommands "!echo|!echo")
          ""
    , assertEqual
          "assertEqual foldCommands !help|!upper"
          (foldCommands "!help|!upper")
          "`!HELLO`\\N`\
        \!ECHO ...`\\N`\
        \!REV ...`\\N`\
        \!UPPER ...`\\N`\
        \!LOWER ...`\\N`\
        \!HELP`"
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
          (Just $ inject 1 "channel" "Hello!")
    , assertEqual
          "assertEqual relay !echo ..."
          (relay "A" 1 $ message "1" "!echo foo bar baz" "B" "channel")
          (Just $ inject 1 "channel" "foo bar baz")
    , assertEqual
          "assertEqual relay !help"
          (relay "A" 1 $ message "1" "!help" "B" "channel")
          (Just $ inject 1 "channel" options)
    , assertEqual
          "assertEqual relay <pipe>"
          (relay "A" 1 $ message "1" "!echo foo|!rev|!upper" "B" "channel")
          (Just $ inject 1 "channel" "OOF")
    , assertEqual
          "assertEqual relay <pipe error>"
          (relay "A" 1 $ message "1" "!echo foo||!upper" "B" "channel")
          Nothing
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs =
        testExtract :
        (testControl ++ testFoldCommands ++ testRelay ++ testValidate)
