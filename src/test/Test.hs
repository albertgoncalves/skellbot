{-# OPTIONS_GHC -Wall #-}

module Main where

import Chat
    ( control
    , extract
    , foldCommands
    , inject
    , options
    , relay
    , sanitize
    , tokenize
    , validate
    )
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Types (message)

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

testValidate :: [Assertion]
testValidate =
    [ assertEqual "assertEqual validate !hello" (validate "!hello") ["hello"]
    , assertEqual
          "assertEqual validate !echo foo bar baz | !rev"
          (validate "!echo foo bar baz")
          ["echo", "foo", "bar", "baz"]
    ]

testSanitize :: [Assertion]
testSanitize =
    [ assertEqual "assertEqual sanitize !hello" (sanitize "!hello") "!hello"
    , assertEqual "assertEqual sanitize !hello {}" (sanitize "!hello {}") ""
    , assertEqual "assertEqual sanitize !hello \"" (sanitize "!hello \"") ""
    ]

testTokenize :: [Assertion]
testTokenize =
    [ assertEqual
          "tokenize !hello|!rev"
          (tokenize "!hello|!rev")
          [["hello"], ["rev"]]
    , assertEqual
          "tokenize !hello | !rev"
          (tokenize "!hello | !rev")
          [["hello"], ["rev"]]
    ]

testControl :: [Assertion]
testControl =
    [ assertEqual "assertEqual control hello" (control "" ["hello"]) "Hello!"
    , assertEqual "assertEqual control HELLO" (control "" ["HELLO"]) ""
    , assertEqual
          "assertEqual control ban Bernar"
          (control "" ["ban", "Bernar"])
          "Bernar has been *banned*."
    ]

testFoldCommands :: [Assertion]
testFoldCommands =
    [ assertEqual "assertEqual foldCommands !HELLO" (foldCommands "!HELLO") ""
    , assertEqual
          "assertEqual foldCommands !hello | !upper"
          (foldCommands "!hello | !upper")
          "HELLO!"
    , assertEqual
          "assertEqual foldCommands !echo HELLO! | !lower | !rev"
          (foldCommands "!echo HELLO! | !lower | !rev")
          "!olleh"
    , assertEqual
          "assertEqual foldCommands !hello | !echo | !rev | !upper"
          (foldCommands "!hello | !echo | !rev | !upper")
          "!OLLEH"
    , assertEqual
          "assertEqual foldCommands !bernar"
          (foldCommands "!bernar")
          ":stache:"
    , assertEqual
          "assertEqual foldCommands !bernar | !rev"
          (foldCommands "!bernar | !rev")
          ":stache:"
    , assertEqual
          "assertEqual foldCommands !hello | rev"
          (foldCommands "!hello | rev")
          ""
    , assertEqual
          "assertEqual foldCommands !hello | !echo | !echo | !echo"
          (foldCommands "!hello | !echo | !echo | !echo")
          "Hello!"
    , assertEqual
          "assertEqual foldCommands !echo | !echo"
          (foldCommands "!echo | !echo")
          ""
    , assertEqual
          "assertEqual foldCommands !help | !upper | !rev"
          (foldCommands "!help | !upper | !rev")
          options
    , assertEqual
          "assertEqual foldCommands !hello | !bold | !em"
          (foldCommands "!hello | !bold | !em")
          "_*Hello!*_"
    , assertEqual
          "assertEqual foldCommands !echo foo bar baz | !bold | !em"
          (foldCommands "!echo foo bar baz | !bold | !em")
          "_*foo bar baz*_"
    , assertEqual
          "assertEqual foldCommands !echo {hello}"
          (foldCommands "!echo {hello}")
          ""
    , assertEqual
          "assertEqual foldCommands !echo \\n"
          (foldCommands "!echo \\n")
          ""
    , assertEqual
          "assertEqual foldCommands !echo foo bar baz!{} | !rev"
          (foldCommands "!echo foo bar baz!{} | !rev")
          ""
    , assertEqual
          "assertEqual foldCommands !hello {}!hello"
          (foldCommands "!hello {}!hello")
          ""
    , assertEqual
          "assertEqual foldCommands !hello | !rev | !ban | !em | !year"
          (foldCommands "!hello | !rev | !ban | !em | !year")
          "_!olleH has been *banned*_ in 2019."
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
          (relay "A" 1 $ message "1" "!echo foo | !rev | !upper" "B" "channel")
          (Just $ inject 1 "channel" "OOF")
    , assertEqual
          "assertEqual relay <pipe error>"
          (relay "A" 1 $ message "1" "!echo foo | | !upper" "B" "channel")
          Nothing
    ]

main :: IO Counts
main =
    (runTestTT . TestList . map TestCase)
        (testExtract :
         testSanitize ++
         testTokenize ++
         testControl ++ testFoldCommands ++ testRelay ++ testValidate)
