module Main where

import Chat
    ( control
    , extract
    , foldControl
    , inject
    , options
    , relay
    , sanitize
    , tokenize
    , validate
    )
import Test.HUnit (Counts, Test(TestCase, TestList), assertEqual, runTestTT)
import Test.HUnit.Lang (Assertion)
import Types (Response(None, POST, Websocket), message)

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

testFoldControl :: [Assertion]
testFoldControl =
    [ assertEqual "assertEqual foldControl !HELLO" (foldControl "!HELLO") ""
    , assertEqual
          "assertEqual foldControl !hello | !upper"
          (foldControl "!hello | !upper")
          "HELLO!"
    , assertEqual
          "assertEqual foldControl !echo HELLO! | !lower | !rev"
          (foldControl "!echo HELLO! | !lower | !rev")
          "!olleh"
    , assertEqual
          "assertEqual foldControl !hello | !echo | !rev | !upper"
          (foldControl "!hello | !echo | !rev | !upper")
          "!OLLEH"
    , assertEqual
          "assertEqual foldControl !bernar"
          (foldControl "!bernar")
          ":stache:"
    , assertEqual
          "assertEqual foldControl !bernar | !rev"
          (foldControl "!bernar | !rev")
          ":stache:"
    , assertEqual
          "assertEqual foldControl !hello | rev"
          (foldControl "!hello | rev")
          ""
    , assertEqual
          "assertEqual foldControl !hello | !echo | !echo | !echo"
          (foldControl "!hello | !echo | !echo | !echo")
          "Hello!"
    , assertEqual
          "assertEqual foldControl !echo | !echo"
          (foldControl "!echo | !echo")
          ""
    , assertEqual
          "assertEqual foldControl !help | !upper | !rev"
          (foldControl "!help | !upper | !rev")
          options
    , assertEqual
          "assertEqual foldControl !hello | !bold | !em"
          (foldControl "!hello | !bold | !em")
          "_*Hello!*_"
    , assertEqual
          "assertEqual foldControl !echo foo bar baz | !bold | !em"
          (foldControl "!echo foo bar baz | !bold | !em")
          "_*foo bar baz*_"
    , assertEqual
          "assertEqual foldControl !echo {hello}"
          (foldControl "!echo {hello}")
          ""
    , assertEqual
          "assertEqual foldControl !echo \\n"
          (foldControl "!echo \\n")
          ""
    , assertEqual
          "assertEqual foldControl !echo foo bar baz!{} | !rev"
          (foldControl "!echo foo bar baz!{} | !rev")
          ""
    , assertEqual
          "assertEqual foldControl !hello {}!hello"
          (foldControl "!hello {}!hello")
          ""
    , assertEqual
          "assertEqual foldControl !hello | !rev | !ban | !em | !2019"
          (foldControl "!hello | !rev | !ban | !em | !2019")
          "_!olleH has been *banned*_ in 2019."
    , assertEqual
          "assertEqual foldControl !echo ban Bernar | !em | !bold"
          (foldControl "!echo ban Bernar! | !em | !bold")
          "*_ban Bernar!_*"
    , assertEqual
          "assertEqual foldControl !righton"
          (foldControl "!righton")
          ":righton:x:100:"
    , assertEqual
          "assertEqual foldControl !hello | !bold | ! !ban | !em | !2019"
          (foldControl "!hello | !bold | ! !ban | !em | !2019")
          ""
    , assertEqual
          "assertEqual foldControl !post | !hello"
          (foldControl "!post | !hello")
          "[POST] ..."
    , assertEqual
          "assertEqual foldControl !hello | !post"
          (foldControl "!hello | !post")
          "[POST] ..."
    , assertEqual
          "assertEqual foldControl !hello | !2019 | !em | !ban | !echo"
          (foldControl "!hello | !2019 | !em | !ban | !echo")
          "_Hello! in 2019_ has been *banned*."
    ]

testRelay :: [Assertion]
testRelay =
    [ assertEqual
          "assertEqual relay <no feedback loop>"
          (relay "A" 1 $ message "1" "!hello" "A" "channel")
          None
    , assertEqual
          "assertEqual relay !hello"
          (relay "A" 1 $ message "1" "!hello" "B" "channel")
          (Websocket $ inject 1 "channel" "Hello!")
    , assertEqual
          "assertEqual relay !echo ..."
          (relay "A" 1 $ message "1" "!echo foo bar baz" "B" "channel")
          (Websocket $ inject 1 "channel" "foo bar baz")
    , assertEqual
          "assertEqual relay !help"
          (relay "A" 1 $ message "1" "!help" "B" "channel")
          (Websocket $ inject 1 "channel" options)
    , assertEqual
          "assertEqual relay <pipe>"
          (relay "A" 1 $ message "1" "!echo foo | !rev | !upper" "B" "channel")
          (Websocket $ inject 1 "channel" "OOF")
    , assertEqual
          "assertEqual relay <pipe error>"
          (relay "A" 1 $ message "1" "!echo foo | | !upper" "B" "channel")
          None
    , assertEqual
          "assertEqual relay !post"
          (relay "A" 1 $ message "1" "!post" "B" "channel")
          (POST "[POST] ...")
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs =
        testExtract :
        concat
            [ testSanitize
            , testTokenize
            , testControl
            , testFoldControl
            , testRelay
            , testValidate
            ]
