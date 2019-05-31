{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands
    ( convert
    , filterCommands
    , metaCommands
    , pipeCommands
    , tokenize
    , transform
    )
import Data.Map.Strict (keys)
import Data.Text (intercalate, pack)
import qualified Data.Text as T
import Test.HUnit (Counts, Test(TestCase, TestList), (@=?), runTestTT)
import Test.HUnit.Lang (Assertion)
import Transport (extract, inject, relay)
import Types (Command(Call, Meta, Pipe), Response(POST, Websocket), message)

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
    [ tokenize "!echo hello|!flip" @=? [["!echo", "hello"], ["!flip"]]
    , tokenize "!echo hello | !flip" @=? [["!echo", "hello"], ["!flip"]]
    ]

testConvert :: [Assertion]
testConvert =
    [ convert ["!help"] @=? Just (Meta "!help")
    , convert ["!echo", "hello"] @=? Just (Pipe ("!echo", "hello"))
    , map convert [["!help"], ["!flip"]] @=?
      [Just (Meta "!help"), Just (Pipe ("!flip", ""))]
    ]

testFilterCommands :: [Assertion]
testFilterCommands =
    [ filterCommands [Meta "!help", Pipe ("!flip", "")] @=? Nothing
    , filterCommands [Pipe ("!echo", "hello"), Pipe ("!flip", "")] @=?
      Just [Pipe ("!echo", "hello"), Pipe ("!flip", "")]
    , filterCommands [Call "!post", Pipe ("!ban", "")] @=? Nothing
    , filterCommands [Call "!post"] @=? Just [Call "!post"]
    ]

testTransform :: [Assertion]
testTransform =
    [ transform "!echo bernar | !ban" @=? f "_bernar_ has been *banned*"
    , transform "!echo bernar | !ban | !upper" @=?
      f "_BERNAR_ HAS BEEN *BANNED*"
    , transform "!echo Hello! | !flip | !lower" @=? f "!olleh"
    , transform "!echo bernar| !ban | !2019" @=?
      f "_bernar_ has been *banned* in 2019"
    , transform "!echo bernar | !ban | !flip | !echo foo bar baz" @=? Nothing
    , transform "!echo foo bar baz | !flip" @=? f "zab rab oof"
    , transform "!ban | !flip | !echo foo bar baz | !echo hello" @=? Nothing
    , transform "!bernar | !ban | !flip | | !echo foo bar baz" @=? Nothing
    , transform "!bernar | !ban | !flip | !echo foo bar baz |" @=? Nothing
    , transform "!echo hello | !ban | !2019 | !flip" @=?
      f "9102 ni *dennab* neeb sah _olleh_"
    , transform "!echo :stache:" @=? f ":stache:"
    , transform "!help" @=?
      (f . (\x -> T.concat ["`", x, "`"]) . intercalate "` `")
          (keys pipeCommands ++ keys metaCommands)
    , transform "!help | !flip" @=? Nothing
    , transform "!echo foo | !flip | !echo bar" @=? Nothing
    , transform "!post" @=? (Just . POST) "sentinel"
    , transform "!post | !echo baz" @=? Nothing
    , transform "!ban | !echo foo | !flip" @=? Nothing
    , transform "!echo foo | !ban | !flip" @=?
      Just (Websocket "*dennab* neeb sah _oof_")
    , transform "!echo foo | !ban | !echo bar | !flip" @=? Nothing
    , transform "!echo :stache: | !bday" @=?
      Just
          (Websocket
               ":robot_face::birthday: Happy birthday, *:stache:*!\
                \ :birthday::robot_face:")
    ]
  where
    f = Just . Websocket

testRelay :: [Assertion]
testRelay =
    [ relay "A" 1 (message "1" "!echo hello" "A" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo foo | | !upper" "B" "channel") @=?
      Nothing
    , relay "A" 1 (message "1" "!echo {}" "B" "channel") @=? Nothing
    , relay "A" 1 (message "1" "!echo hello" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "hello"))
    , relay "A" 1 (message "1" "!echo foo bar baz" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "foo bar baz"))
    , relay "A" 1 (message "1" "!echo foo | !flip | !upper" "B" "channel") @=?
      (Just . Websocket) (pack (inject 1 "channel" "OOF"))
    ]

main :: IO Counts
main = (runTestTT . TestList . map TestCase) xs
  where
    xs =
        testExtract :
        concat
            [ testTokenize
            , testConvert
            , testFilterCommands
            , testTransform
            , testRelay
            ]
