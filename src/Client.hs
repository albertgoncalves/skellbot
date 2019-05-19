{-# OPTIONS_GHC -Wall #-}

module Client where

import Chat (extract, relay)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack, unpack)
import Network.WebSockets
    ( ClientApp
    , Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import Text.Printf (printf)
import Types (Response(None, POST, Websocket))
import Wuss (runSecureClient)

wait :: Int -> IO ()
wait = threadDelay . (* 1000000)

maybeRespond :: Connection -> String -> Int -> Text -> IO ()
maybeRespond connection botId i input =
    (putStrLn . printf "SlackApi> %s\n" . unpack) input >>
    case maybe None (relay botId i) ((extract . unpack) input) of
        Websocket response -> sendTextData connection (pack response)
        POST _ -> void (wait 2) -- awaiting implementation!
        None -> return ()

loop :: Connection -> IO ()
loop connection = getLine >>= (\line -> unless (null line) (loop connection))

app :: String -> ClientApp ()
app botId connection =
    putStrLn "\nSlackApi> ... the bursting-forth of the blossom ..." >>
    (forkIO . forever)
        (receiveData connection >>= maybeRespond connection botId 1) >>
    loop connection >>
    sendClose connection (pack "Bye!") >>
    putStrLn "SlackApi> Tread softly because you tread on my dreams."

run :: String -> String -> String -> IO ()
run host path botId = runSecureClient host 443 path (app botId)
