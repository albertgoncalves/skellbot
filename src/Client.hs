{-# OPTIONS_GHC -Wall #-}

module Client where

import Chat (extract, relay)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Data.Text (Text, pack, unpack)
import Network.WebSockets
    ( ClientApp
    , Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import Text.Printf (printf)
import Wuss (runSecureClient)

maybeRespond :: Connection -> String -> Int -> Text -> IO ()
maybeRespond connection botId i input =
    maybe
        (return ())
        (sendTextData connection)
        (pack <$> (relay botId i =<< (extract . unpack) input)) >>
    (putStrLn . printf "SlackApi> %s\n" . unpack) input

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
