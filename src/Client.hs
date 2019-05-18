{-# OPTIONS_GHC -Wall #-}

module Main where

import Chat (extract, relay)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack, unpack)
import Network.WebSockets
    ( ClientApp
    , Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import System.Environment (getEnv)
import Text.Printf (printf)
import Wuss (runSecureClient)

echo :: Connection -> String -> Int -> Text -> IO ()
echo connection botId i input =
    maybe
        (return ())
        (sendTextData connection)
        (pack <$> (relay botId i =<< (extract . unpack) input)) >>
    (putStrLn . printf " -> %s\n" . unpack) input

loop :: Connection -> IO ()
loop connection = getLine >>= (\line -> unless (null line) (loop connection))

app :: String -> ClientApp ()
app botId connection =
    putStrLn "Connected!" >>
    (void . forkIO . forever)
        (receiveData connection >>= echo connection botId 1) >>
    loop connection >>
    sendClose connection (pack "Bye!")

main :: IO ()
main =
    getEnv "RTMHOST" >>=
    (\host -> getEnv "RTMPATH" >>= (\path -> getEnv "BOTID" >>= f host path))
  where
    f host path botId = runSecureClient host 443 path (app botId)
