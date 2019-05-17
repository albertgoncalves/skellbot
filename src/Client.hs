{-# OPTIONS_GHC -Wall #-}

module Main where

import Chat (extract, returnMessage)
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

echo :: Connection -> Text -> IO ()
echo connection x =
    maybe
        (return ())
        (sendTextData connection)
        (pack . returnMessage <$> (extract . unpack) x) >>
    (putStrLn . printf " -> %s\n" . unpack) x

loop :: Connection -> IO ()
loop connection = getLine >>= f >> loop connection
  where
    f line = unless (null line) $ sendTextData connection (pack line)

app :: ClientApp ()
app connection =
    putStrLn "Connected!" >>
    (void . forkIO . forever) (receiveData connection >>= echo connection) >>
    loop connection >>
    sendClose connection (pack "Bye!")

main :: IO ()
main = getEnv "RTMHOST" >>= (\host -> getEnv "RTMPATH" >>= f host)
  where
    f host path = runSecureClient host 443 path app
