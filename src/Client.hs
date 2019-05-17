{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack, unpack)
import Network.WebSockets (ClientApp, Connection, receiveData, sendClose,
    sendTextData)
import System.Environment (getEnv)
import Text.Printf (printf)
import Wuss (runSecureClient)
import Chat (extract, respond)

chat :: Connection -> Text -> IO ()
chat connection x =
    maybe
        (return ())
        (sendTextData connection)
        (pack . respond <$> (extract . unpack) x)
    >> (putStrLn . printf " -> %s\n" . unpack) x

loop :: Connection -> IO ()
loop connection = getLine >>= f >> loop connection
  where
    f line = unless (null line) $ sendTextData connection (pack line)

ws :: ClientApp ()
ws connection =
    putStrLn "Connected!"
    >> (void . forkIO . forever) (receiveData connection >>= chat connection)
    >> loop connection
    >> sendClose connection (pack "Bye!")

main :: IO ()
main =
    getEnv "RTMHOST"
    >>= (\host -> getEnv "RTMPATH"
    >>= (\path -> runSecureClient host 443 path ws))
