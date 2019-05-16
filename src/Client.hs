{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose)
import System.Environment (getEnv)
import Wuss

printText :: Text -> IO ()
printText = print

loop :: IO ()
loop = getLine >>= (\line -> unless (null line) loop)

ws :: ClientApp ()
ws connection =
    putStrLn "Connected!"
    >> (void . forkIO . forever) (receiveData connection >>= printText)
    >> loop
    >> sendClose connection (pack "Bye!")

main :: IO ()
main = getEnv "URL" >>= (\url -> runSecureClient hostName 443 url ws)
  where
    hostName = "wss://cerberus-xxxx.lb.slack-msgs.com"
