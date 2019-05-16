{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose)
import System.Environment (getEnv)
import Wuss

main :: IO ()
main = do
    url <- getEnv "URL"
    runSecureClient "wss://cerberus-xxxx.lb.slack-msgs.com" 443 url ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    void . forkIO . forever $ do
        message <- receiveData connection
        print (message :: Text)
    let loop = getLine >>= (\line -> unless (null line) loop)
    loop
    sendClose connection (pack "Bye!")
