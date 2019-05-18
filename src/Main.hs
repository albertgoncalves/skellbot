{-# OPTIONS_GHC -Wall #-}

module Main where

import Client (run)
import System.Environment (getEnv)

main :: IO ()
main =
    getEnv "RTMHOST" >>=
    (\host -> getEnv "RTMPATH" >>= (\path -> getEnv "BOTID" >>= run host path))
