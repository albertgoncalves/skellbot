{-# OPTIONS_GHC -Wall #-}

module Types where

data Message =
    Message
        { messageId :: String
        , text :: String
        , user :: String
        , channel :: String
        }
    deriving (Eq, Show)
