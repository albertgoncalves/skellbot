module Client where

import Control.Concurrent
    ( MVar
    , forkIO
    , newMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
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
import Transport (extract, relay)
import Types (Response(POST, Websocket))
import Wuss (runSecureClient)

wait :: Int -> IO ()
wait n = putStrLn message >> threadDelay (n * 1000000)
  where
    units =
        case n of
            1 -> "second"
            _ -> "seconds"
    message = printf "Client> waiting %d %s\n" n units

withLock :: MVar () -> Int -> IO () -> IO ()
withLock lock n f =
    takeMVar lock >>= (\release -> f >> wait n >> putMVar lock release)

maybeRespond :: Connection -> MVar () -> String -> Int -> Text -> IO ()
maybeRespond connection lock botId i input =
    (putStrLn . printf "SlackApi> %s\n" . unpack) input >>
    case relay botId i =<< extract (unpack input) of
        Just (Websocket response) ->
            withLock lock 1 (sendTextData connection response)
        Just (POST _) ->
            withLock lock 2 $ return () -- awaiting implementation!
        Nothing -> return ()

loop :: Connection -> IO ()
loop connection = getLine >>= (\line -> unless (null line) $ loop connection)

app :: MVar () -> String -> ClientApp ()
app lock botId connection =
    putStrLn "\nClient> ... the bursting-forth of the blossom ...\n" >>
    (forkIO . forever)
        (receiveData connection >>= maybeRespond connection lock botId 1) >>
    loop connection >>
    sendClose connection (pack "Bye!") >>
    putStrLn "Client> Tread softly because you tread on my dreams."

run :: String -> String -> String -> IO ()
run host path botId =
    newMVar () >>= (\lock -> runSecureClient host 443 path $ app lock botId)
