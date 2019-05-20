module Client where

import Chat (extract, relay)
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
import Types (Response(None, POST, Websocket))
import Wuss (runSecureClient)

wait :: Int -> IO ()
wait n = putStrLn message >> threadDelay (n * 1000000)
  where
    message = printf "Client> waiting %d seconds\n" n

withLock :: MVar () -> Int -> IO () -> IO ()
withLock lock n f =
    takeMVar lock >>= (\release -> wait n >> f >> putMVar lock release)

maybeRespond :: Connection -> MVar () -> String -> Int -> Text -> IO ()
maybeRespond connection lock botId i input =
    (putStrLn . printf "SlackApi> %s\n" . unpack) input >>
    case maybe None (relay botId i) ((extract . unpack) input) of
        Websocket response ->
            withLock lock 1 (sendTextData connection (pack response))
        POST _ -> withLock lock 2 (return ()) -- awaiting implementation!
        None -> return ()

loop :: Connection -> IO ()
loop connection = getLine >>= (\line -> unless (null line) $ loop connection)

app :: MVar () -> String -> ClientApp ()
app lock botId connection =
    putStrLn "\nSlackApi> ... the bursting-forth of the blossom ..." >>
    (forkIO . forever)
        (receiveData connection >>= maybeRespond connection lock botId 1) >>
    loop connection >>
    sendClose connection (pack "Bye!") >>
    putStrLn "SlackApi> Tread softly because you tread on my dreams."

run :: String -> String -> String -> IO ()
run host path botId =
    newMVar () >>= (\lock -> runSecureClient host 443 path $ app lock botId)
