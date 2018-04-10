{-# LANGUAGE OverloadedStrings #-}

module LSP(
    ConnectionParams(..),
    startServer
) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Data.Aeson
import Data.Text (pack)
import Network
import System.IO

import qualified Base as Base
import JSONRPC as JSONRPC

data ConnectionParams
    = STDIO
    | TCPServer PortNumber
    | TCPClient String PortNumber
    deriving (Show)

startServer :: ConnectionParams -> IO ()
startServer STDIO = do
    contents <- getContents
    let responseStream = handleClient contents

    hSetBuffering stdout NoBuffering
    hPutStr stdout responseStream

startServer (TCPClient host port) = do
    putStrLn $ "connecting to " ++ (show host) ++ " :" ++ (show port)

    sock <- connectTo host (PortNumber port)

    contents <- hGetContents sock
    let responseStream = handleClient contents

    hSetBuffering sock NoBuffering
    hPutStr sock responseStream

startServer (TCPServer port) = do
    sock <- listenOn $ PortNumber port
    putStrLn $ "listening on " ++ (show port)
    forever $ do
        (client, addr, _) <- accept sock
        putStrLn $ "connection from " ++ (show addr)

        contents <- hGetContents client

        forkIO $ do let responseStream = handleClient contents
                    hPutStr client responseStream

handleClient :: String -> String
handleClient messageStream =
    let (message, e)     = Base.parsePackets messageStream
        requestBatches   = map (JSONRPC.parseMessage . Base.content) message
        requests         = concat $ map (either ((:[]) . Left) (map Right)) requestBatches
        responses        = concat $ map handleRequest requests
        responseMessages = map (Base.LSPPacket [] . JSONRPC.encodeMessage) responses
    in  concat $ map Base.encodeMessage responseMessages

handleRequest :: Either String JSONRPC.ClientMessage -> [JSONRPC.ServerMessage]
handleRequest (Left e)
    = return JSONRPC.ErrorResponse {
          seMsgID = Nothing,
          seError = JSONRPC.Error JSONRPC.parseError e Nothing
      }
handleRequest (Right (Request msgID "initialize" params))
    = [
        ServerNotification {
            snMethod = "window/showMessage",
            snParams =  Just $ object [
                    ("type", Number 3), -- info
                    ("message", "LSP - Hello world!")
                ]
        },
        Response {
            srMsgID  = msgID,
            srResult = object []
        }
    ]
handleRequest (Right (ClientNotification method params))
    = [
        ServerNotification {
            snMethod = "window/showMessage",
            snParams =  Just $ object [
                    ("type", Number 3), -- info
                    ("message", String $ pack $ "Notification from client: " ++ (show method))
                ]
        }
    ]
handleRequest (Right (Request msgID method params))
    = [
        ServerNotification {
            snMethod = "window/showMessage",
            snParams =  Just $ object [
                    ("type", Number 3), -- info
                    ("message", String $ pack $ "Request from client: " ++ (show method))
                ]
        },
        ErrorResponse {
            seMsgID = Just msgID,
            seError = JSONRPC.Error JSONRPC.methodNotFound "method not found" Nothing
        }
    ]