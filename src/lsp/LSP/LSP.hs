{-# LANGUAGE OverloadedStrings #-}

module LSP.LSP (
    handleClient
) where

import Data.Aeson
import Data.Text (pack)

import qualified LSP.Base as Base
import LSP.JSONRPC as JSONRPC

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