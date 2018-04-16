{-# LANGUAGE OverloadedStrings #-}

module LSP.LSP (
    handleClient
) where

import Control.Monad.State
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text (pack)

import qualified LSP.Base as Base
import LSP.JSONRPC as JSONRPC
import LSP.Data.State as State
import LSP.Data.TextDocument

decodeMessageStream :: String -> ([Either String JSONRPC.ClientMessage], Maybe String)
decodeMessageStream input =
    let (message, e)   = Base.parsePackets input
        requestBatches = map (JSONRPC.parseMessage . Base.content) message
        requests       = concat $ map (either ((:[]) .Left) (map Right)) requestBatches
    in  (requests, e)

encodeMessageString :: [JSONRPC.ServerMessage] -> String
encodeMessageString responses =
    let responseMessages = map (Base.LSPPacket [] . JSONRPC.encodeMessage) responses
    in  concat $ map Base.encodeMessage responseMessages

mapWithState :: (a -> State s b) -> [a] -> State s [b]
mapWithState f []     = return []
mapWithState f (a:bs) =
    state (\s -> let (resA,  nextS)  = runState (f a) s
                     (resBs, finalS) = runState (mapWithState f bs) nextS
                 in  (resA:resBs, finalS))

handleClient :: String -> String
handleClient input =
    let (requests, e) = decodeMessageStream input
        responses = concat $ fst $ runState (mapWithState handleRequest requests) State.initial
    in  encodeMessageString responses

handleRequest :: Either String JSONRPC.ClientMessage ->
                 State LSPState [JSONRPC.ServerMessage]
handleRequest (Left e)
    = return [
          JSONRPC.ErrorResponse {
              seMsgID = Nothing,
              seError = JSONRPC.Error JSONRPC.parseError e Nothing
          }
      ]
handleRequest (Right (Request msgID "initialize" params))
    = return [
        ServerNotification {
            snMethod = "window/showMessage",
            snParams = Just $ object [
                    ("type", Number 3), -- info
                    ("message", "LSP - Hello world!")
                ]
        },
        Response {
            srMsgID  = msgID,
            srResult = object [
                ("capabilities", object [
                    ("textDocumentSync", Number 2), -- Incremental
                    ("colorProvider", object [])
                ])
            ]
        }
    ]

handleRequest (Right (ClientNotification "textDocument/didOpen" params))
    = case Just fromJSON <*> params of
          Just (Success document) -> do state <- get
                                        put $ State.addTextDocument document state
                                        return []
          Just (Aeson.Error err) ->
              return [
                      showMessage MessageError "Client notification textDocument/didOpen has bad params"
                  ]
          Nothing ->
              return [
                      showMessage MessageError "Client notification textDocument/didOpen is missing params"
                  ]

handleRequest (Right (ClientNotification "textDocument/didChange" params))
    = case Just fromJSON <*> params of
          Just (Success documentChange) -> do state <- get
                                              let newState = State.changeTextDocument documentChange state
                                              put $ newState
                                              return [
                                                      -- showMessage MessageLog ("State: " ++ show newState)
                                                  ]
          Just (Aeson.Error err) ->
              return [
                      showMessage MessageError "Client notification textDocument/didChange has bad params",
                      showMessage MessageLog ("Err " ++ (show err))
                  ]
          Nothing ->
              return [
                      showMessage MessageError "Client notification textDocument/didChange is missing params"
                  ]

handleRequest (Right (ClientNotification method params))
    = return [
              showMessage MessageInfo $ "Unknown notification from client: " ++ (show method)
          ]

handleRequest (Right (Request msgID method params))
    = return [
        showMessage MessageError $ "Unknown request from client: " ++ (show method),
        ErrorResponse {
            seMsgID = Just msgID,
            seError = JSONRPC.Error JSONRPC.methodNotFound "method not found" Nothing
        }
    ]

data ServerMessageLevel = MessageError |
                          MessageWarning |
                          MessageInfo |
                          MessageLog
                          deriving (Enum)

showMessage :: ServerMessageLevel -> String -> ServerMessage
showMessage level msg =
    JSONRPC.ServerNotification {
        snMethod = "window/showMessage",
        snParams = Just $ object [
                "type"    .= (1 + fromEnum level),
                "message" .= msg
            ]
    }