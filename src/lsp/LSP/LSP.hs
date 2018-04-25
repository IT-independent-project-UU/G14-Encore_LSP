{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSP.LSP (
    handleClient
) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text (pack)
import System.IO

-- LSP
import qualified LSP.Base as Base
import LSP.JSONRPC as JSONRPC
import LSP.Data.State as State
import LSP.Data.TextDocument as TextDocument
import LSP.Data.Hover as Hover
import LSP.Producer (produceAndUpdateState)

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

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

sequenceWithState :: (a -> s -> IO (b, s)) -> [a] -> IO s -> [IO (b, s)]
sequenceWithState f [] _ = []
sequenceWithState f (a:bs) s =
    this : sequenceWithState f bs (this >>= (return . snd))
    where this = s >>= (\s -> f a s)

handleClient :: Handle -> Handle -> IO ()
handleClient input output =
    do inputStream <- hGetContents input
       let (requests, e) = decodeMessageStream inputStream
       let responses = map (fmap fst) $ sequenceStateM handleRequest requests (return State.initial)
       sequence_ $ map (\a -> do
           messages <- a
           hPutStr output $ encodeMessageString messages) responses
       return ()

handleRequest :: Either String JSONRPC.ClientMessage ->
                 StateM LSPState IO [ServerMessage]
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
                          ("colorProvider", object []),
                          ("hoverProvider", Bool True)
                      ])
                  ]
              }
          ]

handleRequest (Right (ClientNotification "textDocument/didOpen" params))
    = case Just fromJSON <*> params of
          Just (Success document) ->
              do modify $ State.addTextDocument document
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
          Just (Success documentChange) ->
              do modify $ State.changeTextDocument documentChange
                 newState <- get
                 return [
                         showMessage MessageLog ("State: " ++ show newState)
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

handleRequest (Right (Request msgID "textDocument/hover" params)) state
    = case Just fromJSON <*> params of
          Just (Success posParams) ->
               return ([
                   Response {
                       srMsgID = msgID,
                           srResult = toJSON $ Hover {
                               Hover.contents = "Test",
                               range = (position posParams, position posParams)
                           }
                       }
                   ], state)
          Just (Aeson.Error err) ->
              return ([
                      showMessage MessageError "Hover - Error ",
                      showMessage MessageError (show err)
                  ], state)
          Nothing ->
              return  ([], state)

handleRequest (Right (ClientNotification method params)) state
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
